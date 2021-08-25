{-# LANGUAGE DeriveAnyClass #-}
module EVM.Dev where

import System.Directory

import Prelude hiding (Word)

import EVM.Types
import EVM.Dapp
import EVM.Solidity
import EVM.UnitTest
import EVM.Symbolic

import EVM hiding (path)
import qualified EVM.Fetch
import qualified EVM.TTY
import qualified EVM.Emacs
import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git
import qualified EVM.Stepper
import qualified EVM.VMTest    as VMTest

import Data.SBV hiding (Word)
import qualified Data.Aeson           as JSON
import Options.Generic
import Data.SBV.Trans.Control
import Data.Maybe (fromMaybe)
import Control.Monad.State.Strict (execStateT)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad.State.Class as State
import Control.Monad.State.Strict (runState, liftIO, StateT, get)
import Control.Lens hiding (op, passing)
import Control.Monad.Operational (ProgramViewT(..), ProgramView)
import qualified Control.Monad.Operational as Operational

loadDappInfo :: String -> String -> IO DappInfo
loadDappInfo path file =
  withCurrentDirectory path $
    readSolc file >>=
      \case
        Just (contractMap, sourcecache) ->
          pure (dappInfo "." contractMap sourcecache)
        _ ->
          error "nope, sorry"

ghciTest :: String -> String -> Maybe String -> IO [Bool]
ghciTest root path statePath =
  withCurrentDirectory root $ do
    loadFacts <-
      case statePath of
        Nothing ->
          pure id
        Just repoPath -> do
          facts <- Git.loadFacts (Git.RepoAt repoPath)
          pure (flip Facts.apply facts)
    params <- getParametersFromEnvironmentVariables Nothing
    dapp <- loadDappInfo root path
    let
      opts = UnitTestOptions
        { oracle = EVM.Fetch.zero
        , verbose = Nothing
        , maxIter = Nothing
        , askSmtIters = Nothing
        , smtTimeout = Nothing
        , smtState = Nothing
        , solver = Nothing
        , match = ""
        , covMatch = Nothing
        , fuzzRuns = 100
        , replay = Nothing
        , vmModifier = loadFacts
        , dapp = dapp
        , testParams = params
        , maxDepth = Nothing
        , ffiAllowed = False
        }
    readSolc path >>=
      \case
        Just (contractMap, _) -> do
          let unitTests = findAllUnitTests (Map.elems contractMap)
          results <- runSMT $ query $ concatMapM (runUnitTestContract opts contractMap) unitTests
          let (passing, _) = unzip results
          pure passing

        Nothing ->
          error ("Failed to read Solidity JSON for `" ++ path ++ "'")

runBCTest :: (String, VMTest.Case) -> IO Bool
runBCTest (name, x) = do
  let vm0 = VMTest.vmForCase x
  putStr (name ++ " ")
  out <-
    execStateT (EVM.Stepper.interpret EVM.Fetch.zero EVM.Stepper.execFully) vm0
  ok <- VMTest.checkExpectation False x out
  putStrLn (if ok then "ok" else "")
  return ok

ghciBCTest :: String -> IO ()
ghciBCTest file = do
  let parser = VMTest.parseBCSuite
  parsed <- parser <$> LazyByteString.readFile file
  case parsed of
     Left "No cases to check." -> putStrLn "no-cases ok"
     Left err -> print err
     Right allTests ->
        mapM_ runBCTest (Map.toList allTests)

ghciTty :: String -> String -> Maybe String -> IO ()
ghciTty root path statePath =
  withCurrentDirectory root $ do
    loadFacts <-
      case statePath of
        Nothing ->
          pure id
        Just repoPath -> do
          facts <- Git.loadFacts (Git.RepoAt repoPath)
          pure (flip Facts.apply facts)
    params <- getParametersFromEnvironmentVariables Nothing
    let
      testOpts = UnitTestOptions
        { oracle = EVM.Fetch.zero
        , verbose = Nothing
        , maxIter = Nothing
        , askSmtIters = Nothing
        , smtTimeout = Nothing
        , smtState = Nothing
        , solver = Nothing
        , match = ""
        , covMatch = Nothing
        , fuzzRuns = 100
        , replay = Nothing
        , vmModifier = loadFacts
        , dapp = emptyDapp
        , testParams = params
        , maxDepth = Nothing
        , ffiAllowed = False
        }
    EVM.TTY.main testOpts root path

ghciEmacs :: IO ()
ghciEmacs =
  EVM.Emacs.main

foo :: IO ()
foo = ghciEmacs

data VMTrace =
  VMTrace
  { pc      :: Int
  , op      :: Int
  , stack   :: [Word]
  , memSize :: Int
  , depth   :: Int
  , gas     :: Word
  } deriving (Generic, JSON.ToJSON)

data VMTraceResult =
  VMTraceResult
  { output  :: String
  , gasUsed :: Word
  } deriving (Generic, JSON.ToJSON)

getOp :: VM -> Word8
getOp vm =
  let i  = vm ^. state . EVM.pc
      code' = vm ^. state . code
      xs = case code' of
        ConcreteBuffer xs' -> ConcreteBuffer (BS.drop i xs')
        SymbolicBuffer xs' -> SymbolicBuffer (drop i xs')
  in if len xs == 0 then 0
  else case xs of
       ConcreteBuffer b -> BS.index b 0
       SymbolicBuffer b -> fromSized $ fromMaybe (error "unexpected symbolic code") (unliteral (b !! 0))

vmtrace :: VM -> VMTrace
vmtrace vm =
  let
    -- Convenience function to access parts of the current VM state.
    -- Arcane type signature needed to avoid monomorphism restriction.
    the :: (b -> VM -> Const a VM) -> ((a -> Const a a) -> b) -> a
    the f g = view (f . g) vm
    memsize = the state memorySize
  in VMTrace { pc = the state EVM.pc
             , op = num $ getOp vm
             , gas = the state EVM.gas
             , memSize = memsize
             -- increment to match geth format
             , depth = 1 + length (view frames vm)
             -- reverse to match geth format
             , stack = reverse $ forceLit <$> the state EVM.stack
             }

vmres :: VM -> VMTraceResult
vmres vm =
  let
    gasUsed' = view (tx . txgaslimit) vm - view (state . EVM.gas) vm
    res = case view result vm of
      Just (VMSuccess out) -> forceBuffer out
      Just (VMFailure (Revert out)) -> out
      _ -> mempty
  in VMTraceResult
     -- more oddities to comply with geth
     { output = drop 2 $ show $ ByteStringS res
     , gasUsed = gasUsed'
     }

interpretWithTrace :: EVM.Fetch.Fetcher -> EVM.Stepper.Stepper a -> StateT VM IO a
interpretWithTrace fetcher =
  eval . Operational.view

  where
    eval
      :: ProgramView EVM.Stepper.Action a
      -> StateT VM IO a

    eval (Return x) = do
      vm <- get
      liftIO $ B.putStrLn $ JSON.encode $ vmres vm
      pure x

    eval (action :>>= k) = do
      vm <- get
      case action of
        EVM.Stepper.Run -> do
          -- Have we reached the final result of this action?
          use result >>= \case
            Just _ -> do
              -- Yes, proceed with the next action.
              interpretWithTrace fetcher (k vm)
            Nothing -> do
              liftIO $ B.putStrLn $ JSON.encode $ vmtrace vm

              -- No, keep performing the current action
              State.state (runState exec1)
              interpretWithTrace fetcher (EVM.Stepper.run >>= k)

        -- Stepper wants to keep executing?
        EVM.Stepper.Exec -> do
          -- Have we reached the final result of this action?
          use result >>= \case
            Just r -> do
              -- Yes, proceed with the next action.
              interpretWithTrace fetcher (k r)
            Nothing -> do
              liftIO $ B.putStrLn $ JSON.encode $ vmtrace vm

              -- No, keep performing the current action
              State.state (runState exec1)
              interpretWithTrace fetcher (EVM.Stepper.exec >>= k)
        EVM.Stepper.Wait q ->
          do m <- liftIO (fetcher q)
             State.state (runState m) >> interpretWithTrace fetcher (k ())
        EVM.Stepper.Ask _ ->
          error "cannot make choices with this interpretWithTraceer"
        EVM.Stepper.IOAct m ->
          m >>= interpretWithTrace fetcher . k
        EVM.Stepper.EVM m -> do
          r <- State.state (runState m)
          interpretWithTrace fetcher (k r)
