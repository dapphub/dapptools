{-# LANGUAGE DeriveAnyClass #-}
module EVM.Dev where

import System.Directory

import Prelude hiding (Word)

import EVM.Types
import EVM.Dapp
import EVM.Solidity
import EVM.UnitTest
import EVM.Concrete
import EVM.Symbolic

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
import Control.Monad.State.Strict (execStateT)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Control.Monad.State.Class as State
import Control.Monad.State.Strict (runState, liftIO, StateT, get)
import Control.Lens
import Control.Monad.Operational (Program, singleton, ProgramViewT(..), ProgramView)
import qualified Control.Monad.Operational as Operational
import EVM
import qualified EVM.Exec
import qualified EVM.Fetch as Fetch
import Data.Text (Text, isPrefixOf)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (pure [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; pure $ x++xs

loadDappInfo :: String -> String -> IO DappInfo
loadDappInfo path file =
  withCurrentDirectory path $
    readSolc file >>=
      \case
        Just (contractMap, cache) ->
          pure (dappInfo "." contractMap cache)
        _ ->
          error "nope, sorry"

ghciTest :: String -> String -> Maybe String -> IO [Bool]
ghciTest root path state =
  withCurrentDirectory root $ do
    loadFacts <-
      case state of
        Nothing ->
          pure id
        Just repoPath -> do
          facts <- Git.loadFacts (Git.RepoAt repoPath)
          pure (flip Facts.apply facts)
    params <- getParametersFromEnvironmentVariables Nothing
    let
      opts = UnitTestOptions
        { oracle = EVM.Fetch.zero
        , verbose = Nothing
        , maxIter = Nothing
        , match = ""
        , fuzzRuns = 100
        , replay = Nothing
        , vmModifier = loadFacts
        , testParams = params
        }
    readSolc path >>=
      \case
        Just (contractMap, _) -> do
          let unitTests = findUnitTests
                (\a -> "test" `isPrefixOf` a || "prove" `isPrefixOf` a) (Map.elems contractMap)
          results <- runSMT $ query $ concatMapM (runUnitTestContract opts contractMap) unitTests
          let (passing, _) = unzip results
          pure passing

        Nothing ->
          error ("Failed to read Solidity JSON for `" ++ path ++ "'")

runBCTest :: (String, VMTest.Case) -> IO Bool
runBCTest (name, x) = do
  let vm0 = VMTest.vmForCase x
  putStr (name ++ " ")
  result <-
      execStateT (EVM.Stepper.interpret EVM.Fetch.zero EVM.Stepper.execFully) vm0
  ok <- VMTest.checkExpectation False x result
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
ghciTty root path state =
  withCurrentDirectory root $ do
    loadFacts <-
      case state of
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
        , match = ""
        , fuzzRuns = 100
        , replay = Nothing
        , vmModifier = loadFacts
        , testParams = params
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
  , memory  :: ByteStringS
  , memSize :: Int
  , depth   :: Int
  , gas     :: Word
  } deriving (Generic, JSON.ToJSON)

data VMTraceResult =
  VMTraceResult
  { output  :: String
  , gasUsed :: Word
  } deriving (Generic, JSON.ToJSON)

vmtrace :: VM -> VMTrace
vmtrace vm =
  let
    -- Convenience function to access parts of the current VM state.
    -- Arcane type signature needed to avoid monomorphism restriction.
    the :: (b -> VM -> Const a VM) -> ((a -> Const a a) -> b) -> a
    the f g = view (f . g) vm
    op' = if BS.length (the state code) <= the state EVM.pc
          then 0
          else fromIntegral $ BS.index (the state code) (the state EVM.pc)
    memsize = the state memorySize
  in VMTrace { pc = the state EVM.pc
             , op = op'
             , gas = the state EVM.gas
             -- pad to match geth format
             , memory = ByteStringS . padRight memsize $ forceBuffer $ the state EVM.memory
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
      _ -> mempty
  in VMTraceResult
     -- more oddities to comply with geth
     { output = if BS.null res then "" else show $ ByteStringS res
     , gasUsed = gasUsed'
     }

interpretWithTrace :: Fetch.Fetcher -> EVM.Stepper.Stepper a -> StateT VM IO a
interpretWithTrace fetcher =
  eval . Operational.view

  where
    eval
      :: ProgramView EVM.Stepper.Action a
      -> StateT VM IO a

    eval (Return x) =
      pure x

    eval (action :>>= k) = do
      vm <- get
      case action of
        EVM.Stepper.Run -> do
          -- Have we reached the final result of this action?
          use result >>= \case
            Just _ -> do
              liftIO $ B.putStrLn $ JSON.encode $ vmres vm
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
              liftIO $ B.putStrLn $ JSON.encode $ vmres vm
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
        EVM.Stepper.EVM m -> do
          r <- State.state (runState m)
          interpretWithTrace fetcher (k r)
