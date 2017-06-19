-- Main file of the hsevm CLI program

{-# Language CPP #-}
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM
import qualified EVM.TTY as EVM.TTY

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM.Debug
import EVM.Exec
import EVM.Solidity
import EVM.Types
import EVM.UnitTest

import Control.Monad              (unless)
import Control.Monad.State.Strict (execState)
import Control.Lens
import Data.ByteString            (ByteString)
import Data.List                  (intercalate)
import System.Directory           (withCurrentDirectory)
import System.IO                  (hFlush, stdout)

import qualified Data.ByteString.Lazy  as LazyByteString
import qualified Data.Map              as Map
import qualified Options.Generic       as Options

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command
  = Exec
      { code       :: ByteString
      , calldata   :: Maybe ByteString
      , address    :: Maybe Addr
      , caller     :: Maybe Addr
      , origin     :: Maybe Addr
      , coinbase   :: Maybe Addr
      , value      :: Maybe W256
      , number     :: Maybe W256
      , timestamp  :: Maybe W256
      , gaslimit   :: Maybe W256
      , difficulty :: Maybe W256
      , debug      :: Bool
      }
  | DappTest
      { jsonFile :: String
      , dappRoot :: String
      , debug    :: Bool
      }
  | Interactive
      { jsonFile :: String
      , dappRoot :: String
      }
  | VmTest
      { file  :: String
      , test  :: [String]
      , debug :: Bool
      }
  deriving (Show, Options.Generic, Eq)

instance Options.ParseRecord Command where
  parseRecord =
    Options.parseRecordWithModifiers Options.lispCaseModifiers

optsMode :: Command -> Mode
optsMode x = if debug x then Debug else Run

main :: IO ()
main = do
  opts <- Options.getRecord "hsevm -- Ethereum evaluator"
  case opts of
    Exec {} ->
      launchExec opts
    VmTest {} ->
      launchVMTest opts
    DappTest {} ->
      withCurrentDirectory (dappRoot opts) $
        dappTest (optsMode opts) (jsonFile opts)
    Interactive {} ->
      withCurrentDirectory (dappRoot opts) $
        EVM.TTY.main (dappRoot opts) (jsonFile opts)

dappTest :: Mode -> String -> IO ()
dappTest mode solcFile = do
  readSolc solcFile >>=
    \case
      Just (contractMap, cache) -> do
        let unitTests = findUnitTests (Map.elems contractMap)
        mapM_ (runUnitTestContract mode contractMap cache) unitTests
      Nothing ->
        error ("Failed to read Solidity JSON for `" ++ solcFile ++ "'")

launchExec :: Command -> IO ()
launchExec opts =
  let vm = vmFromCommand opts in
    case optsMode opts of
      Run -> print (view EVM.result (execState exec vm))
      _ -> error "not implemented"

vmFromCommand :: Command -> EVM.VM
vmFromCommand opts =
  EVM.makeVm $ EVM.VMOpts
    { EVM.vmoptCode       = hexByteString "--code" (code opts)
    , EVM.vmoptCalldata   = maybe "" (hexByteString "--calldata")
                              (calldata opts)
    , EVM.vmoptValue      = word value 0
    , EVM.vmoptAddress    = addr address 1
    , EVM.vmoptCaller     = addr caller 2
    , EVM.vmoptOrigin     = addr origin 3
    , EVM.vmoptCoinbase   = addr coinbase 0
    , EVM.vmoptNumber     = word number 0
    , EVM.vmoptTimestamp  = word timestamp 0
    , EVM.vmoptGaslimit   = word gaslimit 0
    , EVM.vmoptDifficulty = word difficulty 0
    }
  where
    word f def = maybe def id (f opts)
    addr f def = maybe def id (f opts)

launchVMTest :: Command -> IO ()
launchVMTest opts =
#if MIN_VERSION_aeson(1, 0, 0)
  VMTest.parseSuite <$> LazyByteString.readFile (file opts) >>=
   \case
     Left err -> print err
     Right allTests ->
       let testFilter =
             if null (test opts)
             then id
             else filter (\(x, _) -> elem x (test opts))
       in do
         let tests = testFilter (Map.toList allTests)
         putStrLn $ "Running " ++ show (length tests) ++ " tests"
         results <- mapM (runVMTest (optsMode opts)) tests
         let failed = [name | (name, False) <- zip (map fst tests) results]
         unless (null failed) $ do
           putStrLn ""
           putStrLn $ "Failed: " ++ intercalate ", " failed
#else
  putStrLn "Not supported"
#endif

#if MIN_VERSION_aeson(1, 0, 0)
runVMTest :: Mode -> (String, VMTest.Case) -> IO Bool
runVMTest mode (name, x) = do
  let vm = VMTest.vmForCase x
  case mode of
    Run ->
      do putStr (name ++ ": ")
         hFlush stdout
         let vm' = execState exec vm
         ok <- VMTest.checkExpectation x vm'
         putStrLn (if ok then "OK" else "FAIL")
         return ok

    Debug ->
      do error "not implemented"
#endif
