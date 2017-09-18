-- Main file of the hsevm CLI program

{-# Language CPP #-}
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM
import qualified EVM.Concrete as EVM
import qualified EVM.TTY as EVM.TTY
import EVM.Machine (w256)

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM.Debug
import EVM.Exec
import EVM.Solidity
import EVM.Types hiding (word)
import EVM.UnitTest

import Control.Lens
import Control.Monad              (unless)
import Control.Monad.State.Strict (execState)
import Data.ByteString            (ByteString)
import Data.List                  (intercalate, isSuffixOf)
import Data.Maybe                 (fromMaybe)
import System.Directory           (withCurrentDirectory, listDirectory)
import System.Exit                (die)
import System.IO                  (hFlush, stdout)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Map               as Map
import qualified Options.Generic        as Options

import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git

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
      , gas        :: Maybe W256
      , number     :: Maybe W256
      , timestamp  :: Maybe W256
      , gaslimit   :: Maybe W256
      , difficulty :: Maybe W256
      , debug      :: Bool
      , state      :: Maybe String
      }
  | DappTest
      { jsonFile :: Maybe String
      , dappRoot :: Maybe String
      , debug    :: Bool
      }
  | Interactive
      { jsonFile :: Maybe String
      , dappRoot :: Maybe String
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
  let root = fromMaybe "." (dappRoot opts)
  case opts of
    Exec {} ->
      launchExec opts
    VmTest {} ->
      launchVMTest opts
    DappTest {} ->
      withCurrentDirectory root $ do
        testFile <- findTestFile (jsonFile opts)
        dappTest (optsMode opts) testFile
    Interactive {} ->
      withCurrentDirectory root $ do
        testFile <- findTestFile (jsonFile opts)
        EVM.TTY.main root testFile

findTestFile :: Maybe String -> IO String
findTestFile (Just s) = pure s
findTestFile Nothing = do
  outFiles <- listDirectory "out"
  case filter (isSuffixOf ".t.sol.json") outFiles of
    [x] -> pure ("out/" ++ x)
    [] ->
      error $ concat
        [ "No `*.t.sol.json' file found in `./out'.\n"
        , "Maybe you need to run `dapp build'.\n"
        , "You can specify a file with `--json-file'."
        ]
    xs ->
      error $ concat
        [ "Multiple `*.t.sol.json' files found in `./out'.\n"
        , "Specify one using `--json-file'.\n"
        , "Files found: "
        , intercalate ", " xs
        ]

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
launchExec opts = do
  let vm = vmFromCommand opts
  vm1 <- case state opts of
    Nothing -> pure vm
    Just path ->
      -- Note: this will load the code, so if you've specified a state
      -- repository, then you effectively can't change `--code' after
      -- the first run.
      Facts.apply vm <$> Git.loadFacts (Git.RepoAt path)

  case optsMode opts of
    Run ->
      let vm' = execState exec vm1
      in case view EVM.result vm' of
        Nothing ->
          error "internal error; no EVM result"
        Just (EVM.VMFailure e) -> do
          die (show e)
        Just (EVM.VMSuccess (EVM.B x)) -> do
          let hex = BS16.encode x
          if ByteString.null hex then pure ()
            else do
              ByteString.putStr hex
              putStrLn ""
          case state opts of
            Nothing -> pure ()
            Just path ->
              Git.saveFacts (Git.RepoAt path) (Facts.vmFacts vm')
    Debug ->
      EVM.TTY.runFromVM vm

vmFromCommand :: Command -> EVM.VM EVM.Concrete
vmFromCommand opts =
  vm1 & EVM.env . EVM.contracts . ix address' . EVM.balance +~ (w256 value')
  where
    value'   = word value 0
    address' = addr address 1
    vm1 = EVM.makeVm $ EVM.VMOpts
      { EVM.vmoptCode       = hexByteString "--code" (code opts)
      , EVM.vmoptCalldata   = maybe "" (hexByteString "--calldata")
                                (calldata opts)
      , EVM.vmoptValue      = value'
      , EVM.vmoptAddress    = address'
      , EVM.vmoptCaller     = addr caller 2
      , EVM.vmoptOrigin     = addr origin 3
      , EVM.vmoptGas        = word gas 0
      , EVM.vmoptCoinbase   = addr coinbase 0
      , EVM.vmoptNumber     = word number 0
      , EVM.vmoptTimestamp  = word timestamp 0
      , EVM.vmoptGaslimit   = word gaslimit 0
      , EVM.vmoptDifficulty = word difficulty 0
      }
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
