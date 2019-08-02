-- Main file of the hevm CLI program

{-# Language BangPatterns #-}
{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language NumDecimals #-}
{-# Language OverloadedStrings #-}
{-# Language StandaloneDeriving #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}

import EVM.Concrete (w256)

import qualified EVM as EVM
import qualified EVM.FeeSchedule as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Flatten
import qualified EVM.Stepper
import qualified EVM.TTY as EVM.TTY
import qualified EVM.Emacs as EVM.Emacs

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM (ExecMode(..))
import EVM.Debug
import EVM.Exec
import EVM.Solidity
import EVM.Types hiding (word)
import EVM.UnitTest (UnitTestOptions, coverageReport, coverageForUnitTestContract)
import EVM.UnitTest (runUnitTestContract)
import EVM.UnitTest (getParametersFromEnvironmentVariables, testNumber)
import EVM.Dapp (findUnitTests, dappInfo)

import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git
import qualified EVM.UnitTest  as EVM.UnitTest

import Control.Concurrent.Async   (async, waitCatch)
import Control.Exception          (evaluate)
import Control.Lens
import Control.Monad              (void, when, forM_)
import Control.Monad.State.Strict (execState)
import Data.ByteString            (ByteString)
import Data.List                  (intercalate, isSuffixOf)
import Data.Text                  (Text, unpack, pack)
import Data.Maybe                 (fromMaybe)
import Data.Version               (showVersion)
import System.Directory           (withCurrentDirectory, listDirectory)
import System.Exit                (die, exitFailure)
import System.IO                  (hFlush, stdout)
import System.Process             (callProcess)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import qualified System.Timeout         as Timeout

import qualified Paths_hevm      as Paths
import qualified Text.Regex.TDFA as Regex

import Options.Generic as Options

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command w
  = Exec -- Execute a given program with specified env & calldata
      { code        :: w ::: ByteString       <?> "Program bytecode"
      , calldata    :: w ::: Maybe ByteString <?> "Tx: calldata"
      , address     :: w ::: Maybe Addr       <?> "Tx: address"
      , caller      :: w ::: Maybe Addr       <?> "Tx: caller"
      , origin      :: w ::: Maybe Addr       <?> "Tx: origin"
      , coinbase    :: w ::: Maybe Addr       <?> "Block: coinbase"
      , value       :: w ::: Maybe W256       <?> "Tx: Eth amount"
      , gas         :: w ::: Maybe W256       <?> "Tx: gas amount"
      , number      :: w ::: Maybe W256       <?> "Block: number"
      , timestamp   :: w ::: Maybe W256       <?> "Block: timestamp"
      , gaslimit    :: w ::: Maybe W256       <?> "Tx: gas limit"
      , gasprice    :: w ::: Maybe W256       <?> "Tx: gas price"
      , maxcodesize :: w ::: Maybe W256       <?> "Block: max code size"
      , difficulty  :: w ::: Maybe W256       <?> "Block: difficulty"
      , debug       :: w ::: Bool             <?> "Run interactively"
      , state       :: w ::: Maybe String     <?> "Path to state repository"
      }
  | DappTest -- Run DSTest unit tests
      { jsonFile    :: w ::: Maybe String <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot    :: w ::: Maybe String <?> "Path to dapp project root directory (default: . )"
      , debug       :: w ::: Bool         <?> "Run interactively"
      , rpc         :: w ::: Maybe URL    <?> "Fetch state from a remote node"
      , verbose     :: w ::: Maybe Int    <?> "Append call trace: {1} failures {2} all"
      , coverage    :: w ::: Bool         <?> "Coverage analysis"
      , state       :: w ::: Maybe String <?> "Path to state repository"
      , match       :: w ::: Maybe String <?> "Test case filter - only run methods matching regex"
      }
  | Interactive -- Browse & run unit tests interactively
      { jsonFile :: w ::: Maybe String <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot :: w ::: Maybe String <?> "Path to dapp project root directory (default: . )"
      , rpc      :: w ::: Maybe URL    <?> "Fetch state from a remote node"
      , state    :: w ::: Maybe String <?> "Path to state repository"
      }
  | VmTest -- Run an Ethereum VMTest
      { file    :: w ::: String    <?> "Path to .json test file"
      , test    :: w ::: [String]  <?> "Test case filter - only run specified test method(s)"
      , debug   :: w ::: Bool      <?> "Run interactively"
      , diff    :: w ::: Bool      <?> "Print expected vs. actual state on failure"
      , timeout :: w ::: Maybe Int <?> "Execution timeout (default: 10 sec.)"
      }
  | VmTestReport -- Run all Ethereum VMTests
      { tests :: w ::: String <?> "Path to Ethereum VMTests directory"
      }
  | BcTest -- Run Ethereum Blockhain/GeneralState test
      { file    :: w ::: String    <?> "Path to .json test file"
      , test    :: w ::: [String]  <?> "Test case filter - only run specified test method(s)"
      , debug   :: w ::: Bool      <?> "Run interactively"
      , diff    :: w ::: Bool      <?> "Print expected vs. actual state on failure"
      , timeout :: w ::: Maybe Int <?> "Execution timeout (default: 10 sec.)"
      }
  | Flatten -- Concat all dependencies for a given source file
    { sourceFile :: w ::: String       <?> "Path to solidity source file e.g. src/contract.sol"
    , jsonFile   :: w ::: Maybe String <?> "Filename or path to dapp build output (default: out/*.solc.json)"
    , dappRoot   :: w ::: Maybe String <?> "Path to dapp project root directory (default: . )"
    }
  | Emacs
  | Version
  deriving (Options.Generic)

type URL = Text

instance Options.ParseRecord (Command Options.Wrapped) where
  parseRecord =
    Options.parseRecordWithModifiers Options.lispCaseModifiers

optsMode :: Command Options.Unwrapped -> Mode
optsMode x = if debug x then Debug else Run

unitTestOptions :: Command Options.Unwrapped -> IO UnitTestOptions
unitTestOptions cmd = do
  vmModifier <-
    case state cmd of
      Nothing ->
        pure id
      Just repoPath -> do
        facts <- Git.loadFacts (Git.RepoAt repoPath)
        pure (flip Facts.apply facts)

  params <- getParametersFromEnvironmentVariables

  pure EVM.UnitTest.UnitTestOptions
    { EVM.UnitTest.oracle =
        case rpc cmd of
         Just url -> EVM.Fetch.http (EVM.Fetch.BlockNumber (testNumber params)) url
         Nothing  -> EVM.Fetch.zero
    , EVM.UnitTest.verbose = verbose cmd
    , EVM.UnitTest.match   = pack $ fromMaybe "^test" (match cmd)
    , EVM.UnitTest.vmModifier = vmModifier
    , EVM.UnitTest.testParams = params
    }

main :: IO ()
main = do
  cmd <- Options.unwrapRecord "hevm -- Ethereum evaluator"
  let
    root = fromMaybe "." (dappRoot cmd)
  case cmd of
    Version {} -> putStrLn (showVersion Paths.version)
    Exec {} ->
      launchExec cmd
    VmTest {} ->
      launchTest ExecuteAsVMTest cmd
    BcTest {} ->
      launchTest ExecuteAsBlockchainTest cmd
    DappTest {} ->
      withCurrentDirectory root $ do
        testFile <- findJsonFile (jsonFile cmd)
        testOpts <- unitTestOptions cmd
        case coverage cmd of
          False ->
            dappTest testOpts (optsMode cmd) testFile
          True ->
            dappCoverage testOpts (optsMode cmd) testFile
    Interactive {} ->
      withCurrentDirectory root $ do
        testFile <- findJsonFile (jsonFile cmd)
        testOpts <- unitTestOptions cmd
        EVM.TTY.main testOpts root testFile
    VmTestReport {} ->
      withCurrentDirectory (tests cmd) $ do
        dataDir <- Paths.getDataDir
        callProcess "bash" [dataDir ++ "/run-consensus-tests", "."]
    Flatten {} ->
      withCurrentDirectory root $ do
        theJson <- findJsonFile (jsonFile cmd)
        readSolc theJson >>=
          \case
            Just (contractMap, cache) -> do
              let dapp = dappInfo "." contractMap cache
              EVM.Flatten.flatten dapp (pack (sourceFile cmd))
            Nothing ->
              error ("Failed to read Solidity JSON for `" ++ theJson ++ "'")
    Emacs ->
      EVM.Emacs.main

findJsonFile :: Maybe String -> IO String
findJsonFile (Just s) = pure s
findJsonFile Nothing = do
  outFiles <- listDirectory "out"
  case filter (isSuffixOf ".sol.json") outFiles of
    [x] -> pure ("out/" ++ x)
    [] ->
      error $ concat
        [ "No `*.sol.json' file found in `./out'.\n"
        , "Maybe you need to run `dapp build'.\n"
        , "You can specify a file with `--json-file'."
        ]
    xs ->
      error $ concat
        [ "Multiple `*.sol.json' files found in `./out'.\n"
        , "Specify one using `--json-file'.\n"
        , "Files found: "
        , intercalate ", " xs
        ]

dappTest :: UnitTestOptions -> Mode -> String -> IO ()
dappTest opts _ solcFile = do
  readSolc solcFile >>=
    \case
      Just (contractMap, cache) -> do
        let matcher = regexMatches (EVM.UnitTest.match opts)
        let unitTests = (findUnitTests matcher) (Map.elems contractMap)
        results <- mapM (runUnitTestContract opts contractMap cache) unitTests
        when (any (== False) results) exitFailure
      Nothing ->
        error ("Failed to read Solidity JSON for `" ++ solcFile ++ "'")

regexMatches :: Text -> Text -> Bool
regexMatches regexSource =
  let
    compOpts =
      Regex.defaultCompOpt { Regex.lastStarGreedy = True }
    execOpts =
      Regex.defaultExecOpt { Regex.captureGroups = False }
    regex = Regex.makeRegexOpts compOpts execOpts (unpack regexSource)
  in
    Regex.matchTest regex . Seq.fromList . unpack

dappCoverage :: UnitTestOptions -> Mode -> String -> IO ()
dappCoverage opts _ solcFile = do
  readSolc solcFile >>=
    \case
      Just (contractMap, cache) -> do
        let matcher = regexMatches (EVM.UnitTest.match opts)
        let unitTests = (findUnitTests matcher) (Map.elems contractMap)
        covs <- mconcat <$> mapM (coverageForUnitTestContract opts contractMap cache) unitTests

        let
          dapp = dappInfo "." contractMap cache
          f (k, vs) = do
            putStr "***** hevm coverage for "
            putStrLn (unpack k)
            putStrLn ""
            forM_ vs $ \(n, bs) -> do
              case ByteString.find (\x -> x /= 0x9 && x /= 0x20 && x /= 0x7d) bs of
                Nothing -> putStr "..... "
                Just _ ->
                  case n of
                    -1 -> putStr ";;;;; "
                    0  -> putStr "##### "
                    _  -> putStr "      "
              Char8.putStrLn bs
            putStrLn ""

        mapM_ f (Map.toList (coverageReport dapp covs))
      Nothing ->
        error ("Failed to read Solidity JSON for `" ++ solcFile ++ "'")

launchExec :: Command Options.Unwrapped -> IO ()
launchExec cmd = do
  let vm = vmFromCommand cmd
  vm1 <- case state cmd of
    Nothing -> pure vm
    Just path ->
      -- Note: this will load the code, so if you've specified a state
      -- repository, then you effectively can't change `--code' after
      -- the first run.
      Facts.apply vm <$> Git.loadFacts (Git.RepoAt path)

  case optsMode cmd of
    Run ->
      let vm' = execState exec vm1
      in case view EVM.result vm' of
        Nothing ->
          error "internal error; no EVM result"
        Just (EVM.VMFailure e) -> do
          die (show e)
        Just (EVM.VMSuccess x) -> do
          let hex = BS16.encode x
          if ByteString.null hex then pure ()
            else do
              ByteString.putStr hex
              putStrLn ""
          case state cmd of
            Nothing -> pure ()
            Just path ->
              Git.saveFacts (Git.RepoAt path) (Facts.vmFacts vm')
    Debug ->
      void (EVM.TTY.runFromVM vm1)

vmFromCommand :: Command Options.Unwrapped -> EVM.VM
vmFromCommand cmd =
  vm1 & EVM.env . EVM.contracts . ix address' . EVM.balance +~ (w256 value')
  where
    value'   = word value 0
    address' = addr address 1
    vm1 = EVM.makeVm $ EVM.VMOpts
      { EVM.vmoptCode          = hexByteString "--code" (code cmd)
      , EVM.vmoptCalldata      = maybe "" (hexByteString "--calldata")
                                   (calldata cmd)
      , EVM.vmoptValue         = value'
      , EVM.vmoptAddress       = address'
      , EVM.vmoptCaller        = addr caller 2
      , EVM.vmoptOrigin        = addr origin 3
      , EVM.vmoptGas           = word gas 0
      , EVM.vmoptGaslimit      = word gas 0
      , EVM.vmoptCoinbase      = addr coinbase 0
      , EVM.vmoptNumber        = word number 0
      , EVM.vmoptTimestamp     = word timestamp 0
      , EVM.vmoptBlockGaslimit = word gaslimit 0
      , EVM.vmoptGasprice      = word gasprice 0
      , EVM.vmoptMaxCodeSize   = word maxcodesize 0xffffffff
      , EVM.vmoptDifficulty    = word difficulty 0
      , EVM.vmoptSchedule      = FeeSchedule.metropolis
      , EVM.vmoptCreate        = False
      }
    word f def = maybe def id (f cmd)
    addr f def = maybe def id (f cmd)

launchTest :: ExecMode -> Command Options.Unwrapped ->  IO ()
launchTest execmode cmd = do
#if MIN_VERSION_aeson(1, 0, 0)
  let parser = case execmode of
        ExecuteAsVMTest -> VMTest.parseSuite
        ExecuteAsBlockchainTest -> VMTest.parseBCSuite
        ExecuteNormally -> error "cannot launchTest normally"
  parsed <- parser <$> LazyByteString.readFile (file cmd)
  case parsed of
     Left "No cases to check." -> putStrLn "no-cases ok"
     Left err -> print err
     Right allTests ->
       let testFilter =
             if null (test cmd)
             then id
             else filter (\(x, _) -> elem x (test cmd))
       in
         mapM_ (runVMTest (diff cmd) execmode (optsMode cmd) (timeout cmd)) $
           testFilter (Map.toList allTests)
#else
  putStrLn "Not supported"
#endif

#if MIN_VERSION_aeson(1, 0, 0)
runVMTest :: Bool -> ExecMode -> Mode -> Maybe Int -> (String, VMTest.Case) -> IO Bool
runVMTest diffmode execmode mode timelimit (name, x) = do
  let
    vm0 = VMTest.vmForCase execmode x
    m = case execmode of
      -- whether or not to "finalize the tx"
      ExecuteAsVMTest -> void EVM.Stepper.execFully >> EVM.Stepper.evm (EVM.finalize False)
      ExecuteAsBlockchainTest -> void EVM.Stepper.execFully >> EVM.Stepper.evm (EVM.finalize True)
      ExecuteNormally -> error "cannot launchTest normally"
  putStr (name ++ " ")
  hFlush stdout
  result <- do
    action <- async $
      case mode of
        Run ->
          Timeout.timeout (1e6 * (fromMaybe 10 timelimit)) . evaluate $ do
            execState (VMTest.interpret m) vm0
        Debug ->
          Just <$> EVM.TTY.runFromVM vm0
    waitCatch action
  case result of
    Right (Just vm1) -> do
      ok <- VMTest.checkExpectation diffmode execmode x vm1
      putStrLn (if ok then "ok" else "")
      return ok
    Right Nothing -> do
      putStrLn "timeout"
      return False
    Left e -> do
      putStrLn $ "error: " ++ if diffmode
        then show e
        else (head . lines . show) e
      return False

#endif
