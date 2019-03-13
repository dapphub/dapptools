-- Main file of the hevm CLI program

{-# Language BangPatterns #-}
{-# Language CPP #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language NumDecimals #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

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

import qualified EVM.UnitTest as EVM.UnitTest

import qualified Paths_hevm as Paths

import Control.Concurrent.Async   (async, waitCatch)
import Control.Exception          (evaluate)
import Control.Lens
import Control.Monad              (void, when, forM_)
import Control.Monad.State.Strict (execState)
import Data.ByteString            (ByteString)
import Data.List                  (intercalate, isSuffixOf)
import Data.Text                  (Text, unpack, pack)
import Data.Maybe                 (fromMaybe)
import System.Directory           (withCurrentDirectory, listDirectory)
import System.Exit                (die, exitFailure)
import System.IO                  (hFlush, stdout)
import System.Process             (callProcess)
import System.Timeout             (timeout)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Base16 as BS16
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Map               as Map
import qualified Options.Generic        as Options

import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git

import qualified Text.Regex.TDFA as Regex
import qualified Data.Sequence as Seq

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
      , gasprice   :: Maybe W256
      , difficulty :: Maybe W256
      , debug      :: Bool
      , state      :: Maybe String
      }
  | DappTest
      { jsonFile           :: Maybe String
      , dappRoot           :: Maybe String
      , debug              :: Bool
      , rpc                :: Maybe URL
      , verbose            :: Bool
      , coverage           :: Bool
      , state              :: Maybe String
      , match              :: Maybe String
      }
  | Interactive
      { jsonFile           :: Maybe String
      , dappRoot           :: Maybe String
      , rpc                :: Maybe URL
      , state              :: Maybe String
      }
  | VmTest
      { file  :: String
      , test  :: [String]
      , debug :: Bool
      , diff  :: Bool
      }
  | VmTestReport
      { tests :: String
      }
  | BcTest
      { file  :: String
      , test  :: [String]
      , debug :: Bool
      , diff  :: Bool
      }
  | Flatten
    { sourceFile :: String
    , jsonFile   :: Maybe String
    , dappRoot   :: Maybe String
    }
  | Emacs
  deriving (Show, Options.Generic, Eq)

type URL = Text

instance Options.ParseRecord Command where
  parseRecord =
    Options.parseRecordWithModifiers Options.lispCaseModifiers

optsMode :: Command -> Mode
optsMode x = if debug x then Debug else Run

unitTestOptions :: Command -> IO UnitTestOptions
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
  cmd <- Options.getRecord "hevm -- Ethereum evaluator"
  let
    root = fromMaybe "." (dappRoot cmd)
  case cmd of
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

launchExec :: Command -> IO ()
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

vmFromCommand :: Command -> EVM.VM
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
      , EVM.vmoptDifficulty    = word difficulty 0
      , EVM.vmoptSchedule      = FeeSchedule.metropolis
      , EVM.vmoptCreate        = False
      }
    word f def = maybe def id (f cmd)
    addr f def = maybe def id (f cmd)

launchTest :: ExecMode -> Command ->  IO ()
launchTest execmode cmd = do
#if MIN_VERSION_aeson(1, 0, 0)
  let parser = case execmode of
        ExecuteAsVMTest -> VMTest.parseSuite
        ExecuteAsBlockchainTest -> VMTest.parseBCSuite
  parsed <- parser <$> LazyByteString.readFile (file cmd)
  case parsed of
     Left err -> print err
     Right allTests ->
       let testFilter =
             if null (test cmd)
             then id
             else filter (\(x, _) -> elem x (test cmd))
       in
         mapM_ (runVMTest (diff cmd) execmode (optsMode cmd)) $
           testFilter (Map.toList allTests)
#else
  putStrLn "Not supported"
#endif

#if MIN_VERSION_aeson(1, 0, 0)
runVMTest :: Bool -> ExecMode -> Mode -> (String, VMTest.Case) -> IO Bool
runVMTest diffmode execmode mode (name, x) = do
  let
    vm0 = VMTest.vmForCase execmode x
    m = case execmode of
      -- whether or not to "finalize the tx"
      ExecuteAsVMTest -> void EVM.Stepper.execFully >> EVM.Stepper.evm (EVM.finalize False)
      ExecuteAsBlockchainTest -> void EVM.Stepper.execFully >> EVM.Stepper.evm (EVM.finalize True)
  putStr (name ++ " ")
  hFlush stdout
  result <- do
    action <- async $
      case mode of
        Run ->
          timeout (1e6) . evaluate $ do
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
