-- Main file of the hevm CLI program

{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language NumDecimals #-}
{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}

import qualified EVM
import qualified EVM.FeeSchedule as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Flatten
import qualified EVM.Stepper
import qualified EVM.TTY
import qualified EVM.Emacs

import Text.ParserCombinators.ReadP

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM (ExecMode(..))
import EVM.ABI (sig)
import EVM.Concrete (createAddress)
import EVM.Symbolic
import EVM.Debug
import EVM.Exec
import EVM.ABI
import EVM.Solidity
import EVM.Types hiding (word)
import EVM.UnitTest (UnitTestOptions, coverageReport, coverageForUnitTestContract)
import EVM.UnitTest (runUnitTestContract)
import EVM.UnitTest (getParametersFromEnvironmentVariables, testNumber)
import EVM.Dapp (findUnitTests, dappInfo)
import EVM.RLP (rlpdecode)
import qualified EVM.Patricia as Patricia
import Data.Map (Map)
import Numeric (readHex, showHex)

import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git
import qualified EVM.UnitTest

--import Control.Concurrent.Async   (async, waitCatch)
import Control.Exception          (evaluate)
import qualified Control.Monad.Operational as Operational
import qualified Control.Monad.State.Class as State
import Control.Lens
import Control.Monad              (void, when, forM_)
import Control.Monad.State.Strict (execState, runState, StateT, liftIO, execStateT)
import Data.ByteString            (ByteString)
import Data.List                  (intercalate, isSuffixOf)
import Data.Text                  (Text, unpack, pack, splitOn)
import Data.Text.Encoding         (encodeUtf8)
import Data.Maybe                 (fromMaybe, fromJust)
import Data.Version               (showVersion)
import Data.SBV hiding (Word, verbose)
import Data.SBV.Control hiding (Word, verbose, Version, timeout, create)
import System.Directory           (withCurrentDirectory, listDirectory)
import System.Exit                (die, exitFailure)
import System.IO                  (hFlush, stdout)
import System.Process             (callProcess)
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson.Lens hiding (values)
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy  as Lazy

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Char8  as Char8
import qualified Data.ByteString.Lazy   as LazyByteString
import qualified Data.Map               as Map
import qualified Data.Sequence          as Seq
import qualified System.Timeout         as Timeout

import qualified Paths_hevm      as Paths
--import qualified Text.Regex.TDFA as Regex

import Options.Generic as Options

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command w
  = Assert -- Execute a given program with specified env & calldata
      { code        :: w ::: Maybe ByteString   <?> "Program bytecode"
      , funcSig     :: w ::: Text               <?> "Function signature"
      , debug       :: w ::: Bool               <?> "Run interactively"
      }
  | Exec -- Execute a given program with specified env & calldata
      { code        :: w ::: Maybe ByteString <?> "Program bytecode"
      , calldata    :: w ::: Maybe ByteString <?> "Tx: calldata"
      , address     :: w ::: Maybe Addr       <?> "Tx: address"
      , caller      :: w ::: Maybe Addr       <?> "Tx: caller"
      , origin      :: w ::: Maybe Addr       <?> "Tx: origin"
      , coinbase    :: w ::: Maybe Addr       <?> "Block: coinbase"
      , value       :: w ::: Maybe W256       <?> "Tx: Eth amount"
      , nonce       :: w ::: Maybe W256       <?> "Nonce of origin"
      , gas         :: w ::: Maybe W256       <?> "Tx: gas amount"
      , number      :: w ::: Maybe W256       <?> "Block: number"
      , timestamp   :: w ::: Maybe W256       <?> "Block: timestamp"
      , gaslimit    :: w ::: Maybe W256       <?> "Tx: gas limit"
      , gasprice    :: w ::: Maybe W256       <?> "Tx: gas price"
      , create      :: w ::: Bool             <?> "Tx: creation"
      , maxcodesize :: w ::: Maybe W256       <?> "Block: max code size"
      , difficulty  :: w ::: Maybe W256       <?> "Block: difficulty"
      , debug       :: w ::: Bool             <?> "Run interactively"
      , state       :: w ::: Maybe String     <?> "Path to state repository"
      , rpc         :: w ::: Maybe URL        <?> "Fetch state from a remote node"
      , block       :: w ::: Maybe W256       <?> "Block state is be fetched from"
      }
  | DappTest -- Run DSTest unit tests
      { jsonFile    :: w ::: Maybe String             <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot    :: w ::: Maybe String             <?> "Path to dapp project root directory (default: . )"
      , debug       :: w ::: Bool                     <?> "Run interactively"
      , fuzzRuns    :: w ::: Maybe Int                <?> "Number of times to run fuzz tests"
      , replay      :: w ::: Maybe (Text, ByteString) <?> "Custom fuzz case to run/debug"
      , rpc         :: w ::: Maybe URL                <?> "Fetch state from a remote node"
      , verbose     :: w ::: Maybe Int                <?> "Append call trace: {1} failures {2} all"
      , coverage    :: w ::: Bool                     <?> "Coverage analysis"
      , state       :: w ::: Maybe String             <?> "Path to state repository"
      , match       :: w ::: Maybe String             <?> "Test case filter - only run methods matching regex"
      }
  | Interactive -- Browse & run unit tests interactively
      { jsonFile :: w ::: Maybe String <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot :: w ::: Maybe String <?> "Path to dapp project root directory (default: . )"
      , rpc      :: w ::: Maybe URL    <?> "Fetch state from a remote node"
      , state    :: w ::: Maybe String <?> "Path to state repository"
      , replay   :: w ::: Maybe (Text, ByteString) <?> "Custom fuzz case to run/debug"
      }
  | BcTest -- Run an Ethereum Blockhain/GeneralState test
      { file    :: w ::: String    <?> "Path to .json test file"
      , test    :: w ::: [String]  <?> "Test case filter - only run specified test method(s)"
      , debug   :: w ::: Bool      <?> "Run interactively"
      , diff    :: w ::: Bool      <?> "Print expected vs. actual state on failure"
      , timeout :: w ::: Maybe Int <?> "Execution timeout (default: 10 sec.)"
      }
  | VmTest -- Run an Ethereum VMTest
      { file    :: w ::: String    <?> "Path to .json test file"
      , test    :: w ::: [String]  <?> "Test case filter - only run specified test method(s)"
      , debug   :: w ::: Bool      <?> "Run interactively"
      , diff    :: w ::: Bool      <?> "Print expected vs. actual state on failure"
      , timeout :: w ::: Maybe Int <?> "Execution timeout (default: 10 sec.)"
      }
  | Compliance -- Run Ethereum Blockhain or VMTest compliance report
      { tests   :: w ::: String       <?> "Path to Ethereum Tests directory"
      , group   :: w ::: Maybe String <?> "Report group to run: VM or Blockchain (default: Blockchain)"
      , match   :: w ::: Maybe String <?> "Test case filter - only run methods matching regex"
      , skip    :: w ::: Maybe String <?> "Test case filter - skip tests containing string"
      , html    :: w ::: Bool         <?> "Output html report"
      , timeout :: w ::: Maybe Int    <?> "Execution timeout (default: 10 sec.)"
      }
  | Flatten -- Concat all dependencies for a given source file
    { sourceFile :: w ::: String       <?> "Path to solidity source file e.g. src/contract.sol"
    , jsonFile   :: w ::: Maybe String <?> "Filename or path to dapp build output (default: out/*.solc.json)"
    , dappRoot   :: w ::: Maybe String <?> "Path to dapp project root directory (default: . )"
    }
  | Emacs
  | Version
  | Rlp  -- RLP decode a string and print the result
  { decode :: w ::: ByteString <?> "RLP encoded hexstring"
  }
  | Abiencode
  { abi  :: w ::: String      <?> "Signature of types to decode / encode"
  , arg  :: w ::: [String]    <?> "Values to encode"
  }
  | MerkleTest -- Insert a set of key values and check against the given root
  { file :: w ::: String <?> "Path to .json test file"
  }
  | StripMetadata -- Remove metadata from contract bytecode
  { code        :: w ::: Maybe ByteString       <?> "Program bytecode"
  }

  deriving (Options.Generic)

type URL = Text


-- For some reason haskell can't derive a
-- parseField instance for (Text, ByteString)
instance Options.ParseField (Text, ByteString)

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

  let
    testn = testNumber params
    block = if 0 == testn
       then EVM.Fetch.Latest
       else EVM.Fetch.BlockNumber testn

  pure EVM.UnitTest.UnitTestOptions
    { EVM.UnitTest.oracle =
        case rpc cmd of
         Just url -> EVM.Fetch.http block url
         Nothing  -> EVM.Fetch.zero
    , EVM.UnitTest.verbose = verbose cmd
    , EVM.UnitTest.match   = pack $ fromMaybe "^test" (match cmd)
    , EVM.UnitTest.fuzzRuns = fromMaybe 100 (fuzzRuns cmd)
    , EVM.UnitTest.replay   = do
        arg <- replay cmd
        return (fst arg, LazyByteString.fromStrict (hexByteString "--replay" $ strip0x $ snd arg))
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
    Assert {} -> assert cmd
    Exec {} ->
      launchExec cmd
    Abiencode {} ->
      print . ByteStringS $ abiencode (abi cmd) (arg cmd)
    VmTest {} ->
      launchTest ExecuteAsVMTest cmd
    BcTest {} ->
      launchTest ExecuteAsBlockchainTest cmd
    DappTest {} ->
      withCurrentDirectory root $ do
        testFile <- findJsonFile (jsonFile cmd)
        testOpts <- unitTestOptions cmd
        case (coverage cmd, optsMode cmd) of
          (False, Run) ->
            dappTest testOpts (optsMode cmd) testFile
          (False, Debug) ->
            EVM.TTY.main testOpts root testFile
          (True, _) ->
            dappCoverage testOpts (optsMode cmd) testFile
    Interactive {} ->
      withCurrentDirectory root $ do
        testFile <- findJsonFile (jsonFile cmd)
        testOpts <- unitTestOptions cmd
        EVM.TTY.main testOpts root testFile
    Compliance {} ->
      case (group cmd) of
        Just "Blockchain" -> launchScript "/run-blockchain-tests" cmd
        Just "VM" -> launchScript "/run-consensus-tests" cmd
        _ -> launchScript "/run-blockchain-tests" cmd
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
    Rlp {} ->
      case rlpdecode $ hexByteString "--decode" $ strip0x $ decode cmd of
        Nothing -> error "Malformed RLP string"
        Just c -> print c
    MerkleTest {} -> merkleTest cmd
    StripMetadata {} -> print .
      ByteStringS . stripBytecodeMetadata . hexByteString "bytecode" . strip0x $ fromJust $ code cmd

launchScript :: String -> Command Options.Unwrapped -> IO ()
launchScript script cmd =
  withCurrentDirectory (tests cmd) $ do
    dataDir <- Paths.getDataDir
    callProcess "bash"
      [ dataDir ++ script
      , "."
      , show (html cmd)
      , fromMaybe "" (match cmd)
      , fromMaybe "" (skip cmd)
      , show $ fromMaybe 10 (timeout cmd)
      ]

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
dappTest opts _ solcFile =
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
regexMatches = error "wait"
-- regexMatches regexSource =
--   let
--     compOpts =
--       Regex.defaultCompOpt { Regex.lastStarGreedy = True }
--     execOpts =
--       Regex.defaultExecOpt { Regex.captureGroups = False }
--     regex = Regex.makeRegexOpts compOpts execOpts (unpack regexSource)
--   in
--     Regex.matchTest regex . Seq.fromList . unpack

assert :: Command Options.Unwrapped -> IO ()
assert cmd =
  if debug cmd
  then do
    -- todo; merge with vmFromCommand or not?
      let Just types = parseFunArgs $ funcSig cmd
          bytecode = maybe (error "bytecode not given") (hexByteString "--code" . strip0x) (code cmd)
      void . runSMT $ query $ do input <- symAbiArg types
                                 let calldata' = litBytes (sig (funcSig cmd)) <> input
                                 symstore <- freshArray_ Nothing
                                 let preState = loadSymVM bytecode symstore calldata'
                                 io (EVM.TTY.runFromVM EVM.Fetch.zero preState)

  else
    let post = Just $ \(input, output) ->
          case view EVM.result output of
            Just (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) -> sFalse
            _ -> sTrue
        bytecode = maybe (error "bytecode not given") EVM.RuntimeCode (code cmd)
    in do results <- runSMT $ query $ verify bytecode (funcSig cmd) (const sTrue) post
          case results of
            Left () -> print "All good"
            Right a -> do print "Assertion violation:"
                          print a

dappCoverage :: UnitTestOptions -> Mode -> String -> IO ()
dappCoverage opts _ solcFile =
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

symbolEVM :: Symbolic (SWord 256, SWord 256)
symbolEVM = do x <- symbolic "x"
               y <- symbolic "y"
               pure (x,y)

launchExec :: Command Options.Unwrapped -> IO ()
launchExec cmd = do
  vm <- vmFromCommand cmd
  vm1 <- case state cmd of
    Nothing -> pure vm
    Just path ->
      -- Note: this will load the code, so if you've specified a state
      -- repository, then you effectively can't change `--code' after
      -- the first run.
      Facts.apply vm <$> Git.loadFacts (Git.RepoAt path)

  case optsMode cmd of
    Run -> do
      vm' <- execStateT (interpret fetcher . void $ EVM.Stepper.execFully) vm1
      case view EVM.result vm' of
        Nothing ->
          error "internal error; no EVM result"
        Just (EVM.VMFailure (EVM.Revert msg)) ->
          die . show . ByteStringS $ msg
        Just (EVM.VMFailure err) ->
          die . show $ err
        Just (EVM.VMSuccess msg) -> do
          print . ByteStringS $ EVM.forceLitBytes msg
          case state cmd of
            Nothing -> pure ()
            Just path ->
              Git.saveFacts (Git.RepoAt path) (Facts.vmFacts vm')
    Debug -> void (EVM.TTY.runFromVM fetcher vm1)
   where fetcher = maybe EVM.Fetch.zero (EVM.Fetch.http block') (rpc cmd)
         block'  = maybe EVM.Fetch.Latest EVM.Fetch.BlockNumber (block cmd)

data Testcase = Testcase {
  _entries :: [(Text, Maybe Text)],
  _root :: Text
} deriving Show

parseTups :: JSON.Value -> JSON.Parser [(Text, Maybe Text)]
parseTups (JSON.Array arr) = do
  tupList <- mapM parseJSON (V.toList arr)
  mapM (\[k, v] -> do
                  rhs <- parseJSON v
                  key <- parseJSON k
                  return (key, rhs))
         tupList
parseTups invalid = JSON.typeMismatch "Malformed array" invalid


parseTrieTest :: JSON.Object -> JSON.Parser Testcase
parseTrieTest p = do
  kvlist <- p .: "in"
  entries <- parseTups kvlist
  root <- p .: "root"
  return $ Testcase entries root

instance FromJSON Testcase where
  parseJSON (JSON.Object p) = parseTrieTest p
  parseJSON invalid = JSON.typeMismatch "Merkle test case" invalid

parseTrieTests :: Lazy.ByteString -> Either String (Map String Testcase)
parseTrieTests = JSON.eitherDecode'

merkleTest :: Command Options.Unwrapped -> IO ()
merkleTest cmd = do
  parsed <- parseTrieTests <$> LazyByteString.readFile (file cmd)
  case parsed of
    Left err -> print err
    Right testcases -> mapM_ runMerkleTest testcases

runMerkleTest :: Testcase -> IO ()
runMerkleTest (Testcase entries root) = case Patricia.calcRoot entries' of
                                          Nothing -> error "Test case failed"
                                          Just n -> case n == strip0x (hexText root) of
                                            True -> putStrLn "Test case success"
                                            False -> error ("Test case failure; expected "
                                                            <> show root <> " but got " <> show (ByteStringS n))
  where entries' = fmap (\(k, v) ->
                           (tohexOrText k,
                            tohexOrText (fromMaybe mempty v)))
                   entries

tohexOrText :: Text -> ByteString
tohexOrText s = case "0x" `Char8.isPrefixOf` encodeUtf8 s of
                  True -> hexText s
                  False -> encodeUtf8 s


vmFromCommand :: Command Options.Unwrapped -> IO EVM.VM
vmFromCommand cmd = do
  vm <- case (code cmd, rpc cmd) of
     (Nothing, Nothing) -> error "Missing: --code TEXT or --rpc TEXT"
     (Just c,  Nothing) -> return $ vm1 (EVM.initialContract (codeType (hexByteString "--code" (strip0x c))))
     (a     , Just url) -> do maybeContract <- EVM.Fetch.fetchContractFrom block' url address'
                              case maybeContract of
                                  Nothing -> error $ "contract not found: " <> show address'
                                  Just contract' -> case a of
                                    Nothing -> return (vm1 contract')
                                    -- if both code and url is given,
                                    -- fetch the contract then overwrite the code
                                    Just c -> return $ (vm1 contract') & set (EVM.state . EVM.code) c
  return $ vm & EVM.env . EVM.contracts . ix address' . EVM.balance +~ (w256 value')
      where
        block'   = maybe EVM.Fetch.Latest EVM.Fetch.BlockNumber (block cmd)
        value'   = word value 0
        caller'  = addr caller 0
        origin'  = addr origin caller'
        codeType = if create cmd then EVM.InitCode else EVM.RuntimeCode
        address' = if create cmd
              then createAddress origin' (word nonce 0)
              else addr address 0xacab
    
        vm1 c = EVM.makeVm $ EVM.VMOpts
          { EVM.vmoptContract      = c
          , EVM.vmoptCalldata      = maybe [] (litBytes . (hexByteString "--calldata") . strip0x)
                                       (calldata cmd)
          , EVM.vmoptValue         = value'
          , EVM.vmoptAddress       = address'
          , EVM.vmoptCaller        = caller'
          , EVM.vmoptOrigin        = origin'
          , EVM.vmoptGas           = word gas 0
          , EVM.vmoptGaslimit      = word gas 0
          , EVM.vmoptCoinbase      = addr coinbase 0
          , EVM.vmoptNumber        = word number 0
          , EVM.vmoptTimestamp     = word timestamp 0
          , EVM.vmoptBlockGaslimit = word gaslimit 0
          , EVM.vmoptGasprice      = word gasprice 0
          , EVM.vmoptMaxCodeSize   = word maxcodesize 0xffffffff
          , EVM.vmoptDifficulty    = word difficulty 0
          , EVM.vmoptSchedule      = FeeSchedule.istanbul
          , EVM.vmoptCreate        = create cmd
          }
        word f def = fromMaybe def (f cmd)
        addr f def = fromMaybe def (f cmd)

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
runVMTest diffmode execmode mode timelimit (name, x) = error "not now"
-- runVMTest diffmode execmode mode timelimit (name, x) = do
--   let vm0 = VMTest.vmForCase execmode x
--   putStr (name ++ " ")
--   hFlush stdout
--   result <- do
--     action <- async $
--       case mode of
--         Run ->
--           Timeout.timeout (1e6 * (fromMaybe 10 timelimit)) . evaluate $ do
--             execState (VMTest.interpret . void $ EVM.Stepper.execFully) vm0
--         Debug ->
--           Just <$> EVM.TTY.runFromVM EVM.Fetch.zero vm0
--     waitCatch action
--   case result of
--     Right (Just vm1) -> do
--       ok <- VMTest.checkExpectation diffmode execmode x vm1
--       putStrLn (if ok then "ok" else "")
--       return ok
--     Right Nothing -> do
--       putStrLn "timeout"
--       return False
--     Left e -> do
--       putStrLn $ "error: " ++ if diffmode
--         then show e
--         else (head . lines . show) e
--       return False

#endif

interpret
  :: (EVM.Query -> IO (EVM.EVM ()))
  -> EVM.Stepper.Stepper a
  -> StateT EVM.VM IO (Either EVM.Stepper.Failure a)
interpret fetcher =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView EVM.Stepper.Action a
      -> StateT EVM.VM IO (Either EVM.Stepper.Failure a)

    eval (Operational.Return x) =
      pure (Right x)

    eval (action Operational.:>>= k) =
      case action of
        EVM.Stepper.Exec ->
          exec >>= interpret fetcher . k
        EVM.Stepper.Wait q ->
          do m <- liftIO (fetcher q)
             State.state (runState m) >> interpret fetcher (k ())
        EVM.Stepper.Note _ ->
          interpret fetcher (k ())
        EVM.Stepper.Fail e ->
          pure (Left e)
        EVM.Stepper.EVM m ->
          State.state (runState m) >>= interpret fetcher . k

abiencode :: (AsValue s) => s -> [String] -> ByteString
abiencode abi args =
  let declarations = parseMethodInput <$> V.toList (abi ^?! key "inputs" . _Array)
      sig = signature abi
  in if length declarations == length args
     then abiMethod sig $ AbiTuple . V.fromList $ zipWith makeAbiValue (snd <$> declarations) args
     else error $ "wrong number of arguments:" <> show (length args) <> ": " <> show args
