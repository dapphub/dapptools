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
import EVM.Concrete (w256, litWord, sw256)
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
import EVM.ABI (sig, decodeAbiValue, AbiType(..))
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

import Control.Concurrent.Async   (async, waitCatch)
import Control.Exception          (evaluate)
import qualified Control.Monad.Operational as Operational
import qualified Control.Monad.State.Class as State
import Control.Lens
import Control.Applicative
import Control.Monad              (void, when, forM_, (>=>))
import Control.Monad.State.Strict (execState, runState, runStateT, StateT, liftIO, execStateT, lift)
import Data.ByteString            (ByteString)
import Data.List                  (intercalate, isSuffixOf)
import Data.Text                  (Text, unpack, pack, splitOn)
import Data.Text.Encoding         (encodeUtf8)
import Data.Maybe                 (fromMaybe, fromJust)
import Data.Version               (showVersion)
import Data.SBV hiding (Word, solver, verbose)
import qualified Data.SBV as SBV
import Data.SBV.Control hiding (Word, verbose, Version, timeout, create)
import System.Directory           (withCurrentDirectory, listDirectory)
import System.Exit                (die, exitFailure)
import System.IO                  (hFlush, stdout)
import System.Environment         (setEnv)
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
import qualified Text.Regex.TDFA as Regex

import Options.Generic as Options

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command w
  = Symbolic -- Execute a given program with specified env & calldata
      { code          :: w ::: Maybe ByteString <?> "Program bytecode"
      , jsonFile      :: w ::: Maybe String     <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , funcSig       :: w ::: Maybe Text       <?> "Function signature"
      , debug         :: w ::: Bool             <?> "Run interactively"
      , getModels     :: w ::: Bool             <?> "Print example testcase for each execution path"
      , smttimeout    :: w ::: Maybe Integer    <?> "Timeout given to smt solver in seconds"
      , maxIterations :: w ::: Maybe Integer    <?> "Number of times we may revisit a particular branching point"
      , solver        :: w ::: Maybe Text       <?> "Smt solver to use z3 (default) or cvc4"
      }
  | Equivalence -- prove equivalence between two programs
      { codeA   :: w ::: ByteString <?> "Bytecode of the first program"
      , codeB   :: w ::: ByteString <?> "Bytecode of the second program"
      , funcSig :: w ::: Maybe Text <?> "Function signature"
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
      , jsonFile    :: w ::: Maybe String     <?> "Filename or path to dapp build output (default: out/*.solc.json)"
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
    Symbolic {} -> assert cmd
    Equivalence {} -> equivalenceCheck cmd
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
regexMatches regexSource =
  let
    compOpts =
      Regex.defaultCompOpt { Regex.lastStarGreedy = True }
    execOpts =
      Regex.defaultExecOpt { Regex.captureGroups = False }
    regex = Regex.makeRegexOpts compOpts execOpts (unpack regexSource)
  in
    Regex.matchTest regex . Seq.fromList . unpack

equivalenceCheck :: Command Options.Unwrapped -> IO ()
equivalenceCheck cmd =
  do let bytecodeA = hexByteString "--code" . strip0x $ codeA cmd
         bytecodeB = hexByteString "--code" . strip0x $ codeB cmd
         types = parseFunArgs =<< funcSig cmd
     void . runSMT . query $ do
           (preStateA, preStateB) <- both (abstractVM (funcSig cmd)) (bytecodeA, bytecodeB)
           smtState <- queryState
           (aRes, bRes) <- both (\x -> io $ fst <$> runStateT (interpret (EVM.Fetch.oracle smtState Nothing) Nothing EVM.Stepper.runFully) x)
                             (preStateA, preStateB)
           resetAssertions
           case (aRes, bRes) of
             (Left errA, Right _) -> error $ "A Failed: " <> show errA
             (Right _, Left errB) -> error $ "B Failed: " <> show errB
             (Left errA, Left errB) -> error $ "A Failed: " <> show errA <> "\nand B Failed:" <> show errB
             (Right aVMs, Right bVMs) -> do
               let differingEndStates = flip fmap [(a,b) | a <- aVMs, b <- bVMs] $
               -- for each pair of endstates, if there exists an input such both
               -- path conditions are satisfiably, then the results must be equal
                     (\ab -> let (aPath, bPath) = both' (view EVM.pathConditions) ab
                                 (aSelf, bSelf) = both' (view (EVM.state . EVM.contract)) ab
                                 (aEnv, bEnv) = both' (view (EVM.env . EVM.contracts)) ab
                                 (aResult, bResult) = both' (view EVM.result) ab
                                 notSame = fromBool (aSelf /= bSelf)
                                 (EVM.Symbolic aStorage, EVM.Symbolic bStorage) = (view EVM.storage (aEnv ^?! ix aSelf), view EVM.storage (bEnv ^?! ix bSelf))
                                 differingResults = case (aResult, bResult) of
                                      (Just (EVM.VMSuccess a), Just (EVM.VMSuccess b)) -> a ./= b .|| notSame .|| aStorage ./= bStorage
                                      (Just (EVM.VMFailure _), Just (EVM.VMFailure _)) -> sFalse
                                      (Just _, Just _) -> sTrue
                                      _ -> error "Internal error during symbolic execution (should not be possible)"
                             in (sAnd aPath .&& sAnd bPath .&& differingResults))
               -- If there exists a pair of endstates where this is not the case,
               -- the following constraint is satisfiable
               constrain $ sOr differingEndStates

               checkSat >>= \case
                  Unk -> error "solver said unknown!"
                  Sat -> do let (calldata', cdlen) = view (EVM.state . EVM.calldata) preStateA
                            cdlen <- num <$> getValue cdlen
                            model <- mapM (getValue.fromSized) (take cdlen calldata')
                            io $ do putStrLn $ "Not equal!"
                                    putStrLn $ "Counterexample:"
                                    case types of
                                      Just typ -> print $ decodeAbiValue typ $ Lazy.fromStrict (ByteString.pack (drop 4 model))
                                      Nothing -> print $ ByteStringS (ByteString.pack model)
                                    exitFailure
                  Unsat -> return ()


-- prelude plz
both' :: (a -> b) -> (a, a) -> (b, b)
both' f (x, y) = (f x, f y)

-- cvc4 sets timeout via a commandline option instead of smtlib `(set-option)`
runSMTWithTimeOut :: Maybe Text -> Maybe Integer -> Symbolic a -> IO a
runSMTWithTimeOut (Just "cvc4") Nothing sym  = runSMTWith cvc4 sym
runSMTWithTimeOut _             Nothing sym  = runSMTWith z3 sym
runSMTWithTimeOut (Just "cvc4") (Just n) sym = do setEnv "SBV_CVC4_OPTIONS" ("--lang=smt --incremental --interactive --no-interactive-prompt --model-witness-value --tlimit-per=" <> show n)
                                                  a <- runSMTWith cvc4 sym
                                                  setEnv "SBV_CVC4_OPTIONS" ""
                                                  return a
runSMTWithTimeOut _            (Just n) sym = runSMTWith z3 $ do setTimeOut n
                                                                 sym

-- Although it is tempting to fully abstract calldata and give any hints about the nature of the signature
-- doing so results in significant time spent in consulting z3 about rather trivial matters. But with cvc4 it is quite pleasant!

-- If function signatures are known, they should always be given for best results.
assert :: Command Options.Unwrapped -> IO ()
assert cmd =
  let bytecode = maybe (error "bytecode not given") (hexByteString "--code" . strip0x) (code cmd)
      root = fromMaybe "." (dappRoot cmd)
      srcinfo = ((,) root) <$> (jsonFile cmd)
  in if debug cmd
     then runSMTWithTimeOut (solver cmd) (smttimeout cmd) $
                                  query $ do preState <- abstractVM (funcSig cmd) bytecode
                                             smtState <- queryState
                                             io $ void $ EVM.TTY.runFromVM srcinfo (EVM.Fetch.oracle smtState Nothing) preState

     else runSMTWithTimeOut (solver cmd) (smttimeout cmd) $
             query $ do results <- checkAssert (EVM.RuntimeCode bytecode) (maxIterations cmd) (funcSig cmd)
                        case results of
                          Right a -> io $ do print "Assertion violation:"
                                             die . show $ ByteStringS a
                          Left (pre, posts) ->
                            do io $ putStrLn $ "Explored: " <> show (length posts) <> " branches successfully"
                               -- When `--get-model` is passed, we print example calldata for each path
                               when (getModels cmd) $
                                 let (calldata', cdlen) = view (EVM.state . EVM.calldata) pre
                                 in forM_ (zip [1..] posts) $ \(i, postVM) -> do
                                   resetAssertions
                                   constrain (sAnd (view EVM.pathConditions postVM))
                                   io $ putStrLn $ "-- Branch (" <> show i <> "/" <> show (length posts) <> ") --"
                                   checkSat >>= \case
                                     Unk -> io $ putStrLn "Timed out"
                                     Unsat -> io $ putStrLn "Inconsistent path conditions: dead path"
                                     Sat -> do
                                       cdlen <- num <$> getValue cdlen
                                       calldatainput <- mapM (getValue.fromSized) (take cdlen calldata')
                                       io $ do
                                         print $ "Calldata:"
                                         print $ ByteStringS (ByteString.pack calldatainput)
                                       case view EVM.result postVM of
                                         Nothing -> error "internal error; no EVM result"
                                         Just (EVM.VMFailure (EVM.Revert "")) -> io . putStrLn $ "Reverted"
                                         Just (EVM.VMFailure (EVM.Revert msg)) -> let inputbytes = (decodeAbiValue AbiStringType $ Lazy.fromStrict (ByteString.drop 4 msg))
                                                                                  in io . putStrLn $ "Reverted: " <> show inputbytes
                                         Just (EVM.VMFailure err) -> io . putStrLn $ "Failed: " <> show err
                                         Just (EVM.VMSuccess []) -> io $ putStrLn "Stopped"
                                         Just (EVM.VMSuccess msg) -> do output <- mapM (getValue.fromSized) msg
                                                                        io . putStrLn $ "Returned: " <> show (ByteStringS (ByteString.pack output))


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
  let root = fromMaybe "." (dappRoot cmd)
      srcinfo = ((,) root) <$> (jsonFile cmd)
  vm <- vmFromCommand cmd
  vm1 <- case state cmd of
    Nothing -> pure vm
    -- Note: this will load the code, so if you've specified a state
    -- repository, then you effectively can't change `--code' after
    -- the first run.
    Just path -> Facts.apply vm <$> Git.loadFacts (Git.RepoAt path)

  case optsMode cmd of
    Run -> do
      vm' <- execStateT (interpret fetcher Nothing . void $ EVM.Stepper.execFully) vm1
      case view EVM.result vm' of
        Nothing -> error "internal error; no EVM result"
        Just (EVM.VMFailure (EVM.Revert msg)) -> die . show . ByteStringS $ msg
        Just (EVM.VMFailure err) -> die . show $ err
        Just (EVM.VMSuccess msg) -> print . ByteStringS $ EVM.forceLitBytes msg
      case state cmd of
        Nothing -> pure ()
        Just path ->
          Git.saveFacts (Git.RepoAt path) (Facts.vmFacts vm')
    Debug -> void $ EVM.TTY.runFromVM srcinfo fetcher vm1
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
  vm <- case (rpc cmd) of
     Nothing  -> return . vm1 . EVM.initialContract $ maybe (EVM.RuntimeCode "") (codeType . hexByteString "--code" . strip0x) (code cmd)
     Just url -> do maybeContract <- EVM.Fetch.fetchContractFrom block' url address'
                    case maybeContract of
                      Nothing -> error $ "contract not found: " <> show address'
                      Just contract' -> case (code cmd) of
                        Nothing -> return (vm1 contract')
                        -- if both code and url is given,
                        -- fetch the contract then overwrite the code
                        Just c -> return $ (vm1 contract') & set (EVM.state . EVM.code) c

  return $ vm & EVM.env . EVM.contracts . ix address' . EVM.balance +~ (w256 value')
      where
        block'   = maybe EVM.Fetch.Latest EVM.Fetch.BlockNumber (block cmd)
        value'   = word value 0
        caller'  = addr caller 0
        origin'  = addr origin 0
        calldata' = litBytes $ bytes calldata ""
        codeType = if create cmd then EVM.InitCode else EVM.RuntimeCode
        address' = if create cmd
              then createAddress origin' (word nonce 0)
              else addr address 0xacab
        vm1 c = EVM.makeVm $ EVM.VMOpts
          { EVM.vmoptContract      = c
          , EVM.vmoptCalldata      = (calldata', literal . num $ length calldata')
          , EVM.vmoptValue         = value'
          , EVM.vmoptAddress       = address'
          , EVM.vmoptCaller        = litAddr caller'
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
        bytes f def = maybe def id (f cmd)

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
  let vm0 = VMTest.vmForCase execmode x
  putStr (name ++ " ")
  hFlush stdout
  result <- do
    action <- async $
      case mode of
        Run ->
          Timeout.timeout (1e6 * (fromMaybe 10 timelimit)) . evaluate $ do
            execState (VMTest.interpret . void $ EVM.Stepper.execFully) vm0
        Debug ->
          Just <$> EVM.TTY.runFromVM Nothing EVM.Fetch.zero vm0
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

abiencode :: (AsValue s) => s -> [String] -> ByteString
abiencode abi args =
  let declarations = parseMethodInput <$> V.toList (abi ^?! key "inputs" . _Array)
      sig = signature abi
  in if length declarations == length args
     then abiMethod sig $ AbiTuple . V.fromList $ zipWith makeAbiValue (snd <$> declarations) args
     else error $ "wrong number of arguments:" <> show (length args) <> ": " <> show args
