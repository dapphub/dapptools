-- Main file of the hevm CLI program

{-# Language CPP #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TypeOperators #-}
{-# Language RecordWildCards #-}

module Main where

import EVM (StorageModel(..))
import qualified EVM
import EVM.Concrete (createAddress, Whiff(..), wordValue)
import EVM.Symbolic (forceLitBytes, litAddr, w256lit, SymWord(..), len, forceLit)
import qualified EVM.FeeSchedule as FeeSchedule
import qualified EVM.Fetch
import qualified EVM.Flatten
import qualified EVM.Stepper
import qualified EVM.TTY
import qualified EVM.Emacs
import EVM.Dev (concatMapM)

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM.SymExec
import EVM.Debug
import EVM.ABI
import EVM.Solidity
import EVM.Types hiding (word)
import EVM.UnitTest (UnitTestOptions, coverageReport, coverageForUnitTestContract)
import EVM.UnitTest (runUnitTestContract)
import EVM.UnitTest (getParametersFromEnvironmentVariables, testNumber)
import EVM.Dapp (findUnitTests, dappInfo, DappInfo)
import EVM.Format (showTraceTree, showBranchTree)
import EVM.RLP (rlpdecode)
import qualified EVM.Patricia as Patricia
import Data.Map (Map)

import qualified EVM.Facts     as Facts
import qualified EVM.Facts.Git as Git
import qualified EVM.UnitTest

import GHC.IO.Encoding
import Control.Concurrent.Async   (async, waitCatch)
import Control.Lens hiding (pre, passing)
import Control.Monad              (void, when, forM_, unless)
import Control.Monad.State.Strict (execStateT, liftIO)
import Data.ByteString            (ByteString)
import Data.List                  (intercalate, isSuffixOf)
import Data.Tree
import Data.Text                  (Text, unpack, pack)
import Data.Text.Encoding         (encodeUtf8)
import Data.Text.IO               (hPutStr)
import Data.Maybe                 (fromMaybe, fromJust)
import Data.Version               (showVersion)
import Data.SBV hiding (Word, solver, verbose, name)
import Data.SBV.Control hiding (Version, timeout, create)
import System.IO                  (hFlush, stdout, stderr, utf8)
import System.Directory           (withCurrentDirectory, listDirectory)
import System.Exit                (exitFailure, exitWith, ExitCode(..))
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
  = Symbolic -- Symbolically explore an abstract program, or specialized with specified env & calldata
  -- vm opts
      { code          :: w ::: Maybe ByteString <?> "Program bytecode"
      , calldata      :: w ::: Maybe ByteString <?> "Tx: calldata"
      , address       :: w ::: Maybe Addr       <?> "Tx: address"
      , caller        :: w ::: Maybe Addr       <?> "Tx: caller"
      , origin        :: w ::: Maybe Addr       <?> "Tx: origin"
      , coinbase      :: w ::: Maybe Addr       <?> "Block: coinbase"
      , value         :: w ::: Maybe W256       <?> "Tx: Eth amount"
      , nonce         :: w ::: Maybe W256       <?> "Nonce of origin"
      , gas           :: w ::: Maybe W256       <?> "Tx: gas amount"
      , number        :: w ::: Maybe W256       <?> "Block: number"
      , timestamp     :: w ::: Maybe W256       <?> "Block: timestamp"
      , gaslimit      :: w ::: Maybe W256       <?> "Tx: gas limit"
      , gasprice      :: w ::: Maybe W256       <?> "Tx: gas price"
      , create        :: w ::: Bool             <?> "Tx: creation"
      , maxcodesize   :: w ::: Maybe W256       <?> "Block: max code size"
      , difficulty    :: w ::: Maybe W256       <?> "Block: difficulty"
      , chainid       :: w ::: Maybe W256       <?> "Env: chainId"
  -- remote state opts
      , rpc           :: w ::: Maybe URL        <?> "Fetch state from a remote node"
      , block         :: w ::: Maybe W256       <?> "Block state is be fetched from"
      , state         :: w ::: Maybe String     <?> "Path to state repository"
      , cache         :: w ::: Maybe String     <?> "Path to rpc cache repository"

  -- symbolic execution opts
      , jsonFile      :: w ::: Maybe String       <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot      :: w ::: Maybe String       <?> "Path to dapp project root directory (default: . )"
      , storageModel  :: w ::: Maybe StorageModel <?> "Select storage model: ConcreteS, SymbolicS (default) or InitialS"
      , sig           :: w ::: Maybe Text         <?> "Signature of types to decode / encode"
      , arg           :: w ::: [String]           <?> "Values to encode"
      , debug         :: w ::: Bool               <?> "Run interactively"
      , getModels     :: w ::: Bool               <?> "Print example testcase for each execution path"
      , showTree      :: w ::: Bool               <?> "Print branches explored in tree view"
      , smttimeout    :: w ::: Maybe Integer      <?> "Timeout given to SMT solver in milliseconds (default: 20000)"
      , maxIterations :: w ::: Maybe Integer      <?> "Number of times we may revisit a particular branching point"
      , solver        :: w ::: Maybe Text         <?> "Used SMT solver: z3 (default) or cvc4"
      }
  | Equivalence -- prove equivalence between two programs
      { codeA         :: w ::: ByteString    <?> "Bytecode of the first program"
      , codeB         :: w ::: ByteString    <?> "Bytecode of the second program"
      , sig           :: w ::: Maybe Text    <?> "Signature of types to decode / encode"
      , smttimeout    :: w ::: Maybe Integer <?> "Timeout given to SMT solver in milliseconds (default: 20000)"
      , maxIterations :: w ::: Maybe Integer <?> "Number of times we may revisit a particular branching point"
      , solver        :: w ::: Maybe Text    <?> "Used SMT solver: z3 (default) or cvc4"
      , smtoutput     :: w ::: Bool          <?> "Print verbose smt output"
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
      , chainid     :: w ::: Maybe W256       <?> "Env: chainId"
      , debug       :: w ::: Bool             <?> "Run interactively"
      , trace       :: w ::: Bool             <?> "Dump trace"
      , state       :: w ::: Maybe String     <?> "Path to state repository"
      , cache       :: w ::: Maybe String     <?> "Path to rpc cache repository"
      , rpc         :: w ::: Maybe URL        <?> "Fetch state from a remote node"
      , block       :: w ::: Maybe W256       <?> "Block state is be fetched from"
      , jsonFile    :: w ::: Maybe String     <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot    :: w ::: Maybe String     <?> "Path to dapp project root directory (default: . )"
      }
  | DappTest -- Run DSTest unit tests
      { jsonFile    :: w ::: Maybe String             <?> "Filename or path to dapp build output (default: out/*.solc.json)"
      , dappRoot    :: w ::: Maybe String             <?> "Path to dapp project root directory (default: . )"
      , debug         :: w ::: Bool                     <?> "Run interactively"
      , fuzzRuns      :: w ::: Maybe Int                <?> "Number of times to run fuzz tests"
      , replay        :: w ::: Maybe (Text, ByteString) <?> "Custom fuzz case to run/debug"
      , rpc           :: w ::: Maybe URL                <?> "Fetch state from a remote node"
      , verbose       :: w ::: Maybe Int                <?> "Append call trace: {1} failures {2} all"
      , coverage      :: w ::: Bool                     <?> "Coverage analysis"
      , state         :: w ::: Maybe String             <?> "Path to state repository"
      , cache         :: w ::: Maybe String             <?> "Path to rpc cache repository"
      , match         :: w ::: Maybe String             <?> "Test case filter - only run methods matching regex"
      , smttimeout    :: w ::: Maybe Integer            <?> "Timeout given to SMT solver in milliseconds (default: 20000)"
      , maxIterations :: w ::: Maybe Integer            <?> "Number of times we may revisit a particular branching point"
      , solver        :: w ::: Maybe Text               <?> "Used SMT solver: z3 (default) or cvc4"
      }
  | BcTest -- Run an Ethereum Blockhain/GeneralState test
      { file    :: w ::: String    <?> "Path to .json test file"
      , test    :: w ::: [String]  <?> "Test case filter - only run specified test method(s)"
      , debug   :: w ::: Bool      <?> "Run interactively"
      , diff    :: w ::: Bool      <?> "Print expected vs. actual state on failure"
      , timeout :: w ::: Maybe Int <?> "Execution timeout (default: 10 sec.)"
      }
  | Compliance -- Run Ethereum Blockhain compliance report
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
  { abi  :: w ::: Maybe String <?> "Signature of types to decode / encode"
  , arg  :: w ::: [String]     <?> "Values to encode"
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

applyCache :: (Maybe String, Maybe String) -> IO (EVM.VM -> EVM.VM)
applyCache (state, cache) =
  let applyState = flip Facts.apply
      applyCache' = flip Facts.applyCache
  in case (state, cache) of
    (Nothing, Nothing) -> do
      pure id
    (Nothing, Just cachePath) -> do
      facts <- Git.loadFacts (Git.RepoAt cachePath)
      pure $ applyCache' facts
    (Just statePath, Nothing) -> do
      facts <- Git.loadFacts (Git.RepoAt statePath)
      pure $ applyState facts
    (Just statePath, Just cachePath) -> do
      cacheFacts <- Git.loadFacts (Git.RepoAt cachePath)
      stateFacts <- Git.loadFacts (Git.RepoAt statePath)
      pure $ (applyState stateFacts) . (applyCache' cacheFacts)

unitTestOptions :: Command Options.Unwrapped -> String -> Query UnitTestOptions
unitTestOptions cmd testFile = do
  let root = fromMaybe "." (dappRoot cmd)
  srcInfo <- liftIO $ readSolc testFile >>= \case
    Nothing -> error "Could not read .sol.json file"
    Just (contractMap, sourceCache) ->
      pure $ dappInfo root contractMap sourceCache

  vmModifier <- liftIO $ applyCache (state cmd, cache cmd)

  params <- liftIO $ getParametersFromEnvironmentVariables (rpc cmd)
  state <- queryState

  let
    testn = testNumber params
    block' = if 0 == testn
       then EVM.Fetch.Latest
       else EVM.Fetch.BlockNumber testn

  pure EVM.UnitTest.UnitTestOptions
    { EVM.UnitTest.oracle =
        case rpc cmd of
         Just url -> EVM.Fetch.oracle (Just state) (Just (block', url)) True
         Nothing  -> EVM.Fetch.oracle (Just state) Nothing True
    , EVM.UnitTest.maxIter = maxIterations cmd
    , EVM.UnitTest.smtTimeout = smttimeout cmd
    , EVM.UnitTest.solver = solver cmd
    , EVM.UnitTest.smtState = Just state
    , EVM.UnitTest.verbose = verbose cmd
    , EVM.UnitTest.match = pack $ fromMaybe ".*" (match cmd)
    , EVM.UnitTest.fuzzRuns = fromMaybe 100 (fuzzRuns cmd)
    , EVM.UnitTest.replay = do
        arg' <- replay cmd
        return (fst arg', LazyByteString.fromStrict (hexByteString "--replay" $ strip0x $ snd arg'))
    , EVM.UnitTest.vmModifier = vmModifier
    , EVM.UnitTest.testParams = params
    , EVM.UnitTest.dapp = srcInfo
    }

main :: IO ()
main = do
  cmd <- Options.unwrapRecord "hevm -- Ethereum evaluator"
  let
    root = fromMaybe "." (dappRoot cmd)
  case cmd of
    Version {} -> putStrLn (showVersion Paths.version)
    Symbolic {} -> assert cmd
    Equivalence {} -> equivalence cmd
    Exec {} ->
      launchExec cmd
    Abiencode {} ->
      print . ByteStringS $ abiencode (abi cmd) (arg cmd)
    BcTest {} ->
      launchTest cmd
    DappTest {} ->
      withCurrentDirectory root $ do
        testFile <- findJsonFile (jsonFile cmd)
        runSMTWithTimeOut (solver cmd) (smttimeout cmd) $ query $ do
          testOpts <- unitTestOptions cmd testFile
          case (coverage cmd, optsMode cmd) of
            (False, Run) -> dappTest testOpts testFile (cache cmd)
            (False, Debug) -> liftIO $ EVM.TTY.main testOpts root testFile
            (True, _) -> liftIO $ dappCoverage testOpts (optsMode cmd) testFile
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

dappTest :: UnitTestOptions -> String -> Maybe String -> Query ()
dappTest opts solcFile cache = do
  out <- liftIO $ readSolc solcFile
  case out of
    Just (contractMap, _) -> do
      let matcher = regexMatches (EVM.UnitTest.match opts)
          unitTests = findUnitTests matcher $ Map.elems contractMap
      results <- concatMapM (runUnitTestContract opts contractMap) unitTests
      let (passing, vms) = unzip results
      case cache of
        Nothing ->
          pure ()
        Just path ->
          -- merge all of the post-vm caches and save into the state
          let
            cache' = mconcat [view EVM.cache vm | vm <- vms]
          in
            liftIO $ Git.saveFacts (Git.RepoAt path) (Facts.cacheFacts cache')

      liftIO $ unless (and passing) exitFailure
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

equivalence :: Command Options.Unwrapped -> IO ()
equivalence cmd =
  do let bytecodeA = hexByteString "--code" . strip0x $ codeA cmd
         bytecodeB = hexByteString "--code" . strip0x $ codeB cmd
     maybeSignature <- case sig cmd of
       Nothing -> return Nothing
       Just sig' -> do method' <- functionAbi sig'
                       return $ Just (view methodSignature method', snd <$> view methodInputs method')

     void . runSMTWithTimeOut (solver cmd) (smttimeout cmd) . query $
       equivalenceCheck bytecodeA bytecodeB (maxIterations cmd) maybeSignature >>= \case
         Right vm -> do io $ putStrLn "Not equal!"
                        io $ putStrLn "Counterexample:"
                        showCounterexample vm maybeSignature
                        io exitFailure
         Left (postAs, postBs) -> io $ do
           putStrLn $ "Explored: " <> show (length postAs)
                       <> " execution paths of A and: "
                       <> show (length postBs) <> " paths of B."
           putStrLn "No discrepancies found."


-- cvc4 sets timeout via a commandline option instead of smtlib `(set-option)`
runSMTWithTimeOut :: Maybe Text -> Maybe Integer -> Symbolic a -> IO a
runSMTWithTimeOut solver maybeTimeout sym
  | solver == Just "cvc4" = do
      setEnv "SBV_CVC4_OPTIONS" ("--lang=smt --incremental --interactive --no-interactive-prompt --model-witness-value --tlimit-per=" <> show timeout)
      a <- runSMTWith cvc4 sym
      setEnv "SBV_CVC4_OPTIONS" ""
      return a
  | solver == Just "z3" = runwithz3
  | solver == Nothing = runwithz3
  | otherwise = error "Unknown solver. Currently supported solvers; z3, cvc4"
 where timeout = fromMaybe 20000 maybeTimeout
       runwithz3 = runSMTWith z3 $ (setTimeOut timeout) >> sym


checkForVMErrors :: [EVM.VM] -> [String]
checkForVMErrors [] = []
checkForVMErrors (vm:vms) =
  case view EVM.result vm of
    Just (EVM.VMFailure EVM.UnexpectedSymbolicArg) ->
      ("Unexpected symbolic argument at opcode: "
      <> maybe "??" show (EVM.vmOp vm)
      <> ". Not supported (yet!)"
      ) : checkForVMErrors vms
    _ ->
      checkForVMErrors vms

emptyDapp :: DappInfo
emptyDapp = dappInfo "" mempty (SourceCache mempty mempty mempty mempty)

getSrcInfo :: Command Options.Unwrapped -> IO DappInfo
getSrcInfo cmd =
  let root = fromMaybe "." (dappRoot cmd)
  in case (jsonFile cmd) of
    Nothing ->
      pure emptyDapp
    Just json -> readSolc json >>= \case
      Nothing ->
        pure emptyDapp
      Just (contractMap, sourceCache) ->
        pure $ dappInfo root contractMap sourceCache

-- Although it is tempting to fully abstract calldata and give any hints about
-- the nature of the signature doing so results in significant time spent in
-- consulting z3 about rather trivial matters. But with cvc4 it is quite
-- pleasant!

-- If function signatures are known, they should always be given for best results.
assert :: Command Options.Unwrapped -> IO ()
assert cmd = do
  let block'  = maybe EVM.Fetch.Latest EVM.Fetch.BlockNumber (block cmd)
      rpcinfo = (,) block' <$> rpc cmd
      treeShowing :: Tree BranchInfo -> Query ()
      treeShowing tree =
        when (showTree cmd) $ do
          consistentTree tree >>= \case
            Nothing -> io $ putStrLn "No consistent paths" -- unlikely
            Just tree' -> io $ setLocaleEncoding utf8 >> putStrLn (showBranchTree tree')

  srcInfo <- getSrcInfo cmd
  maybesig <- case sig cmd of
    Nothing ->
      return Nothing
    Just sig' -> do
      method' <- functionAbi sig'
      let typ = snd <$> view methodInputs method'
          name = view methodSignature method'
      return $ Just (name,typ)
  if debug cmd then
    runSMTWithTimeOut (solver cmd) (smttimeout cmd) $ query $ do
      preState <- symvmFromCommand cmd
      smtState <- queryState
      io $ void $ EVM.TTY.runFromVM
        (maxIterations cmd)
        srcInfo
        (EVM.Fetch.oracle (Just smtState) rpcinfo True)
        preState

  else
    runSMTWithTimeOut (solver cmd) (smttimeout cmd) $ query $ do
      preState <- symvmFromCommand cmd
      verify preState (maxIterations cmd) rpcinfo (Just checkAssertions) >>= \case
        Right tree -> do
          io $ putStrLn "Assertion violation found."
          showCounterexample preState maybesig
          treeShowing tree
          io $ exitWith (ExitFailure 1)
        Left tree -> do
          io $ putStrLn $ "Explored: " <> show (length tree)
                       <> " branches without assertion violations"
          treeShowing tree
          let vmErrs = checkForVMErrors $ leaves tree
          unless (null vmErrs) $ io $ do
            putStrLn $
              "However, "
              <> show (length vmErrs)
              <> " branch(es) errored while exploring:"
            print vmErrs
          -- When `--get-models` is passed, we print example vm info for each path
          when (getModels cmd) $
            forM_ (zip [(1:: Integer)..] (leaves tree)) $ \(i, postVM) -> do
              resetAssertions
              constrain (sAnd (fst <$> view EVM.constraints postVM))
              io $ putStrLn $
                "-- Branch (" <> show i <> "/" <> show (length tree) <> ") --"
              checkSat >>= \case
                DSat _ -> error "assert: unexpected SMT result"
                Unk -> io $ do putStrLn "Timed out"
                               print $ view EVM.result postVM
                Unsat -> io $ do putStrLn "Inconsistent path conditions: dead path"
                                 print $ view EVM.result postVM
                Sat -> do
                  showCounterexample preState maybesig
                  io $ putStrLn "-- Pathconditions --"
                  io $ print $ snd <$> view EVM.constraints postVM
                  case view EVM.result postVM of
                    Nothing ->
                      error "internal error; no EVM result"
                    Just (EVM.VMFailure (EVM.Revert "")) -> io . putStrLn $
                      "Reverted"
                    Just (EVM.VMFailure (EVM.Revert msg)) -> io . putStrLn $
                      "Reverted: " <> show (ByteStringS msg)
                    Just (EVM.VMFailure err) -> io . putStrLn $
                      "Failed: " <> show err
                    Just (EVM.VMSuccess (ConcreteBuffer msg)) ->
                      if ByteString.null msg
                      then io $ putStrLn
                        "Stopped"
                      else io $ putStrLn $
                        "Returned: " <> show (ByteStringS msg)
                    Just (EVM.VMSuccess (SymbolicBuffer msg)) -> do
                      out <- mapM (getValue.fromSized) msg
                      io . putStrLn $
                        "Returned: " <> show (ByteStringS (ByteString.pack out))

dappCoverage :: UnitTestOptions -> Mode -> String -> IO ()
dappCoverage opts _ solcFile =
  readSolc solcFile >>=
    \case
      Just (contractMap, sourceCache) -> do
        let matcher = regexMatches (EVM.UnitTest.match opts)
            unitTests = findUnitTests matcher $ Map.elems contractMap
        covs <- mconcat <$> mapM (coverageForUnitTestContract opts contractMap sourceCache) unitTests

        let
          dapp = dappInfo "." contractMap sourceCache
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
  dapp <- getSrcInfo cmd
  vm <- vmFromCommand cmd
  case optsMode cmd of
    Run -> do
      vm' <- execStateT (EVM.Stepper.interpret fetcher . void $ EVM.Stepper.execFully) vm
      when (trace cmd) $ hPutStr stderr (showTraceTree dapp vm')
      case view EVM.result vm' of
        Nothing ->
          error "internal error; no EVM result"
        Just (EVM.VMFailure (EVM.Revert msg)) -> do
          print $ ByteStringS msg
          exitWith (ExitFailure 2)
        Just (EVM.VMFailure err) -> do
          print err
          exitWith (ExitFailure 2)
        Just (EVM.VMSuccess buf) -> do
          let msg = case buf of
                SymbolicBuffer msg' -> forceLitBytes msg'
                ConcreteBuffer msg' -> msg'
          print $ ByteStringS msg
          case state cmd of
            Nothing -> pure ()
            Just path ->
              Git.saveFacts (Git.RepoAt path) (Facts.vmFacts vm')
          case cache cmd of
            Nothing -> pure ()
            Just path ->
              Git.saveFacts (Git.RepoAt path) (Facts.cacheFacts (view EVM.cache vm'))

    Debug -> void $ EVM.TTY.runFromVM Nothing dapp fetcher vm
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
                  lhs <- parseJSON k
                  return (lhs, rhs))
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
runMerkleTest (Testcase entries root) =
  case Patricia.calcRoot entries' of
    Nothing ->
      error "Test case failed"
    Just n ->
      case n == strip0x (hexText root) of
        True ->
          putStrLn "Test case success"
        False ->
          error ("Test case failure; expected " <> show root
                 <> " but got " <> show (ByteStringS n))
  where entries' = fmap (\(k, v) ->
                           (tohexOrText k,
                            tohexOrText (fromMaybe mempty v)))
                   entries

tohexOrText :: Text -> ByteString
tohexOrText s = case "0x" `Char8.isPrefixOf` encodeUtf8 s of
                  True -> hexText s
                  False -> encodeUtf8 s

-- | Creates a (concrete) VM from command line options
vmFromCommand :: Command Options.Unwrapped -> IO EVM.VM
vmFromCommand cmd = do
  withCache <- applyCache (state cmd, cache cmd)

  (miner,ts,blockNum,diff) <- case rpc cmd of
    Nothing -> return (0,0,0,0)
    Just url -> EVM.Fetch.fetchBlockFrom block' url >>= \case
      Nothing -> error "Could not fetch block"
      Just EVM.Block{..} -> return (_coinbase
                                   , wordValue $ forceLit _timestamp
                                   , wordValue _number
                                   , wordValue _difficulty
                                   )

  contract <- case (rpc cmd, address cmd, code cmd) of
    (Just url, Just addr', Just c) -> do
      EVM.Fetch.fetchContractFrom block' url addr' >>= \case
        Nothing ->
          error $ "contract not found: " <> show address'
        Just contract' ->
          -- if both code and url is given,
          -- fetch the contract and overwrite the code
          return $
            EVM.initialContract  (codeType $ hexByteString "--code" $ strip0x c)
              & set EVM.storage  (view EVM.storage  contract')
              & set EVM.balance  (view EVM.balance  contract')
              & set EVM.nonce    (view EVM.nonce    contract')
              & set EVM.external (view EVM.external contract')

    (Just url, Just addr', Nothing) ->
      EVM.Fetch.fetchContractFrom block' url addr' >>= \case
        Nothing ->
          error $ "contract not found: " <> show address'
        Just contract' -> return contract'

    (_, _, Just c)  ->
      return $
        EVM.initialContract (codeType $ hexByteString "--code" $ strip0x c)

    (_, _, Nothing) ->
      error "must provide at least (rpc + address) or code"

  return $ VMTest.initTx $ withCache (vm0 miner ts blockNum diff contract)
    where
        decipher = hexByteString "bytes" . strip0x
        block'   = maybe EVM.Fetch.Latest EVM.Fetch.BlockNumber (block cmd)
        value'   = word value 0
        caller'  = addr caller 0
        origin'  = addr origin 0
        calldata' = ConcreteBuffer $ bytes calldata ""
        codeType = if create cmd then EVM.InitCode else EVM.RuntimeCode
        address' = if create cmd
              then createAddress origin' (word nonce 0)
              else addr address 0xacab

        vm0 miner ts blockNum diff c = EVM.makeVm $ EVM.VMOpts
          { EVM.vmoptContract      = c
          , EVM.vmoptCalldata      = (calldata', literal . num $ len calldata')
          , EVM.vmoptValue         = w256lit value'
          , EVM.vmoptAddress       = address'
          , EVM.vmoptCaller        = litAddr caller'
          , EVM.vmoptOrigin        = origin'
          , EVM.vmoptGas           = word gas 0
          , EVM.vmoptGaslimit      = word gas 0
          , EVM.vmoptCoinbase      = addr coinbase miner
          , EVM.vmoptNumber        = word number blockNum
          , EVM.vmoptTimestamp     = w256lit $ word timestamp ts
          , EVM.vmoptBlockGaslimit = word gaslimit 0
          , EVM.vmoptGasprice      = word gasprice 0
          , EVM.vmoptMaxCodeSize   = word maxcodesize 0xffffffff
          , EVM.vmoptDifficulty    = word difficulty diff
          , EVM.vmoptSchedule      = FeeSchedule.istanbul
          , EVM.vmoptChainId       = word chainid 1
          , EVM.vmoptCreate        = create cmd
          , EVM.vmoptStorageModel  = ConcreteS
          }
        word f def = fromMaybe def (f cmd)
        addr f def = fromMaybe def (f cmd)
        bytes f def = maybe def decipher (f cmd)

symvmFromCommand :: Command Options.Unwrapped -> Query EVM.VM
symvmFromCommand cmd = do

  (miner,blockNum,diff) <- case rpc cmd of
    Nothing -> return (0,0,0)
    Just url -> io $ EVM.Fetch.fetchBlockFrom block' url >>= \case
      Nothing -> error $ "Could not fetch block"
      Just EVM.Block{..} -> return (_coinbase
                                   , wordValue _number
                                   , wordValue _difficulty
                                   )

  caller' <- maybe (SAddr <$> freshVar_) (return . litAddr) (caller cmd)
  ts <- maybe (S (Var "Timestamp") <$> freshVar_) (return . w256lit) (timestamp cmd)
  callvalue' <- maybe ((S (Var "CallValue")) <$> freshVar_) (return . w256lit) (value cmd)
  (calldata', cdlen, pathCond) <- case (calldata cmd, sig cmd) of
    -- fully abstract calldata (up to 256 bytes)
    (Nothing, Nothing) -> do
      cd <- sbytes256
      len' <- freshVar_
      return (SymbolicBuffer cd, len', (len' .<= 256, Val "len < 256"))
    -- fully concrete calldata
    (Just c, Nothing) ->
      let cd = ConcreteBuffer $ decipher c
      in return (cd, num (len cd), (sTrue, Dull))
    -- calldata according to given abi with possible specializations from the `arg` list
    (Nothing, Just sig') -> do
      method' <- io $ functionAbi sig'
      let typs = snd <$> view methodInputs method'
      (cd, cdlen) <- symCalldata (view methodSignature method') typs (arg cmd)
      return (SymbolicBuffer cd, cdlen, (sTrue, Dull))

    _ -> error "incompatible options: calldata and abi"

  store <- case storageModel cmd of
    -- InitialS and SymbolicS can read and write to symbolic locations
    -- ConcreteS cannot (instead values can be fetched from rpc!)
    -- Initial defaults to 0 for uninitialized storage slots,
    -- whereas the values of SymbolicS are unconstrained.
    Just InitialS  -> EVM.Symbolic <$> freshArray_ (Just 0)
    Just ConcreteS -> return (EVM.Concrete mempty)
    Just SymbolicS -> EVM.Symbolic <$> freshArray_ Nothing
    Nothing -> EVM.Symbolic <$> freshArray_ (if create cmd then (Just 0) else Nothing)

  withCache <- io $ applyCache (state cmd, cache cmd)

  contract' <- case (rpc cmd, address cmd, code cmd) of
    (Just url, Just addr', _) ->
      io (EVM.Fetch.fetchContractFrom block' url addr') >>= \case
        Nothing ->
          error "contract not found."
        Just contract' -> return contract''
          where
            contract'' = case code cmd of
              Nothing -> contract'
              -- if both code and url is given,
              -- fetch the contract and overwrite the code
              Just c -> EVM.initialContract (codeType $ decipher c)
                        & set EVM.origStorage (view EVM.origStorage contract')
                        & set EVM.balance     (view EVM.balance contract')
                        & set EVM.nonce       (view EVM.nonce contract')
                        & set EVM.external    (view EVM.external contract')

    (_, _, Just c)  ->
      return $ (EVM.initialContract . codeType $ decipher c)
    (_, _, Nothing) ->
      error "must provide at least (rpc + address) or code"

  return $ (VMTest.initTx $ withCache $ vm0 miner ts blockNum diff cdlen calldata' callvalue' caller' contract')
    & over EVM.constraints (<> [pathCond])
    & set (EVM.env . EVM.contracts . (ix address') . EVM.storage) store

  where
    decipher = hexByteString "bytes" . strip0x
    block'   = maybe EVM.Fetch.Latest EVM.Fetch.BlockNumber (block cmd)
    origin'  = addr origin 0
    codeType = if create cmd then EVM.InitCode else EVM.RuntimeCode
    address' = if create cmd
          then createAddress origin' (word nonce 0)
          else addr address 0xacab
    vm0 miner ts blockNum diff cdlen calldata' callvalue' caller' c = EVM.makeVm $ EVM.VMOpts
      { EVM.vmoptContract      = c
      , EVM.vmoptCalldata      = (calldata', cdlen)
      , EVM.vmoptValue         = callvalue'
      , EVM.vmoptAddress       = address'
      , EVM.vmoptCaller        = caller'
      , EVM.vmoptOrigin        = origin'
      , EVM.vmoptGas           = word gas 0xffffffffffffffff
      , EVM.vmoptGaslimit      = word gas 0xffffffffffffffff
      , EVM.vmoptCoinbase      = addr coinbase miner
      , EVM.vmoptNumber        = word number blockNum
      , EVM.vmoptTimestamp     = ts
      , EVM.vmoptBlockGaslimit = word gaslimit 0
      , EVM.vmoptGasprice      = word gasprice 0
      , EVM.vmoptMaxCodeSize   = word maxcodesize 0xffffffff
      , EVM.vmoptDifficulty    = word difficulty diff
      , EVM.vmoptSchedule      = FeeSchedule.istanbul
      , EVM.vmoptChainId       = word chainid 1
      , EVM.vmoptCreate        = create cmd
      , EVM.vmoptStorageModel  = fromMaybe SymbolicS (storageModel cmd)
      }
    word f def = fromMaybe def (f cmd)
    addr f def = fromMaybe def (f cmd)

launchTest :: Command Options.Unwrapped ->  IO ()
launchTest cmd = do
#if MIN_VERSION_aeson(1, 0, 0)
  parsed <- VMTest.parseBCSuite <$> LazyByteString.readFile (file cmd)
  case parsed of
     Left "No cases to check." -> putStrLn "no-cases ok"
     Left err -> print err
     Right allTests ->
       let testFilter =
             if null (test cmd)
             then id
             else filter (\(x, _) -> elem x (test cmd))
       in
         mapM_ (runVMTest (diff cmd) (optsMode cmd) (timeout cmd)) $
           testFilter (Map.toList allTests)
#else
  putStrLn "Not supported"
#endif

#if MIN_VERSION_aeson(1, 0, 0)
runVMTest :: Bool -> Mode -> Maybe Int -> (String, VMTest.Case) -> IO Bool
runVMTest diffmode mode timelimit (name, x) =
 do
  let vm0 = VMTest.vmForCase x
  putStr (name ++ " ")
  hFlush stdout
  result <- do
    action <- async $
      case mode of
        Run ->
          Timeout.timeout (1000000 * (fromMaybe 10 timelimit)) $
            execStateT (EVM.Stepper.interpret EVM.Fetch.zero . void $ EVM.Stepper.execFully) vm0
        Debug ->
          Just <$> EVM.TTY.runFromVM Nothing emptyDapp EVM.Fetch.zero vm0
    waitCatch action
  case result of
    Right (Just vm1) -> do
      ok <- VMTest.checkExpectation diffmode x vm1
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

parseAbi :: (AsValue s) => s -> (Text, [AbiType])
parseAbi abijson =
  (signature abijson, snd
    <$> parseMethodInput
    <$> V.toList
      (fromMaybe (error "Malformed function abi") (abijson ^? key "inputs" . _Array)))

abiencode :: (AsValue s) => Maybe s -> [String] -> ByteString
abiencode Nothing _ = error "missing required argument: abi"
abiencode (Just abijson) args =
  let (sig', declarations) = parseAbi abijson
  in if length declarations == length args
     then abiMethod sig' $ AbiTuple . V.fromList $ zipWith makeAbiValue declarations args
     else error $ "wrong number of arguments:" <> show (length args) <> ": " <> show args
