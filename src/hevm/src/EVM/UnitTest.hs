module EVM.UnitTest where

import Prelude hiding (Word)

import EVM
import EVM.ABI
import EVM.Concrete
import EVM.Dapp
import EVM.Debug (srcMapCodePos)
import EVM.Exec
import EVM.Format
import EVM.Solidity
import EVM.Types

import qualified EVM.FeeSchedule as FeeSchedule

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import Control.Lens hiding (Indexed)
import Control.Monad ((>=>))
import Control.Monad.State.Strict hiding (state)
import qualified Control.Monad.State.Strict as State

import Control.Monad.Par.Class (spawn_)
import Control.Monad.Par.IO (runParIO)

import qualified Data.ByteString.Lazy as BSLazy
import Data.ByteString    (ByteString)
import Data.SBV
import Data.Foldable      (toList)
import Data.Map           (Map)
import Data.Maybe         (fromMaybe, catMaybes, fromJust, isJust, fromMaybe, mapMaybe)
import Data.Monoid        ((<>))
import Data.Text          (isPrefixOf, stripSuffix, intercalate, Text, pack, unpack)
import Data.Word          (Word32)
import System.Environment (lookupEnv)
import System.IO          (hFlush, stdout)

import qualified Control.Monad.Par.Class as Par
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Test.QuickCheck hiding (verbose)

data UnitTestOptions = UnitTestOptions
  {
    oracle     :: Query -> IO (EVM ())
  , verbose    :: Maybe Int
  , match      :: Text
  , fuzzRuns   :: Int
  , replay     :: Maybe (Text, BSLazy.ByteString)
  , vmModifier :: VM -> VM
  , testParams :: TestVMParams
  }

data TestVMParams = TestVMParams
  { testAddress       :: Addr
  , testCaller        :: Addr
  , testOrigin        :: Addr
  , testGasCreate     :: W256
  , testGasCall       :: W256
  , testBalanceCreate :: W256
  , testBalanceCall   :: W256
  , testCoinbase      :: Addr
  , testNumber        :: W256
  , testTimestamp     :: W256
  , testGaslimit      :: W256
  , testGasprice      :: W256
  , testMaxCodeSize   :: W256
  , testDifficulty    :: W256
  , testChainId       :: W256
  }

defaultGasForCreating :: W256
defaultGasForCreating = 0xffffffffffff

defaultGasForInvoking :: W256
defaultGasForInvoking = 0xffffffffffff

defaultBalanceForCreator :: W256
defaultBalanceForCreator = 0xffffffffffffffffffffffff

defaultBalanceForCreated :: W256
defaultBalanceForCreated = 0xffffffffffffffffffffffff

defaultMaxCodeSize :: W256
defaultMaxCodeSize = 0xffffffff

type ABIMethod = Text

-- | Assuming a constructor is loaded, this stepper will run the constructor
-- to create the test contract, give it an initial balance, and run `setUp()'.
initializeUnitTest :: UnitTestOptions -> Stepper ()
initializeUnitTest UnitTestOptions { .. } = do

  -- Maybe modify the initial VM, e.g. to load library code
  Stepper.evm (modify vmModifier)

  -- Make a trace entry for running the constructor
  Stepper.evm (pushTrace (EntryTrace "constructor"))

  -- Constructor is loaded; run until it returns code
  bytes <- Stepper.execFullyOrFail
  addr <- Stepper.evm (use (state . contract))

  -- Mutate the current contract to use the new code
  Stepper.evm $ replaceCodeOfSelf (RuntimeCode (forceLitBytes bytes))

  -- Give a balance to the test target
  Stepper.evm $
    env . contracts . ix addr . balance += w256 (testBalanceCreate testParams)

  -- Initialize the test contract
  Stepper.evm $
    setupCall testParams addr "setUp()" emptyAbi
  Stepper.evm (popTrace >> pushTrace (EntryTrace "initialize test"))

  Stepper.note "Running `setUp()'"

  -- Let `setUp()' run to completion
  void Stepper.execFullyOrFail
  Stepper.evm (use result) >>= \case
    Just (VMFailure e) -> Stepper.evm (pushTrace (ErrorTrace e))
    _ -> Stepper.evm popTrace


-- | Assuming a test contract is loaded and initialized, this stepper
-- will run the specified test method and return whether it succeeded.
runUnitTest :: UnitTestOptions -> ABIMethod -> AbiValue -> Stepper Bool
runUnitTest a method args = do
  x <- execTest a method args
  checkFailures a method args x

execTest :: UnitTestOptions -> ABIMethod -> AbiValue -> Stepper Bool
execTest UnitTestOptions { .. } method args = do
  -- The test subject should be loaded and initialized already
  addr <- Stepper.evm $ use (state . contract)
  -- Set up the call to the test method
  Stepper.evm $
    setupCall testParams addr method args
  Stepper.evm (pushTrace (EntryTrace method))
  Stepper.note "Running unit test"
  -- Try running the test method
  bailed <-
    Stepper.execFully >>=
      either (const (pure True)) (const (pure False))
  -- If we failed, put the error in the trace.
  -- It's not clear to me right now why this doesn't happen somewhere else.
  Stepper.evm (use result) >>= \case
    Just (VMFailure e) ->
        Stepper.evm (pushTrace (ErrorTrace e))
    _ -> pure ()
  pure bailed

checkFailures :: UnitTestOptions -> ABIMethod -> AbiValue -> Bool -> Stepper Bool
checkFailures UnitTestOptions { .. } method args bailed = do
     -- Decide whether the test is supposed to fail or succeed
     let shouldFail = "testFail" `isPrefixOf` method

     -- The test subject should be loaded and initialized already
     addr <- Stepper.evm $ use (state . contract)

     -- Ask whether any assertions failed
     Stepper.evm popTrace
     Stepper.evm $ setupCall testParams addr "failed()" args
     Stepper.note "Checking whether assertions failed"
     res <- do arg <- Stepper.execFullyOrFail
               Stepper.decode AbiBoolType (forceLitBytes arg)
     let AbiBool failed = res
     -- Return true if the test was successful
     pure (shouldFail == (bailed || failed))

-- | Randomly generates the calldata arguments and runs the test
fuzzTest :: UnitTestOptions -> Text -> [AbiType] -> VM -> Property
fuzzTest opts sig types vm = forAllShow (genAbiValue (AbiTupleType $ Vector.fromList types)) (show . ByteStringS . encodeAbiValue)
  $ \args -> ioProperty $
    (runStateT (interpret opts (runUnitTest opts sig args)) vm) >>= \case
      (Left _, _) -> return False
      (Right b, _) -> return b

tick :: Text -> IO ()
tick x = Text.putStr x >> hFlush stdout

interpret
  :: UnitTestOptions
  -> Stepper a
  -> StateT VM IO (Either Stepper.Failure a)
interpret opts =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> StateT VM IO (Either Stepper.Failure a)

    eval (Operational.Return x) =
      pure (Right x)

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          exec >>= interpret opts . k
        Stepper.Wait q ->
          do m <- liftIO (oracle opts q)
             State.state (runState m) >> interpret opts (k ())
        Stepper.Note _ ->
          interpret opts (k ())
        Stepper.Fail e ->
          pure (Left e)
        Stepper.EVM m ->
          State.state (runState m) >>= interpret opts . k

-- | This is like an unresolved source mapping.
data OpLocation = OpLocation
  { srcCodehash :: !W256
  , srcOpIx     :: !Int
  } deriving (Eq, Ord, Show)

srcMapForOpLocation :: DappInfo -> OpLocation -> Maybe SrcMap
srcMapForOpLocation dapp (OpLocation hash opIx) =
  case preview (dappSolcByHash . ix hash) dapp of
    Nothing -> Nothing
    Just (codeType, solc) ->
      let
        vec =
          case codeType of
            Runtime  -> view runtimeSrcmap solc
            Creation -> view creationSrcmap solc
      in
        preview (ix opIx) vec

type CoverageState = (VM, MultiSet OpLocation)

currentOpLocation :: VM -> OpLocation
currentOpLocation vm =
  case currentContract vm of
    Nothing ->
      error "internal error: why no contract?"
    Just c ->
      OpLocation
        (view codehash c)
        (fromMaybe (error "internal error: op ix") (vmOpIx vm))

execWithCoverage :: StateT CoverageState IO VMResult
execWithCoverage = do
  -- This is just like `exec` except for every instruction evaluated,
  -- we also increment a counter indexed by the current code location.
  vm0 <- use _1
  case view result vm0 of
    Nothing -> do
      vm1 <- zoom _1 (State.state (runState exec1) >> get)
      zoom _2 (modify (MultiSet.insert (currentOpLocation vm1)))
      execWithCoverage
    Just r ->
      pure r

interpretWithCoverage
  :: UnitTestOptions
  -> Stepper a
  -> StateT CoverageState IO (Either Stepper.Failure a)
interpretWithCoverage opts =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> StateT CoverageState IO (Either Stepper.Failure a)

    eval (Operational.Return x) =
      pure (Right x)

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          execWithCoverage >>= interpretWithCoverage opts . k
        Stepper.Wait q ->
          do m <- liftIO (oracle opts q)
             zoom _1 (State.state (runState m)) >> interpretWithCoverage opts (k ())
        Stepper.Note _ ->
          interpretWithCoverage opts (k ())
        Stepper.Fail e ->
          pure (Left e)
        Stepper.EVM m ->
          zoom _1 (State.state (runState m)) >>= interpretWithCoverage opts . k

coverageReport
  :: DappInfo
  -> MultiSet SrcMap
  -> Map Text (Vector (Int, ByteString))
coverageReport dapp cov =
  let
    sources :: SourceCache
    sources = view dappSources dapp

    allPositions :: Set (Text, Int)
    allPositions =
      ( Set.fromList
      . mapMaybe (srcMapCodePos sources)
      . toList
      $ mconcat
        ( view dappSolcByName dapp
        & Map.elems
        & map (\x -> view runtimeSrcmap x <> view creationSrcmap x)
        )
      )

    srcMapCov :: MultiSet (Text, Int)
    srcMapCov = MultiSet.mapMaybe (srcMapCodePos sources) cov

    -- linesByName :: Map Text (Vector ByteString)
    linesByName =
      ( Map.fromList
      . map
          (\(k, v) ->
             (fst (fromJust (Map.lookup k (view sourceFiles sources))), v))
      . Map.toList
      $ view sourceLines sources
      )

    f :: Text -> Vector ByteString -> Vector (Int, ByteString)
    f name =
      Vector.imap
        (\i bs ->
           let
             n =
               if Set.member (name, i + 1) allPositions
               then MultiSet.occur (name, i + 1) srcMapCov
               else -1
           in (n, bs))
  in
    Map.mapWithKey f linesByName

coverageForUnitTestContract
  :: UnitTestOptions
  -> Map Text SolcContract
  -> SourceCache
  -> (Text, [(Text, [AbiType])])
  -> IO (MultiSet SrcMap)
coverageForUnitTestContract
  opts@(UnitTestOptions {..}) contractMap sources (name, testNames) = do

  -- Look for the wanted contract by name from the Solidity info
  case preview (ix name) contractMap of
    Nothing ->
      -- Fail if there's no such contract
      error $ "Contract " ++ unpack name ++ " not found"

    Just theContract -> do
      -- Construct the initial VM and begin the contract's constructor
      let vm0 = initialUnitTestVm opts theContract (Map.elems contractMap)
      (vm1, cov1) <-
        execStateT
          (interpretWithCoverage opts
            (Stepper.enter name >> initializeUnitTest opts))
          (vm0, mempty)

      -- Define the thread spawner for test cases
      let
        runOne (testName, _) = spawn_ . liftIO $ do
          (x, (_, cov)) <-
            runStateT
              (interpretWithCoverage opts (runUnitTest opts testName emptyAbi))
              (vm1, mempty)
          case x of
            Right True -> pure cov
            _ -> error "test failure during coverage analysis; fix it!"

      -- Run all the test cases in parallel and gather their coverages
      covs <-
        runParIO (mapM runOne testNames >>= mapM Par.get)

      -- Sum up all the coverage counts
      let cov2 = MultiSet.unions (cov1 : covs)

      -- Gather the dapp-related metadata
      let dapp = dappInfo "." contractMap sources

      pure (MultiSet.mapMaybe (srcMapForOpLocation dapp) cov2)

runUnitTestContract
  :: UnitTestOptions
  -> Map Text SolcContract
  -> SourceCache
  -> (Text, [(Text, [AbiType])])
  -> IO Bool
runUnitTestContract
  opts@(UnitTestOptions {..}) contractMap sources (name, testSigs) = do

  -- Print a header
  putStrLn $ "Running " ++ show (length testSigs) ++ " tests for "
    ++ unpack name

  -- Look for the wanted contract by name from the Solidity info
  case preview (ix name) contractMap of
    Nothing ->
      -- Fail if there's no such contract
      error $ "Contract " ++ unpack name ++ " not found"

    Just theContract -> do
      -- Construct the initial VM and begin the contract's constructor
      let vm0 = initialUnitTestVm opts theContract (Map.elems contractMap)
      vm1 <-
        execStateT
          (interpret opts
            (Stepper.enter name >> initializeUnitTest opts))
          vm0

      -- Gather the dapp-related metadata
      let dapp = dappInfo "." contractMap sources

      case view result vm1 of
        Nothing -> error "internal error: setUp() did not end with a result"
        Just (VMFailure _) -> do
          Text.putStrLn "\x1b[31m[FAIL]\x1b[0m setUp()"
          tick "\n"
          tick $ failOutput vm1 dapp opts "setUp()"
          pure False
        Just (VMSuccess _) -> do

          -- Define the thread spawner for normal test cases
          let
            runOne testName args = do
              let argInfo = pack (if args == emptyAbi then "" else " with arguments: " <> show args)
              x <-
                runStateT
                  (interpret opts (execTest opts testName args))
                  vm1
              case x of
                (Right b, vm2) -> do
                  y <- runStateT
                    (interpret opts (checkFailures opts testName args b))
                    vm2
                  case y of
                    (Right True,  vm) ->
                      let gasSpent = num (testGasCall testParams) - view (state . gas) vm2
                          gasText = pack . show $ (fromIntegral gasSpent :: Integer)
                      in
                        pure
                        ("\x1b[32m[PASS]\x1b[0m "
                         <> testName <> argInfo <> " (gas: " <> gasText <> ")"
                        , Right (passOutput vm dapp opts testName))
                    (Right False, vm) ->
                             pure ("\x1b[31m[FAIL]\x1b[0m "
                             <> testName <> argInfo, Left (failOutput vm dapp opts testName))
                    (Left _, _)       ->
                             pure ("\x1b[33m[OOPS]\x1b[0m "
                             <> testName <> argInfo, Left ("VM error for " <> testName))

                (Left _, _)       ->
                  pure ("\x1b[33m[OOPS]\x1b[0m "
                        <> testName <> argInfo, Left ("VM error for " <> testName))

          -- Define the thread spawner for property based tests
          let fuzzRun (testName, types) = do
                let args = Args{ replay          = Nothing
                               , maxSuccess      = fuzzRuns
                               , maxDiscardRatio = 10
                               , maxSize         = 100
                               , chatty          = isJust verbose
                               , maxShrinks      = maxBound
                               }
                res <- quickCheckWithResult args (fuzzTest opts testName types vm1)
                case res of
                  Success numTests _ _ _ _ _ ->
                    pure ("\x1b[32m[PASS]\x1b[0m "
                           <> testName <> " (runs: " <> (pack $ show numTests) <> ")",
                           -- vm1 isn't quite the post vm we want...
                           -- but the vm we want is not accessible here anyway...
                           Right (passOutput vm1 dapp opts testName))
                  Failure _ _ _ _ _ _ _ _ _ _ failCase _ _ ->
                    let abiValue = decodeAbiValue (AbiTupleType (Vector.fromList types)) $ BSLazy.fromStrict $ hexText (pack $ concat failCase)
                        ppOutput = pack $ show abiValue
                    in do
                    -- Run the failing test again to get a proper trace
                    vm2 <- execStateT (interpret opts (runUnitTest opts testName abiValue)) vm1
                    pure ("\x1b[31m[FAIL]\x1b[0m "
                             <> testName <> ". Counterexample: " <> ppOutput
                             <> "\nRun:\n dapp test --replay '(\"" <> testName <> "\",\""
                             <> (pack (concat failCase)) <> "\")'\nto test this case again, or \n dapp debug --replay '(\""
                             <> testName <> "\",\"" <> (pack (concat failCase)) <> "\")'\nto debug it.",
                             Left (failOutput vm2 dapp opts testName))
                  _ -> pure ("\x1b[31m[OOPS]\x1b[0m "
                              <> testName, Left (failOutput vm1 dapp opts testName))

          let runTest (testName, []) = runOne testName emptyAbi
              runTest (testName, types) = case replay of
                Nothing -> fuzzRun (testName, types)
                Just (sig, callData) -> if sig == testName
                                         then runOne testName $
                                              decodeAbiValue (AbiTupleType (Vector.fromList types)) callData
                                         else fuzzRun (testName, types)

          let inform (x, y) = Text.putStrLn x >> pure y

          -- Run all the test cases and print their status updates
          details <-
            mapM (runTest >=> inform) testSigs

          let running = [x | Right x <- details]
          let bailing = [x | Left x <- details]

          tick "\n"
          tick (Text.unlines (filter (not . Text.null) running))
          tick (Text.unlines (filter (not . Text.null) bailing))

          pure (null bailing)

indentLines :: Int -> Text -> Text
indentLines n s =
  let p = Text.replicate n " "
  in Text.unlines (map (p <>) (Text.lines s))

passOutput :: VM -> DappInfo -> UnitTestOptions -> Text -> Text
passOutput vm dapp UnitTestOptions { .. } testName =
  case verbose of
    Just 2 ->
      mconcat
        [ "Success: "
        , fromMaybe "" (stripSuffix "()" testName)
        , "\n"
        , indentLines 2 (formatTestLogs (view dappEventMap dapp) (view logs vm))
        , indentLines 2 (showTraceTree dapp vm)
        , "\n"
        ]
    _ ->
      ""

failOutput :: VM -> DappInfo -> UnitTestOptions -> Text -> Text
failOutput vm dapp UnitTestOptions { .. } testName = mconcat
  [ "Failure: "
  , fromMaybe "" (stripSuffix "()" testName)
  , "\n"
  , indentLines 2 (formatTestLogs (view dappEventMap dapp) (view logs vm))
  , case verbose of
      Nothing -> ""
      _       -> indentLines 2 (showTraceTree dapp vm)
  ]

formatTestLogs :: Map W256 Event -> Seq.Seq Log -> Text
formatTestLogs events xs =
  case catMaybes (toList (fmap (formatTestLog events) xs)) of
    [] -> "\n"
    ys -> "\n" <> intercalate "\n" ys <> "\n\n"

formatTestLog :: Map W256 Event -> Log -> Maybe Text
formatTestLog _ (Log _ _ []) = Nothing
formatTestLog events (Log _ args (topic:_)) =
  case maybeLitWord topic of
    Nothing -> Nothing 
    Just t -> case (Map.lookup (wordValue t) events) of
                   Nothing -> Nothing
                   Just (Event name _ _) -> case name of
                     "log_bytes32" ->
                       Just $ formatSBytes args
               
                     "log_named_bytes32" ->
                       let key = take 32 args
                           val = drop 32 args
                       in Just $ formatSString key <> ": " <> formatSBytes val
               
                     "log_named_address" ->
                       let key = take 32 args
                           val = drop 44 args
                       in Just $ formatSString key <> ": " <> formatSBinary val
               
                     "log_named_int" ->
                       let key = take 32 args
                           val = case maybeLitWord (swordAt 32 args) of
                             Just c -> showDec Signed (wordValue c)
                             Nothing -> "<symbolic int>"
                      in Just $ formatSString key <> ": " <> val
               
                     "log_named_uint" ->
                       let key = take 32 args
                           val = case maybeLitWord (swordAt 32 args) of
                             Just c -> showDec Unsigned (wordValue c)
                             Nothing -> "<symbolic uint>"
                       in Just $ formatSString key <> ": " <> val

-- TODO: event logs (bytes);
-- TODO: event log_named_decimal_int  (bytes32 key, int val, uint decimals);
-- TODO: event log_named_decimal_uint (bytes32 key, uint val, uint decimals);

                     _ -> Nothing

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

setupCall :: TestVMParams -> Addr -> Text -> AbiValue -> EVM ()
setupCall params target sig args  = do
  let TestVMParams {..} = params
  resetState
  loadContract target
  assign (state . calldata) $ (litBytes $ abiMethod sig args, literal . num . BS.length $ abiMethod sig args)
  assign (state . caller) (litAddr testCaller)
  assign (state . gas) (w256 testGasCall)

initialUnitTestVm :: UnitTestOptions -> SolcContract -> [SolcContract] -> VM
initialUnitTestVm (UnitTestOptions {..}) theContract _ =
  let
    TestVMParams {..} = testParams
    vm = makeVm $ VMOpts
           { vmoptContract = initialContract (InitCode (view creationCode theContract))
           , vmoptCalldata = ([], 0)
           , vmoptValue = 0
           , vmoptAddress = testAddress
           , vmoptCaller = (litAddr testCaller)
           , vmoptOrigin = testOrigin
           , vmoptGas = testGasCreate
           , vmoptGaslimit = testGasCreate
           , vmoptCoinbase = testCoinbase
           , vmoptNumber = testNumber
           , vmoptTimestamp = testTimestamp
           , vmoptBlockGaslimit = testGaslimit
           , vmoptGasprice = testGasprice
           , vmoptMaxCodeSize = testMaxCodeSize
           , vmoptDifficulty = testDifficulty
           , vmoptSchedule = FeeSchedule.istanbul
           , vmoptCreate = False
           }
    creator =
      initialContract (RuntimeCode mempty)
        & set nonce 1
        & set balance (w256 testBalanceCreate)
  in vm
    & set (env . contracts . at ethrunAddress) (Just creator)
    & set (env . chainId) (w256 testChainId)

getParametersFromEnvironmentVariables :: IO TestVMParams
getParametersFromEnvironmentVariables = do
  let
    getWord s def = maybe def read <$> lookupEnv s
    getAddr s def = maybe def read <$> lookupEnv s

  TestVMParams
    <$> getAddr "DAPP_TEST_ADDRESS" (createAddress ethrunAddress 1)
    <*> getAddr "DAPP_TEST_CALLER" ethrunAddress
    <*> getAddr "DAPP_TEST_ORIGIN" ethrunAddress
    <*> getWord "DAPP_TEST_GAS_CREATE" defaultGasForCreating
    <*> getWord "DAPP_TEST_GAS_CALL" defaultGasForInvoking
    <*> getWord "DAPP_TEST_BALANCE_CREATE" defaultBalanceForCreator
    <*> getWord "DAPP_TEST_BALANCE_CALL" defaultBalanceForCreated
    <*> getAddr "DAPP_TEST_COINBASE" 0
    <*> getWord "DAPP_TEST_NUMBER" 0
    <*> getWord "DAPP_TEST_TIMESTAMP" 1
    <*> getWord "DAPP_TEST_GAS_LIMIT" 0
    <*> getWord "DAPP_TEST_GAS_PRICE" 0
    <*> getWord "DAPP_TEST_MAXCODESIZE" defaultMaxCodeSize
    <*> getWord "DAPP_TEST_DIFFICULTY" 1
    <*> getWord "DAPP_TEST_CHAINID" 99
