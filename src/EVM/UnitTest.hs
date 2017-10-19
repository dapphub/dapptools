{-# LANGUAGE ViewPatterns #-}

module EVM.UnitTest where

import Prelude hiding (Word)

import EVM
import EVM.ABI
import EVM.Dapp
import EVM.Debug (srcMapCodePos)
import EVM.Exec
import EVM.Format
import EVM.Keccak
import EVM.Solidity
import EVM.Types
import EVM.Machine (blob, w256, forceConcreteBlob)
import EVM.Concrete (Concrete, Blob (B), wordAt)

import qualified EVM.FeeSchedule as FeeSchedule

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import Control.Lens hiding (Indexed)
import Control.Monad.State.Strict hiding (state)
import qualified Control.Monad.State.Strict as State

import Control.Monad.Par.Class (spawn_)
import Control.Monad.Par.IO (runParIO)

import Data.ByteString    (ByteString)
import Data.Foldable      (toList)
import Data.Map           (Map)
import Data.Maybe         (fromMaybe, catMaybes, fromJust, mapMaybe)
import Data.Monoid        ((<>))
import Data.Text          (Text, unpack, isPrefixOf, stripSuffix, intercalate)
import Data.Text.Encoding (encodeUtf8)
import Data.Word          (Word32)
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

data UnitTestOptions = UnitTestOptions
  { gasForCreating :: W256
  , gasForInvoking :: W256
  , balanceForCreator :: W256
  , balanceForCreated :: W256
  , oracle :: Query Concrete -> IO (EVM Concrete ())
  , verbose :: Bool
  , vmModifier :: VM Concrete -> VM Concrete
  }

defaultGasForCreating :: W256
defaultGasForCreating = 0xffffffffffff

defaultGasForInvoking :: W256
defaultGasForInvoking = 0xffffffffffff

defaultBalanceForCreator :: W256
defaultBalanceForCreator = 0xffffffffffffffffffffffff

defaultBalanceForCreated :: W256
defaultBalanceForCreated = 0xffffffffffffffffffffffff

type ABIMethod = Text

-- | Assuming a constructor is loaded, this stepper will run the constructor
-- to create the test contract, give it an initial balance, and run `setUp()'.
initializeUnitTest :: UnitTestOptions -> Stepper Concrete ()
initializeUnitTest UnitTestOptions { .. } = do

  -- Maybe modify the initial VM, e.g. to load library code
  Stepper.evm (modify vmModifier)

  -- Make a trace entry for running the constructor
  Stepper.evm (pushTrace (EntryTrace "constructor"))

  -- Constructor is loaded; run until it returns code
  B bytes <- Stepper.execFullyOrFail
  addr <- Stepper.evm (use (state . contract))

  -- Mutate the current contract to use the new code
  Stepper.evm $ replaceCodeOfSelf bytes

  -- Give a balance to the test target
  Stepper.evm $ env . contracts . ix addr . balance += w256 balanceForCreated

  -- Initialize the test contract
  Stepper.evm (popTrace >> pushTrace (EntryTrace "initialize test"))
  Stepper.evm $ setupCall addr "setUp()" gasForInvoking

  Stepper.note "Running `setUp()'"

  -- Let `setUp()' run to completion
  void Stepper.execFullyOrFail
  Stepper.evm popTrace

-- | Assuming a test contract is loaded and initialized, this stepper
-- will run the specified test method and return whether it succeeded.
runUnitTest :: UnitTestOptions -> ABIMethod -> Stepper Concrete Bool
runUnitTest UnitTestOptions { .. } method = do

  -- Decide whether the test is supposed to fail or succeed
  let shouldFail = "testFail" `isPrefixOf` method

  -- The test subject should be loaded and initialized already
  addr <- Stepper.evm $ use (state . contract)

  -- Set up the call to the test method
  Stepper.evm $ setupCall addr method gasForInvoking
  Stepper.evm (pushTrace (EntryTrace method))
  Stepper.note "Running unit test"

  -- Try running the test method
  bailed <-
    Stepper.execFully >>=
      either (const (pure True)) (const (pure False))

  -- If we failed, put the error in the trace.
  -- It's not clear to me right now why this doesn't happen somewhere else.
  Just problem <- Stepper.evm $ use result
  case problem of
    VMFailure e ->
      Stepper.evm (pushTrace (ErrorTrace e))
    _ ->
      pure ()

  -- Ask whether any assertions failed
  Stepper.evm $ popTrace
  Stepper.evm $ setupCall addr "failed()" 10000
  Stepper.note "Checking whether assertions failed"
  AbiBool failed <- Stepper.execFullyOrFail >>= Stepper.decode AbiBoolType

  -- Return true if the test was successful
  pure (shouldFail == (bailed || failed))

tick :: Text -> IO ()
tick x = Text.putStr x >> hFlush stdout

interpret
  :: UnitTestOptions
  -> Stepper Concrete a
  -> StateT (VM Concrete) IO (Either (Stepper.Failure Concrete) a)
interpret opts =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView (Stepper.Action Concrete) a
      -> StateT (VM Concrete) IO (Either (Stepper.Failure Concrete) a)

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

type CoverageState = (VM Concrete, MultiSet OpLocation)

currentOpLocation :: VM Concrete -> OpLocation
currentOpLocation vm =
  case currentContract vm of
    Nothing ->
      error "internal error: why no contract?"
    Just c ->
      OpLocation
        (view codehash c)
        (fromJust (vmOpIx vm))

execWithCoverage :: StateT CoverageState IO (VMResult Concrete)
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
  -> Stepper Concrete a
  -> StateT CoverageState IO (Either (Stepper.Failure Concrete) a)
interpretWithCoverage opts =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView (Stepper.Action Concrete) a
      -> StateT CoverageState IO (Either (Stepper.Failure Concrete) a)

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
    f name xs =
      Vector.imap
        (\i bs ->
           let
             n =
               if Set.member (name, i + 1) allPositions
               then MultiSet.occur (name, i + 1) srcMapCov
               else -1
           in (n, bs))
        xs
  in
    Map.mapWithKey f linesByName

coverageForUnitTestContract
  :: UnitTestOptions
  -> Map Text SolcContract
  -> SourceCache
  -> (Text, [Text])
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
        runOne testName = spawn_ . liftIO $ do
          (x, (_, cov)) <-
            runStateT
              (interpretWithCoverage opts (runUnitTest opts testName))
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

runUnitTestContract ::
  UnitTestOptions -> Map Text SolcContract -> SourceCache -> (Text, [Text]) -> IO Bool
runUnitTestContract
  opts@(UnitTestOptions {..}) contractMap sources (name, testNames) = do

  -- Print a header
  putStrLn $ "Running " ++ show (length testNames) ++ " tests for "
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

      -- Define the thread spawner for test cases
      let
        runOne testName = spawn_ . liftIO $ do
          x <-
            runStateT
              (interpret opts (runUnitTest opts testName))
              vm1
          case x of
             (Right True,  vm) -> pure (".", Right (passOutput vm testName))
             (Right False, vm) -> pure ("F", Left (failOutput vm dapp testName))
             (Left _, _)       -> pure ("E", Left "\n")

      -- Run all the test cases in parallel and print their status updates
      details <-
        runParIO (mapM runOne testNames >>= mapM Par.get)
          >>= mapM (\(x, y) -> tick x >> pure y)

      let fails = [x | Left x <- details]

      tick "\n\n"
      tick (Text.unlines fails)

      pure (null fails)

indentLines :: Int -> Text -> Text
indentLines n s =
  let p = Text.replicate n " "
  in Text.unlines (map (p <>) (Text.lines s))

passOutput :: VM Concrete -> Text -> Text
passOutput _ testName = "[PASS] " <> testName

failOutput :: VM Concrete -> DappInfo -> Text -> Text
failOutput vm dapp testName = mconcat $
  [ "[FAIL] "
  , fromMaybe "" (stripSuffix "()" testName)
  , "\n"
  , indentLines 2 (formatTestLogs (view dappEventMap dapp) (view logs vm))
  , indentLines 2 (showTraceTree dapp vm)
  , "\n"
  ]

formatTestLogs :: Map W256 Event -> Seq.Seq (Log Concrete) -> Text
formatTestLogs events xs =
  case catMaybes (toList (fmap (formatTestLog events) xs)) of
    [] -> "\n"
    ys -> "\n" <> intercalate "\n" ys <> "\n\n"

formatTestLog :: Map W256 Event -> Log Concrete -> Maybe Text
formatTestLog _ (Log _ _ []) = Nothing
formatTestLog events (Log _ b (t:_)) =
  let
    name  = getEventName event
    args  = forceConcreteBlob b
    event = getEvent t events

  in case name of

    "log_bytes32" ->
      Just $ formatBytes args

    "log_named_bytes32" ->
      let key = BS.take 32 args
          val = BS.drop 32 args
      in Just $ formatString key <> ": " <> formatBytes val

    "log_named_address" ->
      let key = BS.take 32 args
          val = BS.drop 44 args
      in Just $ formatString key <> ": " <> formatBinary val

  -- TODO: event log_named_decimal_int  (bytes32 key, int val, uint decimals);
  -- TODO: event log_named_decimal_uint (bytes32 key, uint val, uint decimals);

    "log_named_int" ->
      let key = BS.take 32 args
          val = wordAt 32 args
      in Just $ formatString key <> ": " <> showDec Signed val

    "log_named_uint" ->
      let key = BS.take 32 args
          val = wordAt 32 args
      in Just $ formatString key <> ": " <> showDec Unsigned val

    _ ->
      Nothing

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

setupCall :: Addr -> Text -> W256 -> EVM Concrete ()
setupCall target abi allowance = do
  resetState
  loadContract target
  assign (state . calldata) (blob (word32Bytes (abiKeccak (encodeUtf8 abi))))
  assign (state . gas) (w256 allowance)

initialUnitTestVm :: UnitTestOptions -> SolcContract -> [SolcContract] -> VM Concrete
initialUnitTestVm (UnitTestOptions {..}) theContract _ =
  let
    vm = makeVm $ VMOpts
           { vmoptCode = view creationCode theContract
           , vmoptCalldata = ""
           , vmoptValue = 0
           , vmoptAddress = newContractAddress ethrunAddress 1
           , vmoptCaller = ethrunAddress
           , vmoptOrigin = ethrunAddress
           , vmoptGas = gasForCreating
           , vmoptCoinbase = 0
           , vmoptNumber = 0
           , vmoptTimestamp = 1
           , vmoptGaslimit = 0
           , vmoptGasprice = 0
           , vmoptDifficulty = 0
           , vmoptSchedule = FeeSchedule.metropolis
           }
    creator =
      initialContract mempty
        & set nonce 1
        & set balance (w256 balanceForCreator)
  in vm
    & set (env . contracts . at ethrunAddress) (Just creator)
