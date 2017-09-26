module EVM.UnitTest where

import EVM
import EVM.ABI
import EVM.Exec
import EVM.Keccak
import EVM.Solidity
import EVM.Types
import EVM.Machine (blob, w256)
import EVM.Concrete (Concrete, Blob (B))
import EVM.Fetch (testFetchContract, testFetchSlot)

import qualified EVM.FeeSchedule as FeeSchedule

import Control.Lens
import Control.Monad.State.Strict hiding (state)

import Control.Monad.Par.Class (spawn_)
import Control.Monad.Par.IO (runParIO)

import Data.Binary.Get    (runGetOrFail)
import Data.ByteString    (ByteString)
import Data.List          (sort)
import Data.Map           (Map)
import Data.Text          (Text, unpack, isPrefixOf)
import Data.Text.Encoding (encodeUtf8)
import Data.Word          (Word32)
import System.IO          (hFlush, stdout)

import qualified Control.Monad.Par.Class as Par
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map

data UnitTestOptions = UnitTestOptions
  { gasForCreating :: W256
  , gasForInvoking :: W256
  , balanceForCreator :: W256
  , balanceForCreated :: W256
  , oracle :: Query Concrete -> IO (EVM Concrete ())
  , onFailure :: VM Concrete -> IO ()
  }

tick :: String -> IO ()
tick x = putStr x >> hFlush stdout

httpOracle :: Query Concrete -> IO (EVM Concrete ())
httpOracle q = do
  case q of
    PleaseFetchContract addr continue ->
      testFetchContract addr >>= \case
        Just x  -> do
          return (continue x)
        Nothing -> error ("oracle error: " ++ show q)
    PleaseFetchSlot addr slot continue ->
      testFetchSlot addr (num slot) >>= \case
        Just x  -> return (continue x)
        Nothing -> error ("oracle error: " ++ show q)

zeroOracle :: Monad m => Query Concrete -> m (EVM Concrete ())
zeroOracle q = do
  case q of
    PleaseFetchContract _ continue ->
      return (continue (initialContract ""))
    PleaseFetchSlot _ _ continue ->
      return (continue 0)

runUnitTestContract ::
  UnitTestOptions -> Map Text SolcContract -> SourceCache -> (Text, [Text]) -> IO ()
runUnitTestContract opts@(UnitTestOptions {..}) contractMap _ (name, testNames) = do
  -- Define the driver that executes a VM and
  -- answers its queries until completion.
  let
    oracular
      :: VM Concrete
      -> IO (VMResult Concrete, VM Concrete)
    oracular vm = do
      case runState exec vm of
        (VMFailure (Query q), vm') ->
          do m <- oracle q
             oracular (execState m vm')
        x -> return x

  -- Print a header
  putStrLn $ "Running " ++ show (length testNames) ++ " tests for "
    ++ unpack name

  -- look for the wanted contract by name from the Solidity info
  case preview (ix name) contractMap of
    Nothing ->
      -- Fail if there's no such contract
      error $ "Contract " ++ unpack name ++ " not found"

    Just theContract -> do
      -- Construct the initial VM and begin the contract's constructor
      let
        vm0 = initialUnitTestVm opts theContract (Map.elems contractMap)

      -- Run the constructor and use the constructed code, emulating CREATE
      vm2 <-
        oracular vm0 >>= \case
          (VMFailure e, _) ->
            error ("Creation error: " ++ show e)
          (VMSuccess (B targetCode), vm1) -> do
            return (execState (replaceCodeOfSelf targetCode) vm1)

      -- Provide the requested balance for the test contract
      let
        target = view (state . contract) vm2
        vm3 = vm2 & env . contracts . ix target . balance +~ w256 balanceForCreated

      -- Run the `setUp()' method, and then run all test methods in parallel
      oracular (execState (setupCall target "setUp()"  gasForInvoking) vm3) >>= \case
        (VMFailure e, _) -> do
          putStrLn ("error in setUp(): " ++ show e)
        (VMSuccess _, vm4) -> do
          let
            -- Define the thread spawner for test cases
            runOne testName = spawn_ . liftIO $ do

              -- Thread will first the test case method
              oracular (execState (setupCall target testName gasForInvoking) vm4) >>= \case
                (VMFailure _, vm5) ->
                  -- On failure, decide whether failure is expected, fail otherwise
                  if "testFail" `isPrefixOf` testName
                    then return "."
                    else do
                      onFailure vm5
                      return "F"

                (VMSuccess _, vm5) -> do
                  -- On success, call the `failed()' method to check assertions
                  oracular (execState (setupCall target "failed()" 10000) vm5) >>= \case
                    (VMSuccess (B out), _) ->
                      -- Decode the boolean return value using the ABI
                      case runGetOrFail (getAbi AbiBoolType)
                             (LazyByteString.fromStrict out) of
                        Right (_, _, AbiBool False) ->
                          return "."
                        Right (_, _, AbiBool True) ->
                          -- Decide whether failure is expected, fail otherwise
                          if "testFail" `isPrefixOf` testName
                            then return "."
                            else do
                              return "F"

                        -- The ABI decoder never returns wrong value type
                        Right (_, _, _) ->
                          error "internal error"

                        -- Calling `failed()' returned invalid boolean
                        Left (_, _, e) ->
                          error ("ds-test behaving strangely: " ++ e)

                    -- Calling `failed()' failed
                    (VMFailure e, _) ->
                      error $ "ds-test behaving strangely (" ++ show e ++ ")"

          -- Run all the test cases in parallel and print their status updates
          runParIO (mapM runOne testNames >>= mapM Par.get) >>= mapM_ tick

          tick "\n"

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
           , vmoptDifficulty = 0
           , vmoptSchedule = FeeSchedule.metropolis
           }
    creator =
      initialContract mempty
        & set nonce 1
        & set balance (w256 balanceForCreator)
  in vm
    & set (env . contracts . at ethrunAddress) (Just creator)

unitTestMarkerAbi :: Word32
unitTestMarkerAbi = abiKeccak (encodeUtf8 "IS_TEST()")

findUnitTests :: [SolcContract] -> [(Text, [Text])]
findUnitTests = concatMap f where
  f c =
    case c ^? abiMap . ix unitTestMarkerAbi of
      Nothing -> []
      Just _  ->
        let testNames = unitTestMethods c
        in if null testNames
           then []
           else [(view contractName c, testNames)]

unitTestMethods :: SolcContract -> [Text]
unitTestMethods c = sort (filter (isUnitTestName) (Map.elems (c ^. abiMap)))
  where
    isUnitTestName s =
      "test" `isPrefixOf` s
