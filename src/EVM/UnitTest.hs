module EVM.UnitTest where

import EVM
import EVM.ABI
import EVM.Debug
import EVM.Exec
import EVM.Keccak
import EVM.Solidity
import EVM.Types

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict hiding (state)

import Data.Binary.Get    (runGetOrFail)
import Data.Text          (Text, unpack, isPrefixOf)
import Data.Text.Encoding (encodeUtf8)
import Data.Map           (Map)
import Data.Word          (Word32)
import Data.List          (sort)
import IPPrint.Colored    (cpprint)
import System.IO          (hFlush, stdout)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as LazyByteString

runUnitTestContract ::
  Mode -> Map Text SolcContract -> SourceCache -> (Text, [Text]) -> IO ()
runUnitTestContract mode contractMap cache (contractName, testNames) = do
  putStrLn $ "Running " ++ show (length testNames) ++ " tests for "
    ++ unpack contractName
  case preview (ix contractName) contractMap of
    Nothing ->
      error $ "Contract " ++ unpack contractName ++ " not found"
    Just theContract -> do
      let
        vm0 = initialUnitTestVm theContract (Map.elems contractMap)
        vm2 = case runState exec vm0 of
                (VMRunning, _) ->
                  error "Internal error"
                (VMFailure, _) ->
                  error "Creation error"
                (VMSuccess targetCode, vm1) -> do
                  execState (performCreation targetCode) vm1
        target = view (state . contract) vm2

      forM_ testNames $ \testName -> do
        putStr $ unpack testName ++ ": "
        hFlush stdout
        let vm3 = flip execState vm2 $ do
              setupCall target "setUp()"
              VMSuccess _ <- exec
              setupCall target testName

        let vm4 = execState exec vm3
        case vm4 ^. result of
          VMFailure ->
            if "testFail" `isPrefixOf` testName
              then putStrLn "ok (failed as expected)"
              else do
                putStrLn "unexpected failure"
                _ <- debugger (Just cache) vm3
                return ()
          VMSuccess _ -> do
            case evalState (setupCall target "failed()" >> exec) vm4 of
              VMSuccess out ->
                case runGetOrFail (getAbi AbiBoolType)
                       (LazyByteString.fromStrict out) of
                  Right (_, _, AbiBool False) ->
                    putStrLn "ok"
                  Right (_, _, AbiBool True) ->
                    putStrLn "test failed"
                  Right (_, _, _) ->
                    error "internal error"
                  Left (_, _, e) ->
                    error ("ds-test behaving strangely: " ++ e)
              VMFailure ->
                error "ds-test behaving strangely"
              _ ->
                error "internal error"
          VMRunning ->
            error "internal error"

setupCall :: Addr -> Text -> EVM ()
setupCall target abi = do
  resetState
  loadContract target
  assign (state . calldata) (word32Bytes (abiKeccak (encodeUtf8 abi)))

initialUnitTestVm :: SolcContract -> [SolcContract] -> VM
initialUnitTestVm c theContracts =
  let
    vm = makeVm $ VMOpts
           { vmoptCode = view creationCode c
           , vmoptCalldata = ""
           , vmoptValue = 0
           , vmoptAddress = newContractAddress ethrunAddress 1
           , vmoptCaller = ethrunAddress
           , vmoptOrigin = ethrunAddress
           , vmoptCoinbase = 0
           , vmoptNumber = 0
           , vmoptTimestamp = 0
           , vmoptGaslimit = 0
           , vmoptDifficulty = 0
           }
    creator = initialContract mempty & set nonce 1
  in vm
    & set (env . contracts . at ethrunAddress) (Just creator)
    & set (env . solc) (Map.fromList [(view solcCodehash c, c) | c <- theContracts])

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
      "test" `isPrefixOf` s -- || "testFail" `isPrefixOf` s
