-- Main file of the hsevm CLI program

{-# Language CPP #-}
{-# Language BangPatterns #-}
{-# Language DeriveGeneric #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language TemplateHaskell #-}

import qualified EVM as EVM

#if MIN_VERSION_aeson(1, 0, 0)
import qualified EVM.VMTest as VMTest
#endif

import EVM.Debug
import EVM.Exec
import EVM.Keccak
import EVM.Solidity
import EVM.Types

import Control.Lens
import Control.Monad (unless)
import Control.Monad.State.Strict

import Data.List (intercalate, sort)
import Data.Text (Text, isPrefixOf, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word32)
import Data.Map (Map)

-- import Data.List.NonEmpty (NonEmpty)
-- import qualified Data.List.NonEmpty as NonEmpty

import IPPrint.Colored (cpprint)
import Options.Generic

import System.IO
import System.Directory (withCurrentDirectory)

import Data.ByteString (ByteString)

import qualified Data.ByteString.Lazy  as LazyByteString
import qualified Data.ByteString       as ByteString
import qualified Data.Map              as Map
import qualified Data.Text             as Text

-- This record defines the program's command-line options
-- automatically via the `optparse-generic` package.
data Command
  = Exec
      { code       :: ByteString
      , trace      :: Bool
      , calldata   :: Maybe ByteString
      , address    :: Maybe Addr
      , caller     :: Maybe Addr
      , origin     :: Maybe Addr
      , coinbase   :: Maybe Addr
      , value      :: Maybe W256
      , number     :: Maybe W256
      , timestamp  :: Maybe W256
      , gaslimit   :: Maybe W256
      , difficulty :: Maybe W256
      , debug      :: Bool
      }
  | DappTest
      { jsonfile :: String
      , srcroot  :: String
      , debug    :: Bool
      }
  | VMTest
      { file  :: String
      , test  :: [String]
      , debug :: Bool
      }
  deriving (Show, Generic, Eq)

instance ParseRecord Command

data Mode = Debug | Run

optsMode :: Command -> Mode
optsMode x = if debug x then Debug else Run

main :: IO ()
main = do
  opts <- getRecord "hsevm -- Ethereum evaluator"
  case opts of
    Exec {} ->
      launchExec opts
    VMTest {} ->
      launchVMTest opts
    DappTest {} ->
      withCurrentDirectory (srcroot opts) $
        dappTest (optsMode opts) (jsonfile opts)

dappTest :: Mode -> String -> IO ()
dappTest mode solcFile = do
  readSolc solcFile >>=
    \case
      Just (contractMap, cache) -> do
        let unitTests = findUnitTests (Map.elems contractMap)
        mapM_ (runUnitTestContract mode contractMap cache) unitTests
      Nothing ->
        error ("Failed to read Solidity JSON for `" ++ solcFile ++ "'")

runUnitTestContract ::
  Mode -> Map Text SolcContract -> SourceCache -> (Text, [Text]) -> IO ()
runUnitTestContract mode contractMap cache (contractName, testNames) = do
  putStrLn $ "Running " ++ show (length testNames) ++ " tests for "
    ++ unpack contractName
  case preview (ix contractName) contractMap of
    Nothing ->
      error $ "Contract " ++ unpack contractName ++ " not found"
    Just contract -> do
      let
        vm0 = initialUnitTestVm contract (Map.elems contractMap)
        vm2 = case runState exec vm0 of
                (EVM.VMRunning, _) ->
                  error "Internal error"
                (EVM.VMFailure, vm1) ->
                  error "Creation error"
                (EVM.VMSuccess targetCode, vm1) -> do
                  execState (EVM.performCreation targetCode) vm1
        target = view (EVM.state . EVM.contract) vm2

      forM_ testNames $ \testName -> do
        cpprint ("debugging", testName)
        let vm3 = flip execState vm2 $ do
              EVM.resetState
              EVM.loadContract target
              assign (EVM.state . EVM.caller) ethrunAddress
              assign (EVM.state . EVM.calldata) (EVM.word32Bytes (abiKeccak (encodeUtf8 "setUp()")))
              EVM.VMSuccess _ <- exec
              EVM.resetState
              EVM.loadContract target
              assign (EVM.state . EVM.caller) ethrunAddress
              assign (EVM.state . EVM.calldata) (EVM.word32Bytes (abiKeccak (encodeUtf8 testName)))
              exec
        -- debugger (Just cache) vm3

        --       exec
        cpprint (vm3 ^. EVM.result)
        -- case runState exec vm4 of
        --   (EVM.VMRunning, _) ->
        --     error "Internal error"
        --   (EVM.VMFailure, vm5) ->
        --     cpprint ("failure", testName)
        --   (EVM.VMSuccess _, vm5) ->
        --     cpprint ("success", testName)

initialUnitTestVm :: SolcContract -> [SolcContract] -> EVM.VM
initialUnitTestVm c contracts =
  let
    vm = EVM.makeVm $ EVM.VMOpts
           { EVM.vmoptCode = view creationCode c
           , EVM.vmoptCalldata = ""
           , EVM.vmoptValue = 0
           , EVM.vmoptAddress = newContractAddress ethrunAddress 1
           , EVM.vmoptCaller = ethrunAddress
           , EVM.vmoptOrigin = ethrunAddress
           , EVM.vmoptCoinbase = 0
           , EVM.vmoptNumber = 0
           , EVM.vmoptTimestamp = 0
           , EVM.vmoptGaslimit = 0
           , EVM.vmoptDifficulty = 0
           }
    creator = EVM.initialContract mempty & set EVM.nonce 1
  in vm
    & set (EVM.env . EVM.contracts . at ethrunAddress) (Just creator)
    & set (EVM.env . EVM.solc) (Map.fromList [(view solcCodehash c, c) | c <- contracts])

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

-- ethrun :: Mode -> NonEmpty String -> [ByteString] -> IO ()
-- ethrun mode (codefile NonEmpty.:| codefiles) calldatas = do
--   creationHexCode <- ByteString.readFile codefile
--   let creationCode = hexByteString codefile (creationHexCode)
--   case runState exec (vmForEthrunCreation creationCode) of
--     (EVM.VMSuccess targetCode, vm1) -> do
--       let target = view (EVM.state . EVM.contract) vm1
--       doEthrunTransactions mode
--         (execState (EVM.performCreation targetCode) vm1)
--         target targetCode calldatas
--     (EVM.VMFailure, vm) -> do
--       cpprint ("creation failure", vm)
--     (EVM.VMRunning, _) ->
--       error "internal error"

executeOrDebug :: Mode -> EVM.VM -> IO EVM.VM
executeOrDebug Run   = return . execState exec
executeOrDebug Debug = debugger Nothing

doEthrunTransactions ::
  Mode -> EVM.VM -> Addr -> ByteString -> [ByteString] -> IO ()
doEthrunTransactions _ _ _ _ [] = return ()
doEthrunTransactions mode vm target targetCode (targetData:xs) = do
  vm' <- executeOrDebug mode . flip execState vm $ do
    EVM.resetState
    EVM.loadContract target
    assign (EVM.state . EVM.calldata) targetData
    assign (EVM.state . EVM.caller)   ethrunAddress

  case view EVM.result vm' of
    EVM.VMSuccess out -> do
      cpprint (out, vm')
      doEthrunTransactions mode vm' target targetCode xs
    EVM.VMFailure ->
      cpprint ("VM failure", vm')
    EVM.VMRunning ->
      error "internal error"

launchExec :: Command -> IO ()
launchExec opts =
  let vm = vmFromCommand opts in
    case optsMode opts of
      Run -> print (execState exec vm)
      Debug -> ignore (debugger Nothing vm)

vmFromCommand :: Command -> EVM.VM
vmFromCommand opts =
  EVM.makeVm $ EVM.VMOpts
    { EVM.vmoptCode       = hexByteString "--code" (code opts)
    , EVM.vmoptCalldata   = maybe "" (hexByteString "--calldata")
                              (calldata opts)
    , EVM.vmoptValue      = word value 0
    , EVM.vmoptAddress    = addr address 1
    , EVM.vmoptCaller     = addr caller 2
    , EVM.vmoptOrigin     = addr origin 3
    , EVM.vmoptCoinbase   = addr coinbase 0
    , EVM.vmoptNumber     = word number 0
    , EVM.vmoptTimestamp  = word timestamp 0
    , EVM.vmoptGaslimit   = word gaslimit 0
    , EVM.vmoptDifficulty = word difficulty 0
    }
  where
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
      do _ <- debugger Nothing vm
         return True -- XXX
#endif

ignore :: Monad m => m a -> m ()
ignore x = x >> return ()
