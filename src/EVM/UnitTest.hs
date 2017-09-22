{-# LANGUAGE ViewPatterns #-}

module EVM.UnitTest where

import Prelude hiding (Word)

import EVM
import EVM.ABI
import EVM.Exec
import EVM.Keccak
import EVM.Solidity
import EVM.Types
import EVM.Machine (blob, w256, forceConcreteBlob, forceConcreteWord)
import EVM.Concrete (Concrete, Blob (B), Word (C), wordAt)

import qualified EVM.FeeSchedule as FeeSchedule

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import Control.Lens
import Control.Monad.State.Strict hiding (state)
import qualified Control.Monad.State.Strict as State

import Control.Monad.Par.Class (spawn_)
import Control.Monad.Par.IO (runParIO)

import Data.Binary.Get    (runGetOrFail)
import Data.ByteString    (ByteString)
import Data.ByteString.Builder    (byteStringHex, toLazyByteString)
import Data.Foldable      (toList)
import Data.List          (intercalate, sort)
import Data.Map           (Map)
import Data.Text          (Text, unpack, isPrefixOf, stripSuffix)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Word          (Word32)
import Numeric            (showHex)
import System.IO          (hFlush, stdout)

import qualified Control.Monad.Par.Class as Par
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Data.Sequence as Seq
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data UnitTestOptions = UnitTestOptions
  { gasForCreating :: W256
  , gasForInvoking :: W256
  , balanceForCreator :: W256
  , balanceForCreated :: W256
  , oracle :: Query Concrete -> IO (EVM Concrete ())
  , verbose :: Bool
  }

type ABIMethod = Text

-- | Assuming a constructor is loaded, this stepper will run the constructor
-- to create the test contract, give it an initial balance, and run `setUp()'.
initializeUnitTest :: UnitTestOptions -> Stepper Concrete ()
initializeUnitTest UnitTestOptions { .. } = do

  -- Constructor is loaded; run until it returns code
  B bytes <- Stepper.execFullyOrFail
  addr <- Stepper.evm (use (state . contract))

  -- Mutate the current contract to use the new code
  Stepper.evm $ replaceCodeOfSelf bytes

  -- Give a balance to the test target
  Stepper.evm $ env . contracts . ix addr . balance += w256 balanceForCreated

  -- Initialize the test contract
  Stepper.evm $ setupCall addr "setUp()" gasForInvoking

  Stepper.note "Running `setUp()'"

  -- Let `setUp()' run to completion
  void Stepper.execFullyOrFail

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
  Stepper.note "Running unit test"

  -- Try running the test method
  bailed <-
    Stepper.execFully >>=
      either (const (pure True)) (const (pure False))

  -- Ask whether any assertions failed
  Stepper.evm $ setupCall addr "failed()" 10000
  Stepper.note "Checking whether assertions failed"
  AbiBool failed <- Stepper.execFullyOrFail >>= Stepper.decode AbiBoolType

  -- Return true if the test was successful
  pure (shouldFail == (bailed || failed))

tick :: String -> IO ()
tick x = putStr x >> hFlush stdout

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


runUnitTestContract ::
  UnitTestOptions -> Map Text SolcContract -> SourceCache -> (Text, [Text]) -> IO ()
runUnitTestContract opts@(UnitTestOptions {..}) contractMap _ (name, testNames) = do
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
      let vm0 = initialUnitTestVm opts theContract (Map.elems contractMap)
      vm1 <- execStateT (interpret opts (initializeUnitTest opts)) vm0

      -- Define the thread spawner for test cases
      let
        runOne testName = spawn_ . liftIO $
          runStateT (interpret opts (runUnitTest opts testName)) vm1 >>=
           \case
             (Right True,  vm) -> pure (".", passOutput vm testName theContract)
             (Right False, vm) -> pure ("F", failOutput vm testName theContract)
             (Left _, _)       -> pure ("E", "\n")

      -- Run all the test cases in parallel and print their status updates
      details <- runParIO (mapM runOne testNames >>= mapM Par.get) >>= mapM ticktock

      tick "\n"

      if verbose then do
        tick "\n"
        tick (intercalate "\n" details)
        tick "\n"
      else pure ()

ticktock :: (String, String) -> IO String
ticktock (x, y) = do tick x
                     pure y

showDec :: W256 -> String
showDec (W256 w) =
  if w == num cheatCode
  then "<hevm cheat address>"
  else
    if w > 1000000000000
    then
      "~" ++ Scientific.formatScientific
         Scientific.Generic
         (Just 8)
         (fromIntegral w)
    else
      showDecExact (W256 w)

showDecExact :: W256 -> String
showDecExact (W256 w) = unpack (humanizeInteger w)

showWordExact :: Word Concrete -> Text
showWordExact (C _ (W256 w)) = humanizeInteger w

humanizeInteger :: (Num a, Integral a, Show a) => a -> Text
humanizeInteger =
  ( Text.intercalate ","
  . reverse
  . map Text.reverse
  . Text.chunksOf 3
  . Text.reverse
  . Text.pack
  . show
  )

unpackm :: Maybe Text -> String
unpackm (Just text) = unpack text
unpackm Nothing     = ""

passOutput :: VM Concrete -> Text -> SolcContract -> String
passOutput _ testName _ = "[PASS] " ++ unpack testName ++ "\n"

failOutput :: VM Concrete -> Text -> SolcContract -> String
failOutput vm testName theContract =
  "[FAIL] " ++ unpackm (stripSuffix "()" testName) ++ "\n"
  ++ formatTestLogs (view eventMap theContract) (view logs vm)
  -- TODO: ++ drawForest (fmap (fmap (formatTrace (view eventMap theContract))) (contextTraceForest vm))

formatTestLogs :: Map W256 Event -> Seq.Seq (Log Concrete) -> String
formatTestLogs events xs =
  intercalate "\n" $ toList $ fmap (formatTestLog events) xs

formatTestLog :: Map W256 Event -> Log Concrete -> String
formatTestLog _ (Log _ _ [])    = ""
formatTestLog events (Log _ b (t:_))
  | name == "log_bytes32" = formatBytes args
  | name == "log_named_bytes32" = let key = BS.take 32 args
                                      val = BS.drop 32 args
                                  in formatString key ++ ": " ++ formatBytes val
  | name == "log_named_address" = let key = BS.take 32 args
                                      val = BS.drop 44 args
                                  in formatString key ++ ": " ++ formatBinary val
  -- TODO: event log_named_decimal_int  (bytes32 key, int val, uint decimals);
  -- TODO: event log_named_decimal_uint (bytes32 key, uint val, uint decimals);
  | name == "log_named_int" ||
    name == "log_named_uint"  = let key = BS.take 32 args
                                    val = wordAt 32 args
                                in formatString key ++ ": " ++ showDec val
  | otherwise = formatLog event args
  where name  = getEventName event
        args  = forceConcreteBlob b
        event = getEvent t events

-- TODO: formatTrace equivalent (via printFailure), except allow show trace
-- when passing as well
formatTrace :: Map W256 Event -> Either (Log Concrete) (FrameContext Concrete) -> String
formatTrace _ trace =
  case trace of
    -- TODO: use formatLog and don't drop tail
    Left _ -> ""
    Right _ -> ""


-- TODO: this should take Log
formatLog :: Maybe Event -> ByteString -> String
formatLog event args =
  let types = getEventTypes event
      name  = getEventName event
  in
  case runGetOrFail (getAbiSeq (length types) types)
                      (LazyByteString.fromStrict args) of
                    Right (_, _, abivals) -> "\x1b[36m←" ++ name
                                            ++ showAbiValues abivals
                                            ++ "\x1b[0m"
                    Left (_,_,_) -> error "lol"

-- lol, i guess
toStrict :: LazyByteString.ByteString -> BS.ByteString
toStrict = BS.concat . LazyByteString.toChunks

showAbiValues :: Vector.Vector AbiValue -> String
showAbiValues vs = "(" ++ intercalate ", " (Vector.toList (Vector.map showAbiValue vs)) ++ ")"

showAbiValue :: AbiValue -> String
showAbiValue (AbiUInt _ w)        = unpack $ humanizeInteger w
showAbiValue (AbiInt _ w)         = unpack $ humanizeInteger w
showAbiValue (AbiAddress w160)    = "0x" ++ (showHex w160 "")
showAbiValue (AbiBool b)          = show b
showAbiValue (AbiBytes _ bs)      = formatBytes bs
showAbiValue (AbiBytesDynamic bs) = formatBinary bs
showAbiValue (AbiString bs)       = formatQString bs
-- TODO: arrays
showAbiValue value = show value

isPrintable :: ByteString -> Bool
isPrintable _ = True  -- FIXME: should check if all ascii chars

-- TODO: make all these W256 -> String?
formatBytes :: ByteString -> String
formatBytes b | isPrintable b = formatQString b
              | otherwise     = formatBinary b

formatQString :: ByteString -> String
formatQString s = "\"" ++ (formatString s) ++ "\""

formatString :: ByteString -> String
formatString = unpack . decodeUtf8

formatBinary :: ByteString -> String
formatBinary = (++) "0x" . unpack . decodeUtf8 . toStrict . toLazyByteString . byteStringHex

getEvent :: EVM.Concrete.Word Concrete -> Map W256 Event -> Maybe Event
getEvent w events = Map.lookup (forceConcreteWord w) events

getEventName :: Maybe Event -> String
getEventName (Just (Event name _ _)) = unpack name
getEventName Nothing = ""

getEventTypes :: Maybe Event -> [AbiType]
getEventTypes Nothing = []
getEventTypes (Just (Event _ _ xs)) = map fst xs

getEventArgs :: Blob Concrete -> String
getEventArgs b = formatBlob b

formatBlob :: Blob Concrete -> String
formatBlob b = unpack $ decodeUtf8 $ forceConcreteBlob b

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
