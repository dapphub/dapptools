{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.VMTest
  ( Case
#if MIN_VERSION_aeson(1, 0, 0)
  , parseSuite
  , parseBCSuite
#endif
  , vmForCase
  , checkExpectation
  , interpret
  ) where

import qualified EVM
import qualified EVM.Concrete as EVM
import qualified EVM.Exec
import qualified EVM.FeeSchedule
import qualified EVM.Stepper as Stepper
import qualified EVM.Fetch as Fetch

import Control.Monad.State.Strict (runState, join)
import qualified Control.Monad.Operational as Operational
import qualified Control.Monad.State.Class as State

import EVM (EVM, forceLitBytes)
import EVM.Stepper (Stepper)
import EVM.Transaction
import EVM.Types

import Control.Arrow ((***), (&&&))
import Control.Lens
import Control.Monad

import IPPrint.Colored (cpprint)

import Data.ByteString (ByteString)
import Data.Aeson ((.:), (.:?), FromJSON (..))
import Data.Bifunctor (bimap)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Witherable (Filterable, catMaybes)

import qualified Data.Map          as Map
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy  as Lazy

data Which = Pre | Post

data Block = Block
  { blockCoinbase    :: Addr
  , blockDifficulty  :: W256
  , blockGasLimit    :: W256
  , blockNumber      :: W256
  , blockTimestamp   :: W256
  , blockTxs         :: [Transaction]
  } deriving Show

data Case = Case
  { testVmOpts      :: EVM.VMOpts
  , checkContracts  :: Map Addr Contract
  , testExpectation :: Maybe Expectation
  } deriving Show

data BlockchainCase = BlockchainCase
  { blockchainBlocks  :: [Block]
  , blockchainPre     :: Map Addr Contract
  , blockchainPost    :: Map Addr Contract
  , blockchainNetwork :: String
  } deriving Show

data Contract = Contract
  { _balance :: W256
  , _code    :: EVM.ContractCode
  , _nonce   :: W256
  , _storage :: Map W256 W256
  , _create  :: Bool
  } deriving Show

data Expectation = Expectation
  { expectedOut       :: Maybe ByteString
  , expectedContracts :: Map Addr Contract
  , expectedGas       :: Maybe W256
  } deriving Show

makeLenses ''Contract

accountAt :: Addr -> Getter (Map Addr Contract) Contract
accountAt a = (at a) . (to $ fromMaybe newAccount)

touchAccount :: Addr -> Map Addr Contract -> Map Addr Contract
touchAccount a = Map.insertWith (flip const) a newAccount

newAccount :: Contract
newAccount = Contract
  { _balance = 0
  , _code    = EVM.RuntimeCode mempty
  , _nonce   = 0
  , _storage = mempty
  , _create  = False
  }

splitEithers :: (Filterable f) => f (Either a b) -> (f a, f b)
splitEithers =
  (catMaybes *** catMaybes)
  . (fmap fst &&& fmap snd)
  . (fmap (preview _Left &&& preview _Right))

checkStateFail :: Bool -> Case -> Expectation -> EVM.VM -> (Bool, Bool, Bool, Bool, Bool) -> IO Bool
checkStateFail diff x expectation vm (okState, okMoney, okNonce, okData, okCode) = do
  let
    printField :: (v -> String) -> Map Addr v -> IO ()
    printField f d = putStrLn $ Map.foldrWithKey (\k v acc ->
      acc ++ showAddrWith0x k ++ " : " ++ f v ++ "\n") "" d

    reason = map fst (filter (not . snd)
        [ ("bad-state",       okMoney || okNonce || okData  || okCode || okState)
        , ("bad-balance", not okMoney || okNonce || okData  || okCode || okState)
        , ("bad-nonce",   not okNonce || okMoney || okData  || okCode || okState)
        , ("bad-storage", not okData  || okMoney || okNonce || okCode || okState)
        , ("bad-code",    not okCode  || okMoney || okNonce || okData || okState)
        ])
    check = checkContracts x
    initial = initTx x
    expected = expectedContracts expectation
    actual = view (EVM.env . EVM.contracts . to (fmap clearZeroStorage)) vm

  putStr (unwords reason)
  when (diff && (not okState)) $ do
    putStrLn "\nCheck balance/state: "
    printField (\v -> (show . toInteger  $ _nonce v) ++ " "
                   ++ (show . toInteger  $ _balance v) ++ " "
                   ++ (show . Map.toList $ _storage v)) check
    putStrLn "\nInitial balance/state: "
    printField (\v -> (show . toInteger  $ _nonce v) ++ " "
                   ++ (show . toInteger  $ _balance v) ++ " "
                   ++ (show . Map.toList $ _storage v)) initial
    putStrLn "\nExpected balance/state: "
    printField (\v -> (show . toInteger  $ _nonce v) ++ " "
                   ++ (show . toInteger  $ _balance v) ++ " "
                   ++ (show . Map.toList $ _storage v)) expected
    putStrLn "\nActual balance/state: "
    printField (\v -> (show . toInteger  $ EVM._nonce v) ++ " "
                   ++ (show . toInteger  $ EVM._balance v) ++ " "
                   ++ (show . Map.toList $ EVM._storage v)) actual
  return okState

checkExpectation :: Bool -> EVM.ExecMode -> Case -> EVM.VM -> IO Bool
checkExpectation diff execmode x vm =
  case (execmode, testExpectation x, view EVM.result vm) of
    (EVM.ExecuteAsBlockchainTest, Just expectation, _) -> do
      let (okState, b2, b3, b4, b5) =
            checkExpectedContracts vm (expectedContracts expectation)
      _ <- if not okState then
               checkStateFail
                 diff x expectation vm (okState, b2, b3, b4, b5)
           else return True
      return okState

    (_, Just expectation, Just (EVM.VMSuccess output)) -> do
      let
        (okState, ok2, ok3, ok4, ok5) =
          checkExpectedContracts vm (expectedContracts expectation)
        (s2, b2) = ("bad-output", checkExpectedOut (forceLitBytes output) (expectedOut expectation))
        (s3, b3) = ("bad-gas", checkExpectedGas vm (expectedGas expectation))
        ss = map fst (filter (not . snd) [(s2, b2), (s3, b3)])
      _ <- if not okState then
               checkStateFail
                    diff x expectation vm (okState, ok2, ok3, ok4, ok5)
               else
                   return True
      putStr (unwords ss)
      return (okState && b2 && b3)

    (_, Nothing, Just (EVM.VMSuccess _)) -> do
      putStr "unexpected-success"
      return False

    (_, Nothing, Just (EVM.VMFailure _)) ->
      return True

    (_, Just _, Just (EVM.VMFailure _)) -> do
      putStr "unexpected-failure"
      return False

    (_, _, Nothing) -> do
      cpprint (view EVM.result vm)
      error "internal error"

checkExpectedOut :: ByteString -> Maybe ByteString -> Bool
checkExpectedOut output ex = case ex of
  Nothing       -> True
  Just expected -> output == expected

-- quotient account state by nullness
(~=) :: Map Addr EVM.Contract -> Map Addr EVM.Contract -> Bool
(~=) cs cs' =
    let nullAccount = EVM.initialContract (EVM.RuntimeCode mempty)
        padNewAccounts cs'' ks = (fold [Map.insertWith (\_ x -> x) k nullAccount | k <- ks]) cs''
        padded_cs' = padNewAccounts cs' (Map.keys cs)
        padded_cs  = padNewAccounts cs  (Map.keys cs')
    in padded_cs == padded_cs'

checkExpectedContracts :: EVM.VM -> Map Addr Contract -> (Bool, Bool, Bool, Bool, Bool)
checkExpectedContracts vm expected =
  let cs = vm ^. EVM.env . EVM.contracts . to (fmap clearZeroStorage)
      expectedCs = realizeContracts expected
  in ( (expectedCs ~= cs)
     , (clearBalance <$> expectedCs) ~= (clearBalance <$> cs)
     , (clearNonce   <$> expectedCs) ~= (clearNonce   <$> cs)
     , (clearStorage <$> expectedCs) ~= (clearStorage <$> cs)
     , (clearCode    <$> expectedCs) ~= (clearCode    <$> cs)
     )

clearZeroStorage :: EVM.Contract -> EVM.Contract
clearZeroStorage =
  over EVM.storage (Map.filterWithKey (\_ x -> x /= 0))

clearStorage :: EVM.Contract -> EVM.Contract
clearStorage = set EVM.storage mempty

clearBalance :: EVM.Contract -> EVM.Contract
clearBalance = set EVM.balance 0

clearNonce :: EVM.Contract -> EVM.Contract
clearNonce = set EVM.nonce 0

clearCode :: EVM.Contract -> EVM.Contract
clearCode = set EVM.contractcode (EVM.RuntimeCode mempty)

checkExpectedGas :: EVM.VM -> Maybe W256 -> Bool
checkExpectedGas vm ex = case ex of
  Nothing -> True
  Just expected -> case vm ^. EVM.state . EVM.gas of
    EVM.C _ x | x == expected -> True
    _ -> False

#if MIN_VERSION_aeson(1, 0, 0)

instance FromJSON Contract where
  parseJSON (JSON.Object v) = Contract
    <$> v .: "balance"
    <*> (EVM.RuntimeCode <$> (hexText <$> v .: "code"))
    <*> v .: "nonce"
    <*> v .: "storage"
    <*> pure False
  parseJSON invalid =
    JSON.typeMismatch "VM test case contract" invalid

instance FromJSON Case where
  parseJSON (JSON.Object v) = Case
    <$> parseVmOpts v
    <*> parseContracts Pre v
    <*> parseExpectation v
  parseJSON invalid =
    JSON.typeMismatch "VM test case" invalid

instance FromJSON BlockchainCase where
  parseJSON (JSON.Object v) = BlockchainCase
    <$> v .: "blocks"
    <*> parseContracts Pre v
    <*> parseContracts Post v
    <*> v .: "network"
  parseJSON invalid =
    JSON.typeMismatch "GeneralState test case" invalid

instance FromJSON Block where
  parseJSON (JSON.Object v) = do
    v'         <- v .: "blockHeader"
    txs        <- v .: "transactions"
    coinbase   <- addrField v' "coinbase"
    difficulty <- wordField v' "difficulty"
    gasLimit   <- wordField v' "gasLimit"
    number     <- wordField v' "number"
    timestamp  <- wordField v' "timestamp"
    return $ Block coinbase difficulty gasLimit number timestamp txs
  parseJSON invalid =
    JSON.typeMismatch "Block" invalid

parseVmOpts :: JSON.Object -> JSON.Parser EVM.VMOpts
parseVmOpts v =
  do envV  <- v .: "env"
     execV <- v .: "exec"
     case (envV, execV) of
       (JSON.Object env, JSON.Object exec) ->
         EVM.VMOpts
           <$> (dataField exec "code" >>= pure . EVM.initialContract . EVM.RuntimeCode)
           <*> (dataField exec "data" >>= pure . litBytes)
           <*> wordField exec "value"
           <*> addrField exec "address"
           <*> addrField exec "caller"
           <*> addrField exec "origin"
           <*> wordField exec "gas" -- XXX: correct?
           <*> wordField exec "gas" -- XXX: correct?
           <*> wordField env  "currentNumber"
           <*> wordField env  "currentTimestamp"
           <*> addrField env  "currentCoinbase"
           <*> wordField env  "currentDifficulty"
           <*> pure 0xffffffff
           <*> wordField env  "currentGasLimit"
           <*> wordField exec "gasPrice"
           <*> pure (EVM.FeeSchedule.homestead)
           <*> pure False
       _ ->
         JSON.typeMismatch "VM test case" (JSON.Object v)

parseContracts ::
  Which -> JSON.Object -> JSON.Parser (Map Addr Contract)
parseContracts w v =
  v .: which >>= parseJSON
  where which = case w of
          Pre  -> "pre"
          Post -> "postState"

parseExpectation :: JSON.Object -> JSON.Parser (Maybe Expectation)
parseExpectation v =
  do out       <- fmap hexText <$> v .:? "out"
     contracts <- v .:? "post"
     gas       <- v .:? "gas"
     case (out, contracts, gas) of
       (Just x, Just y, Just z) ->
         return (Just (Expectation (Just x) y (Just z)))
       _ ->
         return Nothing

parseSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseSuite = JSON.eitherDecode'

parseBCSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseBCSuite x = case (JSON.eitherDecode' x) :: Either String (Map String BlockchainCase) of
  Left e        -> Left e
  Right bcCases -> let allCases = (fromBlockchainCase <$> bcCases)
                       keepError (Left e) = errorFatal e
                       keepError _        = True
                       filteredCases = Map.filter keepError allCases
                       (erroredCases, parsedCases) = splitEithers filteredCases
    in if Map.size erroredCases > 0
    then Left ("errored case: " ++ (show $ (Map.elems erroredCases) !! 0))
    else if Map.size parsedCases == 0
    then Left "No cases to check."
    else Right parsedCases
#endif

realizeContracts :: Map Addr Contract -> Map Addr EVM.Contract
realizeContracts = Map.fromList . map f . Map.toList
  where
    f (a, x) = (a, realizeContract x)

realizeContract :: Contract -> EVM.Contract
realizeContract x =
  EVM.initialContract (x ^. code)
    & EVM.balance .~ EVM.w256 (x ^. balance)
    & EVM.nonce   .~ EVM.w256 (x ^. nonce)
    & EVM.storage .~ (
        Map.fromList .
        map (bimap EVM.w256 EVM.w256) .
        Map.toList $ x ^. storage
        )

data BlockchainError
  = TooManyBlocks
  | TooManyTxs
  | NoTxs
  | TargetMissing
  | SignatureUnverified
  | InvalidTx
  | OldNetwork
  | FailedCreate
  deriving Show

errorFatal :: BlockchainError -> Bool
errorFatal TooManyBlocks = True
errorFatal TooManyTxs = True
errorFatal TargetMissing = True
errorFatal SignatureUnverified = True
errorFatal InvalidTx = True
errorFatal _ = False

fromBlockchainCase :: BlockchainCase -> Either BlockchainError Case
fromBlockchainCase (BlockchainCase blocks preState postState network) =
  case (blocks, network) of
    ([block], "Istanbul") -> case blockTxs block of
      [tx] -> case txToAddr tx of
        Nothing -> fromCreateBlockchainCase block tx preState postState
        Just _  -> fromNormalBlockchainCase block tx preState postState
      []        -> Left NoTxs
      _         -> Left TooManyTxs
    ([_], _) -> Left OldNetwork
    (_, _)        -> Left TooManyBlocks

fromCreateBlockchainCase :: Block -> Transaction
                         -> Map Addr Contract -> Map Addr Contract
                         -> Either BlockchainError Case
fromCreateBlockchainCase block tx preState postState =
  case (sender 1 tx,
        checkCreateTx tx block preState) of
    (Nothing, _) -> Left SignatureUnverified
    (_, Nothing) -> Left FailedCreate
    (Just origin, Just (checkState, createdAddr)) -> let
      feeSchedule = EVM.FeeSchedule.istanbul
      in Right $ Case
         (EVM.VMOpts
          { vmoptContract      = EVM.initialContract (EVM.InitCode (txData tx))
          , vmoptCalldata      = mempty
          , vmoptValue         = txValue tx
          , vmoptAddress       = createdAddr
          , vmoptCaller        = origin
          , vmoptOrigin        = origin
          , vmoptGas           = txGasLimit tx - fromIntegral (txGasCost feeSchedule tx)
          , vmoptGaslimit      = txGasLimit tx
          , vmoptNumber        = blockNumber block
          , vmoptTimestamp     = blockTimestamp block
          , vmoptCoinbase      = blockCoinbase block
          , vmoptDifficulty    = blockDifficulty block
          , vmoptMaxCodeSize   = 24576
          , vmoptBlockGaslimit = blockGasLimit block
          , vmoptGasprice      = txGasPrice tx
          , vmoptSchedule      = feeSchedule
          , vmoptCreate        = True
          })
        checkState
        (Just $ Expectation Nothing postState Nothing)


fromNormalBlockchainCase :: Block -> Transaction
                       -> Map Addr Contract -> Map Addr Contract
                       -> Either BlockchainError Case
fromNormalBlockchainCase block tx preState postState =
  let Just toAddr = txToAddr tx
      feeSchedule = EVM.FeeSchedule.istanbul
      toCode = Map.lookup toAddr preState
      theCode = case toCode of
          Nothing -> EVM.RuntimeCode mempty
          Just c -> view code c
  in case (toAddr , toCode , sender 1 tx , checkNormalTx tx block preState) of
      (_, _, Nothing, _) -> Left SignatureUnverified
      (_, _, _, Nothing) -> Left InvalidTx
      (_, _, Just origin, Just checkState) -> Right $ Case
        (EVM.VMOpts
         { vmoptContract      = EVM.initialContract theCode
         { vmoptCode          = theCode
         , vmoptCalldata      = litBytes $ txData tx
         , vmoptValue         = txValue tx
         , vmoptAddress       = toAddr
         , vmoptCaller        = origin
         , vmoptOrigin        = origin
         , vmoptGas           = txGasLimit tx - fromIntegral (txGasCost feeSchedule tx)
         , vmoptGaslimit      = txGasLimit tx
         , vmoptNumber        = blockNumber block
         , vmoptTimestamp     = blockTimestamp block
         , vmoptCoinbase      = blockCoinbase block
         , vmoptDifficulty    = blockDifficulty block
         , vmoptMaxCodeSize   = 24576
         , vmoptBlockGaslimit = blockGasLimit block
         , vmoptGasprice      = txGasPrice tx
         , vmoptSchedule      = feeSchedule
         , vmoptCreate        = False
         })
        checkState
        (Just $ Expectation Nothing postState Nothing)


validateTx :: Transaction -> Map Addr Contract -> Maybe Bool
validateTx tx cs = do
  origin        <- sender 1 tx
  originBalance <- (view balance) <$> view (at origin) cs
  originNonce   <- (view nonce)   <$> view (at origin) cs
  let gasDeposit = fromIntegral (txGasPrice tx) * (txGasLimit tx)
  return $ gasDeposit + (txValue tx) <= originBalance
    && (txNonce tx) == originNonce

checkNormalTx :: Transaction -> Block -> Map Addr Contract -> Maybe (Map Addr Contract)
checkNormalTx tx block prestate = do
  toAddr <- txToAddr tx
  origin <- sender 1 tx
  valid  <- validateTx tx prestate
  let gasDeposit = fromIntegral (txGasPrice tx) * (txGasLimit tx)
      coinbase   = blockCoinbase block
  if not valid then mzero else
    return $
    (Map.adjust ((over nonce   (+ 1))
               . (over balance (subtract gasDeposit))) origin)
    . touchAccount origin
    . touchAccount toAddr
    . touchAccount coinbase $ prestate

checkCreateTx :: Transaction -> Block -> Map Addr Contract -> Maybe ((Map Addr Contract), Addr)
checkCreateTx tx block prestate = do
  origin <- sender 1 tx
  valid  <- validateTx tx prestate
  let gasDeposit  = fromIntegral (txGasPrice tx) * (txGasLimit tx)
      coinbase    = blockCoinbase block
      senderNonce = view (accountAt origin . nonce) prestate
      createdAddr = EVM.createAddress origin senderNonce
      prevCode    = view (accountAt createdAddr .  code) prestate
      prevNonce   = view (accountAt createdAddr . nonce) prestate
  if (prevCode /= EVM.RuntimeCode mempty) || (prevNonce /= 0) || (not valid)
  then mzero
  else
    return
    ((Map.adjust ((over nonce   (+ 1))
                 . (over balance (subtract gasDeposit))) origin)
    . touchAccount origin
    . touchAccount createdAddr
    . touchAccount coinbase $ prestate, createdAddr)

initTx :: Case -> Map Addr Contract
initTx x =
  let
    checkState = checkContracts x
    opts     = testVmOpts x
    toAddr   = EVM.vmoptAddress  opts
    origin   = EVM.vmoptOrigin   opts
    value    = EVM.vmoptValue    opts
    initcode = EVM._contractcode (EVM.vmoptContract opts)
    creation = EVM.vmoptCreate   opts
  in
    (Map.adjust (over balance (subtract value)) origin)
    . (Map.adjust (over balance (+ value)) toAddr)
    . (if creation
       then (Map.adjust (set code initcode) toAddr)
          . (Map.adjust (set nonce 1) toAddr)
          . (Map.adjust (set storage mempty) toAddr)
          . (Map.adjust (set create True) toAddr)
       else id)
    $ checkState

vmForCase :: EVM.ExecMode -> Case -> EVM.VM
vmForCase mode x =
  let
    checkState = checkContracts x
    initState =
      case mode of
        EVM.ExecuteAsBlockchainTest -> initTx x
        EVM.ExecuteAsVMTest         -> checkState
        EVM.ExecuteNormally         -> error "cannot initialize VM normally"
    opts = testVmOpts x
    creation = EVM.vmoptCreate opts
    touchedAccounts =
      if creation then
        [EVM.vmoptOrigin opts]
      else
        [EVM.vmoptOrigin opts, EVM.vmoptAddress opts]
  in
    EVM.makeVm (testVmOpts x)
    & EVM.env . EVM.contracts .~ realizeContracts initState
    & EVM.tx . EVM.txReversion .~ realizeContracts checkState
    & EVM.tx . EVM.origStorage .~ realizeContracts initState
    & EVM.tx . EVM.substate . EVM.touchedAccounts .~ touchedAccounts
    & EVM.execMode .~ mode

interpret :: Stepper a -> EVM a
interpret =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> EVM a

    eval (Operational.Return x) =
      pure x

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          EVM.Exec.exec >>= interpret . k
        Stepper.Wait q ->
          do join (Fetch.zero q)
             interpret (k ())
        Stepper.Note _ ->
          interpret (k ())
        Stepper.Fail _ ->
          error "VMTest stepper not supposed to fail"
        Stepper.EVM m ->
          State.state (runState m) >>= interpret . k
