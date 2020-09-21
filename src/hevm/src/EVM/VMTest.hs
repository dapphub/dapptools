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
  ) where

import qualified EVM
import qualified EVM.Concrete as EVM
import qualified EVM.FeeSchedule

import EVM.Symbolic
import EVM.Transaction
import EVM.Types

import Data.SBV

import Control.Arrow ((***), (&&&))
import Control.Lens
import Control.Monad

import Data.ByteString (ByteString)
import Data.Aeson ((.:), (.:?), FromJSON (..))
import Data.Bifunctor (bimap)
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing)
import Data.Witherable (Filterable, catMaybes)

import qualified Data.Map          as Map
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString as BS

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
      acc ++ show k ++ " : " ++ f v ++ "\n") "" d

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
    actual = view (EVM.env . EVM.contracts . to (fmap (clearZeroStorage.clearOrigStorage))) vm
    printStorage (EVM.Symbolic c) = show c
    printStorage (EVM.Concrete c) = show $ Map.toList c

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
                   ++ (printStorage      $ EVM._storage v)) actual
  return okState

checkExpectation :: Bool -> Case -> EVM.VM -> IO Bool
checkExpectation diff x vm =
  case (testExpectation x, view EVM.result vm) of
    (Just expectation, _) -> do
      let (okState, b2, b3, b4, b5) =
            checkExpectedContracts vm (expectedContracts expectation)
      _ <- if not okState then
               checkStateFail
                 diff x expectation vm (okState, b2, b3, b4, b5)
           else return True
      return okState

    (Nothing, Just (EVM.VMSuccess _)) -> do
      putStr "unexpected-success"
      return False

    (Nothing, Just (EVM.VMFailure _)) ->
      return True

    (_, Nothing) -> do
      print (view EVM.result vm)
      error "internal error"

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
  let cs = vm ^. EVM.env . EVM.contracts . to (fmap (clearZeroStorage.clearOrigStorage))
      expectedCs = clearOrigStorage <$> realizeContracts expected
  in ( (expectedCs ~= cs)
     , (clearBalance <$> expectedCs) ~= (clearBalance <$> cs)
     , (clearNonce   <$> expectedCs) ~= (clearNonce   <$> cs)
     , (clearStorage <$> expectedCs) ~= (clearStorage <$> cs)
     , (clearCode    <$> expectedCs) ~= (clearCode    <$> cs)
     )

clearOrigStorage :: EVM.Contract -> EVM.Contract
clearOrigStorage = set EVM.origStorage mempty

clearZeroStorage :: EVM.Contract -> EVM.Contract
clearZeroStorage c = case EVM._storage c of
  EVM.Symbolic _ -> c
  EVM.Concrete m -> let store = Map.filter (\x -> forceLit x /= 0) m
                    in set EVM.storage (EVM.Concrete store) c

clearStorage :: EVM.Contract -> EVM.Contract
clearStorage = set EVM.storage (EVM.Concrete mempty)

clearBalance :: EVM.Contract -> EVM.Contract
clearBalance = set EVM.balance 0

clearNonce :: EVM.Contract -> EVM.Contract
clearNonce = set EVM.nonce 0

clearCode :: EVM.Contract -> EVM.Contract
clearCode = set EVM.contractcode (EVM.RuntimeCode mempty)

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
           <*> (dataField exec "data" >>= \a -> pure ( (ConcreteBuffer a), literal . num $ BS.length a))
           <*> (w256lit <$> wordField exec "value")
           <*> addrField exec "address"
           <*> (litAddr <$> addrField exec "caller")
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
           <*> pure (EVM.FeeSchedule.istanbul)
           <*> pure 1
           <*> pure False
           <*> pure EVM.ConcreteS
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
realizeContracts = fmap realizeContract

realizeContract :: Contract -> EVM.Contract
realizeContract x =
  EVM.initialContract (x ^. code)
    & EVM.balance .~ EVM.w256 (x ^. balance)
    & EVM.nonce   .~ EVM.w256 (x ^. nonce)
    & EVM.storage .~ EVM.Concrete (
        Map.fromList .
        map (bimap EVM.w256 (litWord . EVM.w256)) .
        Map.toList $ x ^. storage
        )
    & EVM.origStorage .~ (
        Map.fromList .
        map (bimap EVM.w256 EVM.w256) .
        Map.toList $ x ^. storage
        )

data BlockchainError
  = TooManyBlocks
  | TooManyTxs
  | NoTxs
  | SignatureUnverified
  | InvalidTx
  | OldNetwork
  | FailedCreate
  deriving Show

errorFatal :: BlockchainError -> Bool
errorFatal TooManyBlocks = True
errorFatal TooManyTxs = True
errorFatal SignatureUnverified = True
errorFatal InvalidTx = True
errorFatal _ = False

fromBlockchainCase :: BlockchainCase -> Either BlockchainError Case
fromBlockchainCase (BlockchainCase blocks preState postState network) =
  case (blocks, network) of
    ([block], "Istanbul") -> case blockTxs block of
      [tx] -> fromBlockchainCase' block tx preState postState
      []        -> Left NoTxs
      _         -> Left TooManyTxs
    ([_], _) -> Left OldNetwork
    (_, _)        -> Left TooManyBlocks

fromBlockchainCase' :: Block -> Transaction
                       -> Map Addr Contract -> Map Addr Contract
                       -> Either BlockchainError Case
fromBlockchainCase' block tx preState postState =
  let isCreate = isNothing (txToAddr tx)
  in case (sender 1 tx, checkTx tx block preState) of
      (Nothing, _) -> Left SignatureUnverified
      (_, Nothing) -> Left (if isCreate then FailedCreate else InvalidTx)
      (Just origin, Just checkState) -> Right $ Case
        (EVM.VMOpts
         { vmoptContract      = EVM.initialContract theCode
         , vmoptCalldata      = cd
         , vmoptValue         = litWord (EVM.w256 $ txValue tx)
         , vmoptAddress       = toAddr
         , vmoptCaller        = litAddr origin
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
         , vmoptChainId       = 1
         , vmoptCreate        = isCreate
         , vmoptStorageModel  = EVM.ConcreteS
         })
        checkState
        (Just $ Expectation Nothing postState Nothing)
          where
            toAddr = fromMaybe (EVM.createAddress origin senderNonce) (txToAddr tx)
            senderNonce = view (accountAt origin . nonce) preState
            feeSchedule = EVM.FeeSchedule.istanbul
            toCode = Map.lookup toAddr preState
            theCode = if isCreate
                      then EVM.InitCode (txData tx)
                      else maybe (EVM.RuntimeCode mempty) (view code) toCode
            cd = if isCreate
                 then (mempty, 0)
                 else (ConcreteBuffer $ txData tx, literal . num . BS.length $ txData tx)


validateTx :: Transaction -> Map Addr Contract -> Maybe ()
validateTx tx cs = do
  origin        <- sender 1 tx
  originBalance <- (view balance) <$> view (at origin) cs
  originNonce   <- (view nonce)   <$> view (at origin) cs
  let gasDeposit = fromIntegral (txGasPrice tx) * (txGasLimit tx)
  if gasDeposit + (txValue tx) <= originBalance
    && (txNonce tx) == originNonce
  then Just ()
  else Nothing

checkTx :: Transaction -> Block -> Map Addr Contract -> Maybe (Map Addr Contract)
checkTx tx block prestate = do
  origin <- sender 1 tx
  validateTx tx prestate
  let gasDeposit = fromIntegral (txGasPrice tx) * (txGasLimit tx)
      coinbase   = blockCoinbase block
      isCreate   = isNothing (txToAddr tx)
      toAddr     = fromMaybe (EVM.createAddress origin senderNonce) (txToAddr tx)
      senderNonce = view (accountAt origin . nonce) prestate
      prevCode    = view (accountAt toAddr .  code) prestate
      prevNonce   = view (accountAt toAddr . nonce) prestate
  if isCreate && ((prevCode /= EVM.RuntimeCode mempty) || (prevNonce /= 0))
  then mzero
  else
    return $
    (Map.adjust ((over nonce   (+ 1))
               . (over balance (subtract gasDeposit))) origin)
    . touchAccount origin
    . touchAccount toAddr
    . touchAccount coinbase $ prestate

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
    (Map.adjust (over balance (subtract (EVM.wordValue $ forceLit value))) origin)
    . (Map.adjust (over balance (+ (EVM.wordValue $ forceLit value))) toAddr)
    . (if creation
       then (Map.adjust (set code initcode) toAddr)
          . (Map.adjust (set nonce 1) toAddr)
          . (Map.adjust (set storage mempty) toAddr)
          . (Map.adjust (set create True) toAddr)
       else id)
    $ checkState

vmForCase :: Case -> EVM.VM
vmForCase x =
  let
    checkState = checkContracts x
    initState = initTx x
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
    & EVM.tx . EVM.substate . EVM.touchedAccounts .~ touchedAccounts
