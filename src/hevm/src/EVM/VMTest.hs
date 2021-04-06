{-# Language CPP #-}
{-# Language TemplateHaskell #-}

module EVM.VMTest
  ( Case
#if MIN_VERSION_aeson(1, 0, 0)
  , parseBCSuite
#endif
  , initTx
  , setupTx
  , vmForCase
  , checkExpectation
  ) where

import Prelude hiding (Word)

import qualified EVM
import EVM (contractcode, storage, origStorage, balance, nonce, Storage(..), initialContract)
import qualified EVM.Concrete as EVM
import qualified EVM.FeeSchedule

import EVM.Symbolic
import EVM.Transaction
import EVM.Types

import Control.Arrow ((***), (&&&))
import Control.Lens
import Control.Monad

import GHC.Stack

import Data.Aeson ((.:), FromJSON (..))
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
  , checkContracts  :: Map Addr EVM.Contract
  , testExpectation :: Map Addr EVM.Contract
  } deriving Show

data BlockchainCase = BlockchainCase
  { blockchainBlocks  :: [Block]
  , blockchainPre     :: Map Addr EVM.Contract
  , blockchainPost    :: Map Addr EVM.Contract
  , blockchainNetwork :: String
  } deriving Show

splitEithers :: (Filterable f) => f (Either a b) -> (f a, f b)
splitEithers =
  (catMaybes *** catMaybes)
  . (fmap fst &&& fmap snd)
  . (fmap (preview _Left &&& preview _Right))

checkStateFail :: Bool -> Case -> EVM.VM -> (Bool, Bool, Bool, Bool, Bool) -> IO Bool
checkStateFail diff x vm (okState, okMoney, okNonce, okData, okCode) = do
  let
    printContracts :: Map Addr EVM.Contract -> IO ()
    printContracts cs = putStrLn $ Map.foldrWithKey (\k v acc ->
      acc ++ show k ++ " : "
                   ++ (show . toInteger  $ (view nonce v)) ++ " "
                   ++ (show . toInteger  $ (view balance v)) ++ " "
                   ++ (printStorage $ (view storage v))
        ++ "\n") "" cs

    reason = map fst (filter (not . snd)
        [ ("bad-state",       okMoney || okNonce || okData  || okCode || okState)
        , ("bad-balance", not okMoney || okNonce || okData  || okCode || okState)
        , ("bad-nonce",   not okNonce || okMoney || okData  || okCode || okState)
        , ("bad-storage", not okData  || okMoney || okNonce || okCode || okState)
        , ("bad-code",    not okCode  || okMoney || okNonce || okData || okState)
        ])
    check = checkContracts x
    expected = testExpectation x
    actual = view (EVM.env . EVM.contracts . to (fmap (clearZeroStorage.clearOrigStorage))) vm
    printStorage (EVM.Symbolic _ c) = show c
    printStorage (EVM.Concrete c) = show $ Map.toList c

  putStr (unwords reason)
  when (diff && (not okState)) $ do
    putStrLn "\nPre balance/state: "
    printContracts check
    putStrLn "\nExpected balance/state: "
    printContracts expected
    putStrLn "\nActual balance/state: "
    printContracts actual
  return okState

checkExpectation :: HasCallStack => Bool -> Case -> EVM.VM -> IO Bool
checkExpectation diff x vm = do
  let expectation = testExpectation x
      (okState, b2, b3, b4, b5) = checkExpectedContracts vm $ expectation
  unless okState $ void $ checkStateFail
    diff x vm (okState, b2, b3, b4, b5)
  return okState

-- quotient account state by nullness
(~=) :: Map Addr EVM.Contract -> Map Addr EVM.Contract -> Bool
(~=) cs cs' =
    let nullAccount = EVM.initialContract (EVM.RuntimeCode mempty)
        padNewAccounts cs'' ks = (fold [Map.insertWith (\_ x -> x) k nullAccount | k <- ks]) cs''
        padded_cs' = padNewAccounts cs' (Map.keys cs)
        padded_cs  = padNewAccounts cs  (Map.keys cs')
    in and $ zipWith (===) (Map.elems padded_cs) (Map.elems padded_cs')

(===) :: EVM.Contract -> EVM.Contract -> Bool
a === b = codeEqual && storageEqual && (view balance a == view balance b) && (view nonce a == view nonce b)
  where
    storageEqual = view storage a == view storage b
    codeEqual = case (view contractcode a, view contractcode b) of
      (EVM.RuntimeCode (ConcreteBuffer a'), EVM.RuntimeCode (ConcreteBuffer b')) -> a' == b'
      _ -> error "unexpected code"

checkExpectedContracts :: HasCallStack => EVM.VM -> Map Addr EVM.Contract -> (Bool, Bool, Bool, Bool, Bool)
checkExpectedContracts vm expected =
  let cs = vm ^. EVM.env . EVM.contracts . to (fmap (clearZeroStorage.clearOrigStorage))
      expectedCs = clearOrigStorage <$> expected
  in ( (expectedCs ~= cs)
     , (clearBalance <$> expectedCs) ~= (clearBalance <$> cs)
     , (clearNonce   <$> expectedCs) ~= (clearNonce   <$> cs)
     , (clearStorage <$> expectedCs) ~= (clearStorage <$> cs)
     , (clearCode    <$> expectedCs) ~= (clearCode    <$> cs)
     )

clearOrigStorage :: EVM.Contract -> EVM.Contract
clearOrigStorage = set origStorage mempty

clearZeroStorage :: EVM.Contract -> EVM.Contract
clearZeroStorage c = case view storage c of
  EVM.Symbolic _ _ -> c
  EVM.Concrete m -> let store = Map.filter (\x -> forceLit x /= 0) m
                    in set EVM.storage (EVM.Concrete store) c

clearStorage :: EVM.Contract -> EVM.Contract
clearStorage = set storage (EVM.Concrete mempty)

clearBalance :: EVM.Contract -> EVM.Contract
clearBalance = set balance 0

clearNonce :: EVM.Contract -> EVM.Contract
clearNonce = set nonce 0

clearCode :: EVM.Contract -> EVM.Contract
clearCode = set contractcode (EVM.RuntimeCode mempty)

#if MIN_VERSION_aeson(1, 0, 0)

instance FromJSON EVM.Contract where
  parseJSON (JSON.Object v) = do
    code <- (EVM.RuntimeCode . ConcreteBuffer <$> (hexText <$> v .: "code"))
    storage' <- Map.mapKeys w256 <$> v .: "storage"
    balance' <- v .: "balance"
    nonce'   <- v .: "nonce"
    return
      $
      EVM.initialContract code
       & balance .~ w256 balance'
       & nonce   .~ w256 nonce'
       & storage .~ EVM.Concrete (fmap (litWord . w256) storage')
       & origStorage .~ fmap w256 storage'

  parseJSON invalid =
    JSON.typeMismatch "Contract" invalid

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

parseContracts ::
  Which -> JSON.Object -> JSON.Parser (Map Addr EVM.Contract)
parseContracts w v =
  v .: which >>= parseJSON
  where which = case w of
          Pre  -> "pre"
          Post -> "postState"

parseBCSuite ::
  Lazy.ByteString -> Either String (Map String Case)
parseBCSuite x = case (JSON.eitherDecode' x) :: Either String (Map String BlockchainCase) of
  Left e        -> Left e
  Right bcCases -> let allCases = fromBlockchainCase <$> bcCases
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
    ([block], "Berlin") -> case blockTxs block of
      [tx] -> fromBlockchainCase' block tx preState postState
      []        -> Left NoTxs
      _         -> Left TooManyTxs
    ([_], _) -> Left OldNetwork
    (_, _)   -> Left TooManyBlocks

fromBlockchainCase' :: Block -> Transaction
                       -> Map Addr EVM.Contract -> Map Addr EVM.Contract
                       -> Either BlockchainError Case
fromBlockchainCase' block tx preState postState =
  let isCreate = isNothing (txToAddr tx)
  in case (sender 1 tx, checkTx tx preState) of
      (Nothing, _) -> Left SignatureUnverified
      (_, Nothing) -> Left (if isCreate then FailedCreate else InvalidTx)
      (Just origin, Just checkState) -> Right $ Case
        (EVM.VMOpts
         { vmoptContract      = EVM.initialContract theCode
         , vmoptCalldata      = cd
         , vmoptValue         = litWord (w256 $ txValue tx)
         , vmoptAddress       = toAddr
         , vmoptCaller        = litAddr origin
         , vmoptOrigin        = origin
         , vmoptGas           = txGasLimit tx - fromIntegral (txGasCost feeSchedule tx)
         , vmoptGaslimit      = txGasLimit tx
         , vmoptNumber        = blockNumber block
         , vmoptTimestamp     = litWord $ w256 $ blockTimestamp block
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
        postState
          where
            toAddr = fromMaybe (EVM.createAddress origin senderNonce) (txToAddr tx)
            senderNonce = EVM.wordValue $ view (accountAt origin . nonce) preState
            feeSchedule = EVM.FeeSchedule.berlin
            toCode = Map.lookup toAddr preState
            theCode = if isCreate
                      then EVM.InitCode (ConcreteBuffer (txData tx))
                      else maybe (EVM.RuntimeCode mempty) (view contractcode) toCode
            cd = if isCreate
                 then (mempty, 0)
                 else let l = num . BS.length $ txData tx
                      in (ConcreteBuffer $ txData tx, litWord l)


validateTx :: Transaction -> Map Addr EVM.Contract -> Maybe ()
validateTx tx cs = do
  origin        <- sender 1 tx
  originBalance <- (view balance) <$> view (at origin) cs
  originNonce   <- (view nonce)   <$> view (at origin) cs
  let gasDeposit = w256 $ (txGasPrice tx) * (txGasLimit tx)
  if gasDeposit + (w256 $ txValue tx) <= originBalance
    && (w256 $ txNonce tx) == originNonce
  then Just ()
  else Nothing

checkTx :: Transaction -> Map Addr EVM.Contract -> Maybe (Map Addr EVM.Contract)
checkTx tx prestate = do
  origin <- sender 1 tx
  validateTx tx prestate
  let isCreate   = isNothing (txToAddr tx)
      senderNonce = EVM.wordValue $ view (accountAt origin . nonce) prestate
      toAddr      = fromMaybe (EVM.createAddress origin senderNonce) (txToAddr tx)
      prevCode    = view (accountAt toAddr . contractcode) prestate
      prevNonce   = view (accountAt toAddr . nonce) prestate
  if isCreate && ((case prevCode of {EVM.RuntimeCode b -> len b /= 0; _ -> True}) || (prevNonce /= 0))
  then mzero
  else
    return $ prestate

vmForCase :: Case -> EVM.VM
vmForCase x =
  let
    vm = EVM.makeVm (testVmOpts x)
      & set (EVM.env . EVM.contracts) (checkContracts x)
  in
    initTx vm
