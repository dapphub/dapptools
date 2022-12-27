module EVM.Transaction where

import Prelude hiding (Word)

import qualified EVM
import EVM (balance, initialContract)
import EVM.FeeSchedule
import EVM.Precompiled (execute)
import EVM.RLP
import EVM.Symbolic (forceLit)
import EVM.Types

import Control.Lens

import Data.Aeson (FromJSON (..))
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing, isJust)

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString   as BS
import qualified Data.Map          as Map

data AccessListEntry = AccessListEntry {
  accessAddress :: Addr,
  accessStorageKeys :: [W256]
} deriving Show

data TxType = LegacyTransaction
            | AccessListTransaction
            | EIP1559Transaction
  deriving (Show, Eq)

data Transaction = Transaction {
    txData     :: ByteString,
    txGasLimit :: W256,
    txGasPrice :: Maybe W256,
    txNonce    :: W256,
    txR        :: W256,
    txS        :: W256,
    txToAddr   :: Maybe Addr,
    txV        :: W256,
    txValue    :: W256,
    txType     :: TxType,
    txAccessList :: [AccessListEntry],
    txMaxPriorityFeeGas :: Maybe W256,
    txMaxFeePerGas :: Maybe W256
} deriving Show

-- | utility function for getting a more useful representation of accesslistentries
-- duplicates only matter for gas computation
txAccessMap :: Transaction -> Map Addr [W256]
txAccessMap tx = ((Map.fromListWith (++)) . makeTups) $ txAccessList tx
  where makeTups = map (\ale -> (accessAddress ale, accessStorageKeys ale))

ecrec :: W256 -> W256 -> W256 -> W256 -> Maybe Addr
ecrec v r s e = num . word <$> EVM.Precompiled.execute 1 input 32
  where input = BS.concat (word256Bytes <$> [e, v, r, s])

sender :: Int -> Transaction -> Maybe Addr
sender chainId tx = ecrec v' (txR tx) (txS tx) hash
  where hash = keccak (signingData chainId tx)
        v    = txV tx
        v'   = if v == 27 || v == 28 then v
               else 27 + v

signingData :: Int -> Transaction -> ByteString
signingData chainId tx =
  case txType tx of
    LegacyTransaction -> if v == (chainId * 2 + 35) || v == (chainId * 2 + 36)
      then eip155Data
      else normalData
    AccessListTransaction -> eip2930Data
    EIP1559Transaction -> eip1559Data
  where v          = fromIntegral (txV tx)
        to'        = case txToAddr tx of
          Just a  -> BS $ word160Bytes a
          Nothing -> BS mempty
        Just maxFee = txMaxFeePerGas tx
        Just maxPrio = txMaxPriorityFeeGas tx
        Just gasPrice = txGasPrice tx
        accessList = txAccessList tx
        rlpAccessList = EVM.RLP.List $ map (\accessEntry ->
          EVM.RLP.List [BS $ word160Bytes (accessAddress accessEntry),
                        EVM.RLP.List $ map rlpWordFull $ accessStorageKeys accessEntry]
          ) accessList
        normalData = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 gasPrice,
                              rlpWord256 (txGasLimit tx),
                              to',
                              rlpWord256 (txValue tx),
                              BS (txData tx)]
        eip155Data = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 gasPrice,
                              rlpWord256 (txGasLimit tx),
                              to',
                              rlpWord256 (txValue tx),
                              BS (txData tx),
                              rlpWord256 (fromIntegral chainId),
                              rlpWord256 0x0,
                              rlpWord256 0x0]
        eip1559Data = cons 0x02 $ rlpList [
          rlpWord256 (fromIntegral chainId),
          rlpWord256 (txNonce tx),
          rlpWord256 maxPrio,
          rlpWord256 maxFee,
          rlpWord256 (txGasLimit tx),
          to',
          rlpWord256 (txValue tx),
          BS (txData tx),
          rlpAccessList]

        eip2930Data = cons 0x01 $ rlpList [
          rlpWord256 (fromIntegral chainId),
          rlpWord256 (txNonce tx),
          rlpWord256 gasPrice,
          rlpWord256 (txGasLimit tx),
          to',
          rlpWord256 (txValue tx),
          BS (txData tx),
          rlpAccessList]

accessListPrice :: FeeSchedule Integer -> [AccessListEntry] -> Integer
accessListPrice fs al =
    sum (map
      (\ale ->
        g_access_list_address fs +
        (g_access_list_storage_key fs * (toInteger . length) (accessStorageKeys ale)))
        al)

txGasCost :: FeeSchedule Integer -> Transaction -> Integer
txGasCost fs tx =
  let calldata     = txData tx
      zeroBytes    = BS.count 0 calldata
      nonZeroBytes = BS.length calldata - zeroBytes
      baseCost     = g_transaction fs
        + (if isNothing (txToAddr tx) then g_txcreate fs else 0)
        + (accessListPrice fs $ txAccessList tx)
      zeroCost     = g_txdatazero fs
      nonZeroCost  = g_txdatanonzero fs
  in baseCost + zeroCost * (fromIntegral zeroBytes) + nonZeroCost * (fromIntegral nonZeroBytes)

instance FromJSON AccessListEntry where
  parseJSON (JSON.Object val) = do
    accessAddress_ <- addrField val "address"
    accessStorageKeys_ <- (val JSON..: "storageKeys") >>= parseJSONList
    return $ AccessListEntry accessAddress_ accessStorageKeys_
  parseJSON invalid =
    JSON.typeMismatch "AccessListEntry" invalid

instance FromJSON Transaction where
  parseJSON (JSON.Object val) = do
    tdata    <- dataField val "data"
    gasLimit <- wordField val "gasLimit"
    gasPrice <- fmap read <$> val JSON..:? "gasPrice"
    maxPrio  <- fmap read <$> val JSON..:? "maxPriorityFeePerGas"
    maxFee   <- fmap read <$> val JSON..:? "maxFeePerGas"
    nonce    <- wordField val "nonce"
    r        <- wordField val "r"
    s        <- wordField val "s"
    toAddr   <- addrFieldMaybe val "to"
    v        <- wordField val "v"
    value    <- wordField val "value"
    txType   <- fmap read <$> (val JSON..:? "type")
    case txType of
      Just 0x00 -> return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value LegacyTransaction [] Nothing Nothing
      Just 0x01 -> do
        accessListEntries <- (val JSON..: "accessList") >>= parseJSONList
        return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value AccessListTransaction accessListEntries Nothing Nothing
      Just 0x02 -> do
        accessListEntries <- (val JSON..: "accessList") >>= parseJSONList
        return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value EIP1559Transaction accessListEntries maxPrio maxFee
      Just _ -> fail "unrecognized custom transaction type"
      Nothing -> return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value LegacyTransaction [] Nothing Nothing
  parseJSON invalid =
    JSON.typeMismatch "Transaction" invalid

accountAt :: Addr -> Getter (Map Addr EVM.Contract) EVM.Contract
accountAt a = (at a) . (to $ fromMaybe newAccount)

touchAccount :: Addr -> Map Addr EVM.Contract -> Map Addr EVM.Contract
touchAccount a = Map.insertWith (flip const) a newAccount

newAccount :: EVM.Contract
newAccount = initialContract $ EVM.RuntimeCode mempty

-- | Increments origin nonce and pays gas deposit
setupTx :: Addr -> Addr -> Word -> Word -> Map Addr EVM.Contract -> Map Addr EVM.Contract
setupTx origin coinbase gasPrice gasLimit prestate =
  let gasCost = gasPrice * gasLimit
  in (Map.adjust ((over EVM.nonce   (+ 1))
               . (over balance (subtract gasCost))) origin)
    . touchAccount origin
    . touchAccount coinbase $ prestate

-- | Given a valid tx loaded into the vm state,
-- subtract gas payment from the origin, increment the nonce
-- and pay receiving address
initTx :: EVM.VM -> EVM.VM
initTx vm = let
    toAddr   = view (EVM.state . EVM.contract) vm
    origin   = view (EVM.tx . EVM.origin) vm
    gasPrice = view (EVM.tx . EVM.gasprice) vm
    gasLimit = view (EVM.tx . EVM.txgaslimit) vm
    coinbase = view (EVM.block . EVM.coinbase) vm
    value    = view (EVM.state . EVM.callvalue) vm
    toContract = initialContract (EVM.InitCode (view (EVM.state . EVM.code) vm))
    preState = setupTx origin coinbase gasPrice gasLimit $ view (EVM.env . EVM.contracts) vm
    oldBalance = view (accountAt toAddr . balance) preState
    creation = view (EVM.tx . EVM.isCreate) vm
    initState =
      (if isJust (maybeLitWord value)
       then (Map.adjust (over balance (subtract (forceLit value))) origin)
        . (Map.adjust (over balance (+ (forceLit value))) toAddr)
       else id)
      . (if creation
         then Map.insert toAddr (toContract & balance .~ oldBalance)
         else touchAccount toAddr)
      $ preState

    in
      vm & EVM.env . EVM.contracts .~ initState
         & EVM.tx . EVM.txReversion .~ preState
