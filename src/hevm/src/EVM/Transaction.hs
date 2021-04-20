module EVM.Transaction where

import Prelude hiding (Word)

import qualified EVM
import EVM (balance, initialContract)
import EVM.FeeSchedule
import EVM.Precompiled (execute)
import EVM.RLP
import EVM.Symbolic (forceLit)
import EVM.Types (keccak)
import EVM.Types

import Control.Lens

import Data.Aeson (FromJSON (..))
import Data.ByteString (ByteString)
import Data.Map (Map, keys)
import Data.Set (fromList)
import Data.Maybe (fromMaybe, isNothing, isJust)

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString   as BS
import qualified Data.Map          as Map

data AccessListEntry = AccessListEntry {
  accessAddress :: Addr,
  accessStorageKeys :: [W256]
} deriving Show

data Transaction = Transaction {
    txData     :: ByteString,
    txGasLimit :: W256,
    txGasPrice :: W256,
    txNonce    :: W256,
    txR        :: W256,
    txS        :: W256,
    txToAddr   :: Maybe Addr,
    txV        :: W256,
    txValue    :: W256,
    txType     :: Maybe W256,
    txAccessList :: [AccessListEntry]
} deriving Show

-- utility function for getting a more useful representation of accesslistentries
-- duplicates only matter for gas computation
-- ugly! could use a review....
txAccessMap :: Transaction -> Map Addr [W256]
txAccessMap tx = ((Map.fromListWith (++)) . makeTups) $ txAccessList tx
  where makeTups = map (\ale -> (accessAddress ale, accessStorageKeys ale))

ecrec :: W256 -> W256 -> W256 -> W256 -> Maybe Addr
ecrec v r s e = num . word <$> EVM.Precompiled.execute 1 input 32
  where input = BS.concat (word256Bytes <$> [e, v, r, s])

sender :: Int -> Transaction -> Maybe Addr
sender chainId tx = hash >>= (ecrec v' (txR tx) (txS tx))
  where hash = keccak <$> signingData chainId tx
        v    = txV tx
        v'   = if v == 27 || v == 28 then v
               else 28 - mod v 2

signingData :: Int -> Transaction -> Maybe ByteString
signingData chainId tx =
  case txType tx of
    Nothing -> (if v == (chainId * 2 + 35) || v == (chainId * 2 + 36)
      then Just eip155Data
      else Just normalData)
    Just 0x01 -> Just eip2930Data
    _ -> Nothing
  where v          = fromIntegral (txV tx)
        to'        = case txToAddr tx of
          Just a  -> BS $ word160Bytes a
          Nothing -> BS mempty
        accessList = txAccessList tx
        rlpAccessList = BS.concat $ map (\accessEntry ->
          rlpList [BS $ word160Bytes (accessAddress accessEntry), 
          BS $ rlpList $ map rlpWord256 $ accessStorageKeys accessEntry
          ]) accessList
        normalData = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 (txGasPrice tx),
                              rlpWord256 (txGasLimit tx),
                              to',
                              rlpWord256 (txValue tx),
                              BS (txData tx)]
        eip155Data = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 (txGasPrice tx),
                              rlpWord256 (txGasLimit tx),
                              to',
                              rlpWord256 (txValue tx),
                              BS (txData tx),
                              rlpWord256 (fromIntegral chainId),
                              rlpWord256 0x0,
                              rlpWord256 0x0]
        eip2930Data = cons 0x01 $ rlpList [
          rlpWord256 (fromIntegral chainId),
          rlpWord256 (txNonce tx),
          rlpWord256 (txGasPrice tx),
          rlpWord256 (txGasLimit tx),
          to', 
          rlpWord256 (txValue tx),
          BS (txData tx),
          BS rlpAccessList]

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
        + if isNothing (txToAddr tx) then g_txcreate fs else 0
        + (accessListPrice fs $ txAccessList tx)
      zeroCost     = g_txdatazero fs
      nonZeroCost  = g_txdatanonzero fs
  in baseCost + zeroCost * (fromIntegral zeroBytes) + nonZeroCost * (fromIntegral nonZeroBytes)

instance FromJSON AccessListEntry where
  parseJSON (JSON.Object val) = do
    accessAddress_ <- addrField val "address"
    --storageKeys <- (val JSON..: "storageKeys")
    --accessStorageKeys_ <- JSON.listParser (JSON.withText "W256" (return . readNull 0 . Text.unpack)) storageKeys
    accessStorageKeys_ <- (val JSON..: "storageKeys") >>= parseJSONList
    return $ AccessListEntry accessAddress_ accessStorageKeys_
  parseJSON invalid =
    JSON.typeMismatch "AccessListEntry" invalid

instance FromJSON Transaction where
  parseJSON (JSON.Object val) = do
    tdata    <- dataField val "data"
    gasLimit <- wordField val "gasLimit"
    gasPrice <- wordField val "gasPrice"
    nonce    <- wordField val "nonce"
    r        <- wordField val "r"
    s        <- wordField val "s"
    toAddr   <- addrFieldMaybe val "to"
    v        <- wordField val "v"
    value    <- wordField val "value"
    txType   <- wordFieldMaybe val "type"
    --let legacyTxn = Transaction tdata gasLimit gasPrice nonce r s toAddr v value
    case txType of
      Just 0x01 -> do
        accessListEntries <- (val JSON..: "accessList") >>= parseJSONList
        return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value txType accessListEntries
      Just _ -> fail "unrecognized custom transaction type"
      Nothing -> return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value Nothing []
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
