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
import Data.Map (Map)
import Data.Maybe (fromMaybe, isNothing, isJust)

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString   as BS
import qualified Data.Map          as Map

data Transaction = Transaction
  { txData     :: ByteString,
    txGasLimit :: W256,
    txGasPrice :: W256,
    txNonce    :: W256,
    txR        :: W256,
    txS        :: W256,
    txToAddr   :: Maybe Addr,
    txV        :: W256,
    txValue    :: W256
  } deriving Show

ecrec :: W256 -> W256 -> W256 -> W256 -> Maybe Addr
ecrec e v r s = (num . word) <$> EVM.Precompiled.execute 1 input 32
  where input = BS.concat (word256Bytes <$> [e, v, r, s])

sender :: Int -> Transaction -> Maybe Addr
sender chainId tx = ecrec hash v' (txR tx) (txS tx)
  where hash = keccak $ signingData chainId tx
        v    = txV tx
        v'   = if v == 27 || v == 28 then v
               else 28 - mod v 2

signingData :: Int -> Transaction -> ByteString
signingData chainId tx =
  if v == (chainId * 2 + 35) || v == (chainId * 2 + 36)
  then eip155Data
  else normalData
  where v          = fromIntegral (txV tx)
        to'        = case txToAddr tx of
          Just a  -> BS $ word160Bytes a
          Nothing -> BS mempty
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

txGasCost :: FeeSchedule Integer -> Transaction -> Integer
txGasCost fs tx =
  let calldata     = txData tx
      zeroBytes    = BS.count 0 calldata
      nonZeroBytes = BS.length calldata - zeroBytes
      baseCost     = g_transaction fs
        + if isNothing (txToAddr tx) then g_txcreate fs else 0
      zeroCost     = g_txdatazero fs
      nonZeroCost  = g_txdatanonzero fs
  in baseCost + zeroCost * (fromIntegral zeroBytes) + nonZeroCost * (fromIntegral nonZeroBytes)

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
    return $ Transaction tdata gasLimit gasPrice nonce r s toAddr v value
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

    touched = if creation
              then [origin]
              else [origin, toAddr]

    in
      vm & EVM.env . EVM.contracts .~ initState
         & EVM.tx . EVM.txReversion .~ preState
         & EVM.tx . EVM.substate . EVM.touchedAccounts .~ touched
