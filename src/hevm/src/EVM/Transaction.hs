module EVM.Transaction where

import Prelude hiding (Word)

import EVM.FeeSchedule
import EVM.Types (keccak)
import EVM.Precompiled (execute)
import EVM.RLP
import EVM.Types

import Data.Aeson (FromJSON (..))
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)

import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString   as BS

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
