{-# LANGUAGE TemplateHaskell #-}
module EVM.Transaction where

import Prelude hiding (Word, drop, length, take, head, tail)

import EVM.Concrete
import EVM.FeeSchedule
import EVM.Keccak (keccak, word160Bytes, word256Bytes, rlpWord256, rlpBytes, rlpList)
import EVM.Precompiled (execute)
import EVM.Types

import Data.Aeson (FromJSON (..))
import Data.ByteString (ByteString, drop, length, head)
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
  where input = BS.concat $ (word256Bytes <$> [e, v, r, s])

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
          Just a  -> rlpBytes $ word160Bytes a
          Nothing -> rlpBytes mempty
        normalData = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 (txGasPrice tx),
                              rlpWord256 (txGasLimit tx),
                              to',
                              rlpWord256 (txValue tx),
                              rlpBytes   (txData tx)]
        eip155Data = rlpList [rlpWord256 (txNonce tx),
                              rlpWord256 (txGasPrice tx),
                              rlpWord256 (txGasLimit tx),
                              to',
                              rlpWord256 (txValue tx),
                              rlpBytes   (txData tx),
                              rlpWord256 (fromIntegral chainId),
                              rlpWord256 0x0,
                              rlpWord256 0x0]

txGasCost :: FeeSchedule Word -> Transaction -> Word
txGasCost fs tx =
  let calldata     = txData tx
      zeroBytes    = BS.count 0 calldata
      nonZeroBytes = BS.length calldata - zeroBytes
      baseCost     = g_transaction fs
        + if isNothing (txToAddr tx) then g_txcreate fs else 0
      zeroCost     = g_txdatazero fs
      nonZeroCost  = g_txdatanonzero fs
  in baseCost + zeroCost * (fromIntegral zeroBytes) + nonZeroCost * (fromIntegral nonZeroBytes)

data RLP = BS ByteString | List [RLP]
instance Show RLP where
  show (BS str) = show (ByteStringS str)
  show (List list) = show list

--helper function returning (the length of the prefix, the length of the content, isList boolean)
itemInfo :: ByteString -> (Int, Int, Bool)
itemInfo bs = case head bs of
  x | 0 <= x && x < 128   -> (0, 1, False)
  x | 128 <= x && x < 184 -> (1, num x - 128, False)
  x | 184 <= x && x < 192 -> (1 + pre, num $ word $ slice 1 pre bs, False) where pre = num $ x - 183
  x | 192 <= x && x < 248 -> (1, num $ x - 192, True)
  x                       -> (1 + pre, num $ word $ slice 1 pre bs, True)  where pre = num $ x - 247

-- rlp decoding, not fully compliant (suboptimal encodings accepted)
rlpdecode :: ByteString -> Maybe RLP
rlpdecode bs = let (pre, len, isList) = itemInfo bs
               in if pre + len == length bs
                  then if isList
                    then let content = drop pre bs
                         in do
                           items <- sequence $ fmap (\(s, e) -> rlpdecode $ slice s e content) $ rlplengths content 0 $ len
                           Just (List items)
                   else Just (BS $ drop (pre) bs)
                 else Nothing

rlplengths :: ByteString -> Int -> Int -> [(Int,Int)]
rlplengths bs acc top | acc < top = let (pre, len, _) = itemInfo bs
                                    in (acc, pre + len) : rlplengths (drop (pre + len) bs) (acc + pre + len) top
                      | otherwise = []

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
