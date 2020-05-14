{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}

module EVM.Fetch where

import Prelude hiding (Word)

import EVM.Types    (Addr, W256, showAddrWith0x, hexText)
import EVM.Concrete (Word, w256)
import EVM          (EVM, Contract, initialContract, nonce, balance, external)

import qualified EVM

import Control.Lens hiding ((.=))
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Network.Wreq
import Network.Wreq.Session (Session)

import qualified Network.Wreq.Session as Session

-- | Abstract representation of an RPC fetch request
data RpcQuery a where
  QueryCode    :: Addr         -> RpcQuery ByteString
  QueryBalance :: Addr         -> RpcQuery W256
  QueryNonce   :: Addr         -> RpcQuery W256
  QuerySlot    :: Addr -> W256 -> RpcQuery W256
  QueryChainId ::                 RpcQuery W256

data BlockNumber = Latest | BlockNumber W256

deriving instance Show (RpcQuery a)

mkr :: Addr
mkr = 0xc66ea802717bfb9833400264dd12c2bceaa34a6d

rpc :: String -> [String] -> Value
rpc method args = object
  [ "jsonrpc" .= ("2.0" :: String)
  , "id"      .= Number 1
  , "method"  .= method
  , "params"  .= args
  ]

class ToRPC a where
  toRPC :: a -> String

instance ToRPC Addr where
  toRPC = showAddrWith0x

instance ToRPC W256 where
  toRPC = show

instance ToRPC BlockNumber where
  toRPC Latest          = "latest"
  toRPC (BlockNumber n) = show n

readText :: Read a => Text -> a
readText = read . unpack

fetchQuery
  :: Show a
  => BlockNumber
  -> (Value -> IO (Maybe Text))
  -> RpcQuery a
  -> IO (Maybe a)
fetchQuery n f q = do
  x <- case q of
    QueryCode addr ->
      fmap hexText  <$>
        f (rpc "eth_getCode" [toRPC addr, toRPC n])
    QueryNonce addr ->
      fmap readText <$>
        f (rpc "eth_getTransactionCount" [toRPC addr, toRPC n])
    QueryBalance addr ->
      fmap readText <$>
        f (rpc "eth_getBalance" [toRPC addr, toRPC n])
    QuerySlot addr slot ->
      fmap readText <$>
        f (rpc "eth_getStorageAt" [toRPC addr, toRPC slot, toRPC n])
    QueryChainId ->
      fmap readText <$>
        f (rpc "eth_chainId" [toRPC n])
  return x

fetchWithSession :: Text -> Session -> Value -> IO (Maybe Text)
fetchWithSession url sess x = do
  r <- asValue =<< Session.post sess (unpack url) x
  return (r ^? responseBody . key "result" . _String)

fetchContractWithSession
  :: BlockNumber -> Text -> Session -> Addr -> IO (Maybe Contract)
fetchContractWithSession n url sess addr = runMaybeT $ do
  let
    fetch :: Show a => RpcQuery a -> IO (Maybe a)
    fetch = fetchQuery n (fetchWithSession url sess)

  theCode    <- MaybeT $ fetch (QueryCode addr)
  theNonce   <- MaybeT $ fetch (QueryNonce addr)
  theBalance <- MaybeT $ fetch (QueryBalance addr)

  return $
    initialContract (EVM.RuntimeCode theCode)
      & set nonce    (w256 theNonce)
      & set balance  (w256 theBalance)
      & set external True

fetchSlotWithSession
  :: BlockNumber -> Text -> Session -> Addr -> W256 -> IO (Maybe Word)
fetchSlotWithSession n url sess addr slot =
  fmap w256 <$>
    fetchQuery n (fetchWithSession url sess) (QuerySlot addr slot)

fetchContractFrom :: BlockNumber -> Text -> Addr -> IO (Maybe Contract)
fetchContractFrom n url addr =
  Session.withAPISession
    (flip (fetchContractWithSession n url) addr)

fetchSlotFrom :: BlockNumber -> Text -> Addr -> W256 -> IO (Maybe Word)
fetchSlotFrom n url addr slot =
  Session.withAPISession
    (\s -> fetchSlotWithSession n url s addr slot)


-- more like http + z3 now
http :: BlockNumber -> Text -> EVM.Query -> IO (EVM ())
http n url q =
  case q of
    EVM.PleaseFetchContract addr continue ->
      fetchContractFrom n url addr >>= \case
        Just x  ->
          return (continue x)
        Nothing -> error ("oracle error: " ++ show q)
    EVM.PleaseFetchSlot addr slot continue ->
      fetchSlotFrom n url addr (fromIntegral slot) >>= \case
        Just x  -> return (continue x)
        Nothing -> error ("oracle error: " ++ show q)
      

zero :: Monad m => EVM.Query -> m (EVM ())
zero q =
  case q of
    EVM.PleaseFetchContract _ continue ->
      return (continue (initialContract (EVM.RuntimeCode mempty)))
    EVM.PleaseFetchSlot _ _ continue ->
      return (continue 0)
    EVM.PleaseAskSMT jumpcondition pathconds continue ->
      return $ continue EVM.Unknown



type Fetcher = EVM.Query -> IO (EVM ())
