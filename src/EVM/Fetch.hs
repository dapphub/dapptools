{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}

module EVM.Fetch where

import Prelude hiding (Word)

import EVM.Types    (Addr, W256, showAddrWith0x, showWordWith0x, hexText)
import EVM.Concrete (Word, w256)
import EVM          (EVM, Contract, initialContract, nonce, balance, external)

import qualified EVM as EVM

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
  toRPC = showWordWith0x

readText :: Read a => Text -> a
readText = read . unpack

fetchQuery :: Show a => (Value -> IO (Maybe Text)) -> RpcQuery a -> IO (Maybe a)
fetchQuery f q = do
  x <- case q of
    QueryCode addr -> do
      fmap hexText  <$>
        f (rpc "eth_getCode" [toRPC addr, "latest"])
    QueryNonce addr ->
      fmap readText <$>
        f (rpc "eth_getTransactionCount" [toRPC addr, "latest"])
    QueryBalance addr ->
      fmap readText <$>
        f (rpc "eth_getBalance" [toRPC addr, "latest"])
    QuerySlot addr slot ->
      fmap readText <$>
        f (rpc "eth_getStorageAt" [toRPC addr, toRPC slot, "latest"])
  return x

fetchWithSession :: Text -> Session -> Value -> IO (Maybe Text)
fetchWithSession url sess x = do
  r <- asValue =<< Session.post sess (unpack url) x
  return (r ^? responseBody . key "result" . _String)

fetchContractWithSession
  :: Text -> Session -> Addr -> IO (Maybe Contract)
fetchContractWithSession url sess addr = runMaybeT $ do
  let
    fetch :: Show a => RpcQuery a -> IO (Maybe a)
    fetch = fetchQuery (fetchWithSession url sess)

  theCode    <- MaybeT $ fetch (QueryCode addr)
  theNonce   <- MaybeT $ fetch (QueryNonce addr)
  theBalance <- MaybeT $ fetch (QueryBalance addr)

  return $
    initialContract theCode
      & set nonce    (w256 theNonce)
      & set balance  (w256 theBalance)
      & set external True

fetchSlotWithSession
  :: Text -> Session -> Addr -> W256 -> IO (Maybe Word)
fetchSlotWithSession url sess addr slot = do
  fmap w256 <$>
    fetchQuery (fetchWithSession url sess) (QuerySlot addr slot)

fetchContractFrom :: Text -> Addr -> IO (Maybe Contract)
fetchContractFrom url addr =
  Session.withAPISession
    (flip (fetchContractWithSession url) addr)

fetchSlotFrom :: Text -> Addr -> W256 -> IO (Maybe Word)
fetchSlotFrom url addr slot =
  Session.withAPISession
    (\s -> fetchSlotWithSession url s addr slot)

http :: Text -> EVM.Query -> IO (EVM ())
http url q = do
  case q of
    EVM.PleaseFetchContract addr continue ->
      fetchContractFrom url addr >>= \case
        Just x  -> do
          return (continue x)
        Nothing -> error ("oracle error: " ++ show q)
    EVM.PleaseFetchSlot addr slot continue ->
      fetchSlotFrom url addr (fromIntegral slot) >>= \case
        Just x  -> return (continue x)
        Nothing -> error ("oracle error: " ++ show q)

zero :: Monad m => EVM.Query -> m (EVM ())
zero q = do
  case q of
    EVM.PleaseFetchContract _ continue ->
      return (continue (initialContract ""))
    EVM.PleaseFetchSlot _ _ continue ->
      return (continue 0)

type Fetcher = EVM.Query -> IO (EVM ())
