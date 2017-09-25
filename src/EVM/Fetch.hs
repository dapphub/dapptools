{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}

module EVM.Fetch where

import Prelude hiding (Word)

import EVM.Types    (Addr, W256, showAddrWith0x, showWordWith0x, hexText)
import EVM.Machine  (Machine, Word, w256)
import EVM.Concrete (Concrete)
import EVM          (Contract, initialContract, nonce, balance, external)

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

-- Will of course allow custom RPC URLs e.g. to include Infura access key
mainnet :: String
mainnet = "https://mainnet.infura.io"

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
  print q
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
  putStrLn (" => " ++ show x)
  return x

fetchWithSession :: Session -> Value -> IO (Maybe Text)
fetchWithSession sess x = do
  r <- asValue =<< Session.post sess mainnet x
  return (r ^? responseBody . key "result" . _String)

fetchContractWithSession
  :: Machine e
  => Session -> Addr -> IO (Maybe (Contract e))
fetchContractWithSession sess addr = runMaybeT $ do
  let
    fetch :: Show a => RpcQuery a -> IO (Maybe a)
    fetch = fetchQuery (fetchWithSession sess)

  theCode    <- MaybeT $ fetch (QueryCode addr)
  theNonce   <- MaybeT $ fetch (QueryNonce addr)
  theBalance <- MaybeT $ fetch (QueryBalance addr)

  return $
    initialContract theCode
      & set nonce    (w256 theNonce)
      & set balance  (w256 theBalance)
      & set external True

fetchSlotWithSession
  :: Machine e
  => Session -> Addr -> W256 -> IO (Maybe (Word e))
fetchSlotWithSession sess addr slot = do
  fmap w256 <$>
    fetchQuery (fetchWithSession sess) (QuerySlot addr slot)

testFetchContract :: Addr -> IO (Maybe (Contract Concrete))
testFetchContract addr =
  Session.withAPISession
    (flip fetchContractWithSession addr)

testFetchSlot :: Addr -> W256 -> IO (Maybe (Word Concrete))
testFetchSlot addr slot =
  Session.withAPISession
    (\s -> fetchSlotWithSession s addr slot)
