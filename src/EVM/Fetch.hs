{-# Language GADTs #-}

module EVM.Fetch where

import EVM.Types (Addr, W256, showAddrWith0x, showWordWith0x, hexText)

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Network.Wreq

import qualified Network.Wreq.Session as Session

data Query a where
  QueryCode    :: Addr         -> Query ByteString
  QueryBalance :: Addr         -> Query W256
  QueryNonce   :: Addr         -> Query W256
  QuerySlot    :: Addr -> W256 -> Query W256

-- Will of course allow custom RPC URLs e.g. to include Infura access key
mainnet :: String
mainnet = "https://mainnet.infura.io"

mkr :: Addr
mkr = 0xc66ea802717bfb9833400264dd12c2bceaa34a6d

request :: String -> [String] -> Value
request method args = object
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

fetchQuery :: ((Text -> a) -> Value -> IO (Maybe a)) -> Query a -> IO (Maybe a)
fetchQuery f = do
  \case
    QueryCode addr ->
      f hexText (request "eth_getCode" [toRPC addr, "latest"])
    QueryNonce addr ->
      f readText (request "eth_getTransactionCount" [toRPC addr, "latest"])
    QueryBalance addr ->
      f readText (request "eth_getBalance" [toRPC addr, "latest"])
    QuerySlot addr slot ->
      f readText (request "eth_getStorageAt" [toRPC addr, toRPC slot, "latest"])

-- demo kludge
fetch :: IO ()
fetch = Session.withAPISession $ \sess -> do
  let
    foo :: (Text -> a) -> Value -> IO (Maybe a)
    foo f x =
      do
        r <- asValue =<< Session.post sess mainnet x
        return (r ^? responseBody . key "result" . _String . to f)

  print =<< fetchQuery foo (QueryCode mkr)
  print =<< fetchQuery foo (QueryNonce mkr)
  print =<< fetchQuery foo (QueryBalance mkr)
  print =<< fetchQuery foo (QuerySlot mkr 0)
