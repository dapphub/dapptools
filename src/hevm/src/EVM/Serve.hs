{-# Language TemplateHaskell #-}
{-# Language DeriveAnyClass #-}
{-# Language DuplicateRecordFields #-}

module EVM.Serve where

import Prelude hiding (Word, id)

import qualified EVM
import EVM.Concrete
import EVM.Types
import EVM.VMTest (newAccount)
import Control.Lens
import GHC.Generics

import Network.Wai (responseLBS, Application, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Data.Aeson
import Data.Text (pack, unpack)
--import qualified Data.ByteString.Char8  as Char8
-- import qualified Data.Aeson        as JSON
-- import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy   as LazyByteString
import Data.Aeson.Lens hiding (values)

data Request = Request
  { id      :: Int
  , jsonrpc :: String
  , method  :: String
  , params  :: Array
  } deriving (Show, Generic, FromJSON)

getId :: Request -> Int
getId = id
getJsonRpc :: Request -> String
getJsonRpc = jsonrpc

data Response = Response
  { id      :: Int
  , jsonrpc :: String
  , result  :: Value
  } deriving (Show, Generic, ToJSON)

serve :: Application -> IO ()
serve app = do
  let port = 3000
  putStrLn $ "Hevm rpc listening on port " ++ show port
  run port app

rpcServer :: EVM.VM -> Application
rpcServer vm request respond = do
  req <- getRequestBodyChunk request
  case decode $ LazyByteString.fromStrict req of
    Nothing -> do
      putStrLn (show req)
      respond $ responseLBS status200 [(hContentType, "text/plain")]
              $ LazyByteString.fromStrict "sad"

    Just r -> do
      let result = case method r of
            "eth_getBlockByNumber" ->
              String $ pack $ show $ view (EVM.block . EVM.number) vm
            "net_version" ->
              String . pack $ show (num $ wordValue $ view (EVM.env . EVM.chainId) vm :: Integer)
            "eth_getBalance" ->
              let
                addr :: Addr
                Just addr = read . unpack <$> params r ^? ix 0 . _String
              in String . pack $ show (view (EVM.env . EVM.contracts . ix addr . non newAccount . EVM.balance) vm) -- . non (w256 0)) vm)
            _ ->
              "hmm?"

          answer = Response (getId r) (getJsonRpc r) result
          emptyContract = Response (getId r) (getJsonRpc r) result

      putStrLn (show req)
      respond
        $ responseLBS status200 [(hContentType, "application/json")]
        $ encode answer

