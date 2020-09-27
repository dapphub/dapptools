{-# Language TemplateHaskell #-}

module EVM.Serve where

import Prelude hiding (Word)

import qualified EVM

import Control.Lens

import Network.Wai (responseLBS, Application, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy   as LazyByteString

data Request = Request
  { _id      :: Int
  , _jsonrpc :: String
  , _method  :: String
  , _params  :: [String]
  }

makeLenses ''Request

instance FromJSON Request where
  parseJSON (JSON.Object v) = Request
    <$> v .: "id"
    <*> v .: "jsonrpc"
    <*> v .: "method"
    <*> v .: "params"
  parseJSON invalid =
    JSON.typeMismatch "bad rpc request" invalid


serve :: Application -> IO ()
serve app = do
  let port = 3000
  putStrLn $ "Hevm rpc listening on port " ++ show port
  run port app
