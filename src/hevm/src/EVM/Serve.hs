{-# Language TemplateHaskell #-}
{-# Language DeriveAnyClass #-}
{-# Language DuplicateRecordFields #-}

module EVM.Serve where

import Prelude hiding (Word, id)
--import Data.Maybe

import qualified EVM
import EVM.Concrete
import EVM.Symbolic
import EVM.Types
import EVM.Fetch
import Control.Lens
import GHC.Generics

import Network.Wai (responseLBS, Application, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Data.Aeson
import Data.Text (Text, pack, unpack)
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

rpcServer :: EVM.VM -> Text -> Application
rpcServer vm url request respond = do
  req <- getRequestBodyChunk request
  case decode $ LazyByteString.fromStrict req of
    Nothing -> do
      putStrLn (show req)
      respond $ responseLBS status200 [(hContentType, "text/plain")]
              $ LazyByteString.fromStrict "sad"

    Just r -> do
      result <- case method r of
            "net_version" ->
              return $ String . pack $ show (num $ wordValue $ view (EVM.env . EVM.chainId) vm :: Integer)

            "eth_getBlockByNumber" ->
              return $ String $ pack $ show $ view (EVM.block . EVM.number) vm

            "eth_getBalance" -> do
              let Just addr = read . unpack <$> params r ^? ix 0 . _String
              c <- getContract addr
              return $ String . pack $ show (view EVM.balance c)

            "eth_getTransactionCount" -> do
              let Just addr = read . unpack <$> params r ^? ix 0 . _String
              c <- getContract addr
              return $ String . pack $ show (view EVM.nonce c)

            "eth_getStorageAt" -> do
              let Just addr = read . unpack <$> params r ^? ix 0 . _String
                  Just loc  = read . unpack <$> params r ^? ix 1 . _String
              val <- getSlot addr loc
              return $ String . pack $ show val

            "eth_getCode" -> do
              let Just addr = read . unpack <$> params r ^? ix 0 . _String
              EVM.RuntimeCode c <- view EVM.contractcode <$> getContract addr
              return $ String . pack $ show (ByteStringS c)

            _ -> return $ "hmm?"

      let answer = Response (getId r) (getJsonRpc r) result

      putStrLn (show req)
      respond
        $ responseLBS status200 [(hContentType, "application/json")]
        $ encode answer

  where
    -- TODO: optimize getBalance, getCode
    getContract :: Addr -> IO EVM.Contract
    getContract addr =
      case view (EVM.env . EVM.contracts . at addr) vm of
        Just c -> return c
        Nothing -> do Just c <- fetchContractFrom Latest url addr
                      return c
        
    getSlot :: Addr -> Word -> IO SymWord
    getSlot addr loc =
      case EVM.readStorage (view (EVM.env . EVM.contracts . at addr . non EVM.newAccount . EVM.storage) vm) (litWord loc) of
        Just val -> return val
        Nothing -> do Just c <- fetchSlotFrom Latest url addr (wordValue loc)
                      return (litWord c)
        
