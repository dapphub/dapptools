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
import EVM.Stepper
import EVM.UnitTest

import Control.Lens hiding ((.=), from, to, pre)
import Control.Monad.State.Strict
import GHC.Generics hiding (from, to)

import Network.Wai (responseLBS, Application, getRequestBodyChunk)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.Map as Map
import Data.Text (Text, pack, unpack)
--import qualified Data.ByteString.Char8  as Char8
-- import qualified Data.Aeson        as JSON
import qualified Data.Aeson.Types  as JSON
import qualified Data.ByteString.Lazy   as LazyByteString
import Data.Aeson.Lens hiding (values)

data Request = Request
  { id      :: Int
  , jsonrpc :: String
  , method  :: String
  , params  :: Array
  } deriving (Show, Generic, FromJSON)

data Eth_call = Eth_call
  { from :: Addr
  , to :: Addr
  , txdata :: ByteString
  , gas :: W256
  , gasPrice :: W256
  , value :: W256
  } deriving (Show)

instance FromJSON Eth_call where
  parseJSON (Object val) = do
    tdata    <- dataField val "data"
    toAddr   <- addrField val "to"
    fromAddr <- val .:? "from" .!= (Addr 0)
    gasLimit <- val .:? "gas" .!= 0xffffffff
    gasPrice <- val .:? "gasPrice" .!= 0
    value <- val .:? "value" .!= 0
    return $ Eth_call fromAddr toAddr tdata gasLimit gasPrice value
  parseJSON invalid =
    JSON.typeMismatch "Eth_call" invalid

instance ToJSON Eth_call where
  toJSON (Eth_call {..}) =
    object [ "from" .= show from
           , "to" .= show to
           , "data" .= show txdata
           , "gas" .= show gas
           , "gasPrice" .= show gasPrice
           , "value" .= show value
           ]

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

            "eth_call" -> do
              let Just Eth_call{..} = params r ^? ix 0 . _JSON --- . _Object
              c <- getContract from
              let pre = execState (call from to (ConcreteBuffer txdata) gas gasPrice value) (vm & over (EVM.env . EVM.contracts) (Map.insert from c))
              post <- run' pre
              case view EVM.result post of
                Nothing ->
                  error "internal error; no EVM result"
                Just (EVM.VMFailure (EVM.Revert msg)) -> return $ String $ pack $  show $ ByteStringS msg
                Just (EVM.VMFailure _) -> return $ "0x"
                Just (EVM.VMSuccess (ConcreteBuffer msg)) -> return $ String $ pack $ show $ ByteStringS msg
                Just (EVM.VMSuccess (SymbolicBuffer msg)) -> return $ String $ pack $ show $ msg

            _ -> return $ "hmm?"

      let answer = Response (getId r) (getJsonRpc r) result

      putStrLn (show req)
      respond
        $ responseLBS status200 [(hContentType, "application/json")]
        $ encode answer

  where
    run' :: EVM.VM -> IO (EVM.VM)
    run' = execStateT (interpret fetcher . void $ EVM.Stepper.execFully)
    fetcher = http Latest url
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
        
