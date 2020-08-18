{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}

module EVM.Fetch where

import Prelude hiding (Word)

import EVM.Types    (Addr, W256, hexText)
import EVM.Concrete (Word, w256)
import EVM          (EVM, Contract, StorageModel, initialContract, nonce, balance, external)

import qualified EVM

import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.SBV.Trans.Control
import qualified Data.SBV.Internals as SBV
import Data.SBV.Trans hiding (Word)
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
  toRPC = show

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
  :: BlockNumber -> Text -> Addr -> Session -> IO (Maybe Contract)
fetchContractWithSession n url addr sess = runMaybeT $ do
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
    (fetchContractWithSession n url addr)

fetchSlotFrom :: BlockNumber -> Text -> Addr -> W256 -> IO (Maybe Word)
fetchSlotFrom n url addr slot =
  Session.withAPISession
    (\s -> fetchSlotWithSession n url s addr slot)

http :: BlockNumber -> Text -> Fetcher
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
    EVM.PleaseAskSMT _ _ _ -> error "smt calls not available for this oracle"

zero :: Fetcher
zero q =
  case q of
    EVM.PleaseFetchContract _ continue ->
      return (continue (initialContract (EVM.RuntimeCode mempty)))
    EVM.PleaseFetchSlot _ _ continue ->
      return (continue 0)
    EVM.PleaseAskSMT _ _ continue ->
      return $ continue EVM.Unknown



type Fetcher = EVM.Query -> IO (EVM ())

-- smtsolving + (http or zero)
oracle :: SBV.State -> Maybe (BlockNumber, Text) -> StorageModel -> Fetcher
oracle state info model q = do
  case q of
    EVM.PleaseAskSMT jumpcondition pathconditions continue ->
      flip runReaderT state $ SBV.runQueryT $ do
         let pathconds = sAnd pathconditions
         noJump <- checksat $ pathconds .&& jumpcondition ./= 0
         case noJump of
            -- Unsat means condition
            -- cannot be nonzero
            Unsat -> do jump <- checksat $ pathconds .&& jumpcondition .== 0
                        -- can it be zero?
                        case jump of
                          -- No. We are on an inconsistent path.
                          Unsat -> return $ continue EVM.Inconsistent
                          -- Yes. It must be 0.
                          Sat -> return $ continue (EVM.Iszero True)
                          -- Assume 0 is still possible.
                          Unk -> return $ continue (EVM.Iszero True)
            -- Sat means its possible for condition
            -- to be nonzero.
            Sat -> do jump <- checksat $ pathconds .&& jumpcondition .== 0
                      -- can it also be zero?
                      case jump of
                        -- No. It must be nonzero
                        Unsat -> return $ continue (EVM.Iszero False)
                        -- Yes. Both branches possible
                        Sat -> return $ continue EVM.Unknown
                        -- Explore both branches in case of timeout
                        Unk -> return $ continue EVM.Unknown

            -- If the query times out, we simply explore both paths
            Unk -> return $ continue EVM.Unknown
    -- if we are using a symbolic storage model,
    -- we generate a new array to the fetched contract here
    EVM.PleaseFetchContract addr continue -> do
      contract <- case info of
                    Nothing -> return $ Just (initialContract (EVM.RuntimeCode mempty))
                    Just (n, url) -> fetchContractFrom n url addr
      case contract of
        Just x  -> case model of
          EVM.ConcreteS -> return $ continue x
          EVM.InitialS  -> return $ continue $ x & set EVM.storage (EVM.Symbolic $ SBV.sListArray 0 [])
          EVM.SymbolicS -> 
            flip runReaderT state $ SBV.runQueryT $ do
              store <- freshArray_ Nothing
              return $ continue $ x & set EVM.storage (EVM.Symbolic store)
        Nothing -> error ("oracle error: " ++ show q)

    --- for other queries (there's only slot left right now) we default to zero or http
    _ -> case info of
      Nothing -> zero q
      Just (n, url) -> http n url q

checksat :: SBool -> Query CheckSatResult
checksat b = do resetAssertions
                constrain b
                m <- checkSat
                resetAssertions
                return m
