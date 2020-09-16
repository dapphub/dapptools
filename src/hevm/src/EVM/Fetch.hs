{-# Language GADTs #-}
{-# Language StandaloneDeriving #-}
{-# Language LambdaCase #-}

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
http n url = oracle Nothing (Just (n, url)) EVM.ConcreteS True

-- smtsolving + (http or zero)
oracle :: Maybe SBV.State -> Maybe (BlockNumber, Text) -> StorageModel -> Bool -> Fetcher
oracle smtstate info model ensureConsistency q = do
  case q of
    EVM.PleaseAskSMT branchcondition pathconditions continue ->
      case smtstate of
        Nothing -> return $ continue EVM.Unknown
        Just state -> flip runReaderT state $ SBV.runQueryT $ do
         let pathconds = sAnd pathconditions
         -- Is is possible to satisfy the condition?
         continue <$> checkBranch pathconds branchcondition ensureConsistency

    -- if we are using a symbolic storage model,
    -- we generate a new array to the fetched contract here
    EVM.PleaseFetchContract addr continue -> do
      contract <- case info of
                    Nothing -> return $ Just $ initialContract (EVM.RuntimeCode mempty)
                    Just (n, url) -> fetchContractFrom n url addr
      case contract of
        Just x -> case model of
          EVM.ConcreteS -> return $ continue x
          EVM.InitialS  -> return $ continue $ x
             & set EVM.storage (EVM.Symbolic $ SBV.sListArray 0 [])
             & set EVM.origStorage (EVM.Symbolic $ SBV.sListArray 0 [])
          EVM.SymbolicS -> case smtstate of
            Nothing -> return (continue $ x
                               & set EVM.storage (EVM.Symbolic $ SBV.sListArray 0 [])
                               & set EVM.origStorage (EVM.Symbolic $ SBV.sListArray 0 []))

            Just state -> 
              flip runReaderT state $ SBV.runQueryT $ do
                store <- freshArray_ Nothing
                return $ continue $ x
                  & set EVM.storage (EVM.Symbolic store)
                  & set EVM.origStorage (EVM.Symbolic store)
        Nothing -> error ("oracle error: " ++ show q)

    --- for other queries (there's only slot left right now) we default to zero or http
    EVM.PleaseFetchSlot addr slot continue ->
      case info of
        Nothing -> return (continue 0)
        Just (n, url) ->
         fetchSlotFrom n url addr (fromIntegral slot) >>= \case
           Just x  -> return (continue x)
           Nothing ->
             error ("oracle error: " ++ show q)

zero :: Fetcher
zero = oracle Nothing Nothing EVM.ConcreteS True

type Fetcher = EVM.Query -> IO (EVM ())

checksat :: SBool -> Query CheckSatResult
checksat b = do push 1
                constrain b
                b <- getInfo Name
                m <- case b of
                       -- some custom strategies for z3 which have proven to be quite useful (can still be tweaked)
                       Resp_Name "Z3" -> checkSatUsing "(check-sat-using (then (using-params simplify :push_ite_bv true :ite_extra_rules true) smt))"
                       _ -> checkSat
                pop 1
                return m

-- | Checks which branches are satisfiable, checking the pathconditions for consistency
-- if the third argument is true.
-- When in debug mode, we do not want to be able to navigate to dead paths,
-- but for normal execution paths with inconsistent pathconditions
-- will be pruned anyway.
checkBranch :: SBool -> SBool -> Bool -> Query EVM.BranchCondition
checkBranch pathconds branchcondition False = do
  constrain pathconds
  checksat branchcondition >>= \case
     -- the condition is unsatisfiable
     Unsat -> -- if pathconditions are consistent then the condition must be false
            return $ EVM.Case False
     -- Sat means its possible for condition to hold
     Sat -> -- is its negation also possible?
            checksat (sNot branchcondition) >>= \case
               -- No. The condition must hold
               Unsat -> return $ EVM.Case True
               -- Yes. Both branches possible
               Sat -> return EVM.Unknown
               -- Explore both branches in case of timeout
               Unk -> return EVM.Unknown
     -- If the query times out, we simply explore both paths
     Unk -> return EVM.Unknown

checkBranch pathconds branchcondition True = do
  constrain pathconds
  checksat branchcondition >>= \case
     -- the condition is unsatisfiable
     Unsat -> -- are the pathconditions even consistent?
              checksat (sNot branchcondition) >>= \case
                -- No. We are on an inconsistent path.
                Unsat -> return EVM.Inconsistent
                -- Yes. The condition must be false.
                Sat -> return $ EVM.Case False
                -- Assume the negated condition is still possible.
                Unk -> return $ EVM.Case False
     -- Sat means its possible for condition to hold
     Sat -> -- is its negation also possible?
            checksat (sNot branchcondition) >>= \case
               -- No. The condition must hold
               Unsat -> return $ EVM.Case True
               -- Yes. Both branches possible
               Sat -> return EVM.Unknown
               -- Explore both branches in case of timeout
               Unk -> return EVM.Unknown

     -- If the query times out, we simply explore both paths
     Unk -> return EVM.Unknown
