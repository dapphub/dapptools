{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module EVM.Patricia where

import EVM.RLP
import EVM.Types hiding (Literal)

import Control.Monad.Free
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (stripPrefix)
import Data.Sequence (Seq)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Sequence as Seq

data KV k v a
  = Put k v a
  | Get k (v -> a)
  deriving (Functor)

newtype DB k v a = DB (Free (KV k v) a)
  deriving (Functor, Applicative, Monad)

insertDB :: k -> v -> DB k v ()
insertDB k v = DB $ liftF $ Put k v ()

lookupDB :: k -> DB k v v
lookupDB k = DB $ liftF $ Get k id

-- Collapses a series of puts and gets down to the monad of your choice
runDB :: Monad m
      => (k -> v -> m ()) -- ^ The 'put' function for our desired monad
      -> (k -> m v)       -- ^ The 'get' function for the same monad
      -> DB k v a         -- ^ The puts and gets to execute
      -> m a
runDB putt gett (DB ops) = go ops
  where
    go (Pure a) = return a
    go (Free (Put k v next)) = putt k v >> go next
    go (Free (Get k handler)) = gett k >>= go . handler

type Path = [Nibble]

data Ref = Hash ByteString | Literal Node
  deriving (Eq)

instance Show Ref where
  show (Hash d) = show (ByteStringS d)
  show (Literal n) = show n

data Node = Empty
          | Shortcut Path (Either Ref ByteString)
          | Full (Seq Ref) ByteString
  deriving (Show, Eq)

-- the function HP from Appendix C of yellow paper
encodePath :: Path -> Bool -> ByteString
encodePath p isTerminal | even (length p)
  = packNibbles $ Nibble flag : Nibble 0 : p
                        | otherwise
  = packNibbles $ Nibble (flag + 1) : p
  where flag  = if isTerminal then 2 else 0

rlpRef :: Ref -> RLP
rlpRef (Hash d) = BS d
rlpRef (Literal n) = rlpNode n

rlpNode :: Node -> RLP
rlpNode Empty = BS mempty
rlpNode (Shortcut path (Right val)) = List [BS $ encodePath path True, BS val]
rlpNode (Shortcut path (Left  ref)) = List [BS $ encodePath path False, rlpRef ref]
rlpNode (Full refs val) = List $ toList (fmap rlpRef refs) <> [BS val]

type NodeDB = DB ByteString Node

instance Show (NodeDB Node) where
  show = show

putNode :: Node -> NodeDB Ref
putNode node =
  let bytes = rlpencode $ rlpNode node
      digest = word256Bytes $ keccak bytes
  in if BS.length bytes < 32
    then return $ Literal node
    else do
      insertDB digest node
      return $ Hash digest

getNode :: Ref -> NodeDB Node
getNode (Hash d) = lookupDB d
getNode (Literal n) = return n

lookupPath :: Ref -> Path -> NodeDB ByteString
lookupPath root path = getNode root >>= getVal path

getVal :: Path -> Node -> NodeDB ByteString
getVal _ Empty = return BS.empty
getVal path (Shortcut nodePath ref) =
  case (stripPrefix nodePath path, ref) of
    (Just [], Right value) -> return value
    (Just remaining, Left key) -> lookupPath key remaining
    _ -> return BS.empty

getVal [] (Full _ val) = return val
getVal (p:ps) (Full refs _) = lookupPath (refs `Seq.index` (num p)) ps

emptyRef :: Ref
emptyRef = Literal Empty

emptyRefs :: Seq Ref
emptyRefs = Seq.replicate 16 emptyRef

addPrefix :: Path -> Node -> NodeDB Node
addPrefix _ Empty = return Empty
addPrefix [] node = return node
addPrefix path (Shortcut p v) = return $ Shortcut (path <> p) v
addPrefix path n = Shortcut path . Left <$> putNode n

insertRef :: Ref -> Path -> ByteString -> NodeDB Ref
insertRef ref p val = do root <- getNode ref
                         newNode <- if val == BS.empty
                                    then delete root p
                                    else update root p val
                         putNode newNode

update :: Node -> Path -> ByteString -> NodeDB Node
update Empty p new  = return $ Shortcut p (Right new)
update (Full refs _) [] new = return (Full refs new)
update (Full refs old) (p:ps) new = do
  newRef <- insertRef (refs `Seq.index` (num p)) ps new
  return $ Full (Seq.update (num p) newRef refs) old
update (Shortcut (o:os) (Right old)) [] new = do
  newRef <- insertRef emptyRef os old
  return $ Full (Seq.update (num o) newRef emptyRefs) new
update (Shortcut [] (Right old)) (p:ps) new = do
  newRef <- insertRef emptyRef ps new
  return $ Full (Seq.update (num p) newRef emptyRefs) old
update (Shortcut [] (Right _)) [] new =
  return $ Shortcut [] (Right new)
update (Shortcut (o:os) to) (p:ps) new | o == p
  = update (Shortcut os to) ps new >>= addPrefix [o]
                                       | otherwise = do
  oldRef <- case to of
              (Left ref)  -> getNode ref >>= addPrefix os >>= putNode
              (Right val) -> insertRef emptyRef os val
  newRef <- insertRef emptyRef ps new
  let refs = Seq.update (num p) newRef $ Seq.update (num o) oldRef emptyRefs
  return $ Full refs BS.empty
update (Shortcut (o:os) (Left ref)) [] new = do
  newRef <- getNode ref >>= addPrefix os >>= putNode
  return $ Full (Seq.update (num o) newRef emptyRefs) new
update (Shortcut cut (Left ref)) ps new = do
  newRef <- insertRef ref ps new
  return $ Shortcut cut (Left newRef)

delete :: Node -> Path -> NodeDB Node
delete Empty _ = return Empty
delete (Shortcut [] (Right _)) [] = return Empty
delete n@(Shortcut [] (Right _)) _ = return n
delete (Shortcut [] (Left ref)) p = do node <- getNode ref
                                       delete node p
delete n@(Shortcut _ _) [] = return n
delete n@(Shortcut (o:os) to) (p:ps) | p == o
  = delete (Shortcut os to) ps >>= addPrefix [o]
                                     | otherwise
  = return n
delete (Full refs _) [] | refs == emptyRefs
  = return Empty
                        | otherwise
  = return (Full refs BS.empty)
delete (Full refs val) (p:ps) = do
  newRef <- insertRef (refs `Seq.index` (num p)) ps BS.empty
  let newRefs = Seq.update (num p) newRef refs
      nonEmpties = filter (\(_, ref) -> ref /= emptyRef) $ zip [0..15] $ toList newRefs
  case (nonEmpties, BS.null val) of
    ([], True)         -> return Empty
    ([(n, ref)], True)  -> getNode ref >>= addPrefix [Nibble n]
    _                    -> return $ Full newRefs val

insert :: Ref -> ByteString -> ByteString -> NodeDB Ref
insert ref key = insertRef ref (unpackNibbles key)

lookupIn :: Ref -> ByteString -> NodeDB ByteString
lookupIn ref bs = lookupPath ref $ unpackNibbles bs

type Trie = StateT Ref NodeDB

runTrie :: DB ByteString ByteString a -> Trie a
runTrie = runDB putDB getDB
  where
    putDB key val = do
      ref <- get
      newRef <- lift $ insert ref key val
      put newRef
    getDB key = do
      ref <- get
      lift $ lookupIn ref key

type MapDB k v a = StateT (Map.Map k v) Maybe a

runMapDB :: Ord k => DB k v a -> MapDB k v a
runMapDB = runDB putDB getDB
  where
    getDB key = do
      mmap <- get
      lift $ Map.lookup key mmap
    putDB key value = do
      mmap <- get
      let newMap = Map.insert key value mmap
      put newMap


insertValues :: [(ByteString, ByteString)] -> Maybe Ref
insertValues inputs =
  let trie = runTrie $ mapM_ insertPair inputs
      mapDB = runMapDB $ runStateT trie (Literal Empty)
      result = snd <$> evalStateT mapDB Map.empty
      insertPair (key, value) = insertDB key value
  in result

calcRoot :: [(ByteString, ByteString)] -> Maybe ByteString
calcRoot vs = case insertValues vs of
     Just (Hash b) -> Just b
     Just (Literal n) -> Just $ word256Bytes $ keccak $ rlpencode $ rlpNode n
     Nothing -> Nothing
