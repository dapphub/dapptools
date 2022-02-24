{-# Language DataKinds #-}

{-|
   Helper functions for working with Expr instances
-}
module EVM.Expr where

import Prelude hiding (LT)

import EVM.Types
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

-- | Extracts the byte at a given index from a Buf.
--
-- We do our best to return a concrete value wherever possible, but fallback to
-- an abstract expresion if nescessary. Note that a Buf is an infinite
-- structure, so reads outside of the bounds of a ConcreteBuf return 0. This is
-- inline with the semantics of calldata and memory, but not of returndata.
index :: Expr EWord -> Expr Buf -> Expr Byte
index _ EmptyBuf = LitByte 0x0
index i AbstractBuf = ReadByte i AbstractBuf

-- reads from concrete indices
index (Lit x) (ConcreteBuf b)
  = if fromIntegral x < BS.length b
    then LitByte (BS.index b (fromIntegral x))
    else LitByte 0x0
index i@(Lit x) (WriteByte (Lit idx) val src)
  = if fromIntegral x == idx
    then val
    else index i src
index i@(Lit x) (WriteWord (Lit idx) val src)
  = if fromIntegral x <= idx && idx < fromIntegral (x + 8)
    then Index (litByte $ idx - fromIntegral x) val -- TODO: pull a concrete value here if we can
    else index i src
index i@(Lit x) (CopySlice (Lit dstOffset) (Lit srcOffset) (Lit size) src dst)
  = if dstOffset <= fromIntegral x && fromIntegral x < (dstOffset + size)
    then index (Lit $ fromIntegral x - (dstOffset - srcOffset)) src
    else index i dst

-- we can ignore some partially symbolic CopySlice's if x is not within the region written to dst
index i@(Lit x) buf@(CopySlice (Lit dstOffset) (Lit size) _ _ dst)
  = if fromIntegral x < dstOffset || dstOffset + size < fromIntegral x
    then index i dst
    else ReadByte (lit x) buf
index i@(Lit x) buf@(CopySlice (Lit dstOffset) _ _ _ dst)
  = if fromIntegral x < dstOffset
    then index i dst
    else ReadByte (lit x) buf

-- if we have writes to abstract indices, then return a ReadByte expr
index i buf = ReadByte i buf


-- | Reads the word at the given slot from the given storage expression.
--
-- Reads from storage that are backed by Empty or Concrete stores will always
-- return 0x0 if there have not been any writes at the requested slot, in the
-- case of an AbstractStore we return a symbolic value.
readStorage :: Expr Storage -> Expr EWord -> Expr EWord
readStorage EmptyStore _ = Lit 0x0
readStorage store@(ConcreteStore s) loc = case loc of
  Lit l -> case Map.lookup l s of
                 Just v -> Lit v
                 Nothing -> Lit 0x0
  _ -> SLoad loc store
readStorage s@AbstractStore loc = SLoad loc s
readStorage (SStore slot val prev) loc = if loc == slot then val else readStorage prev loc


-- | Writes a value to a key in a storage expression.
--
-- Concrete writes on top of a concrete or empty store will produce a new
-- ConcreteStore, otherwise we add a new write to the storage expression.
writeStorage :: Expr EWord -> Expr EWord -> Expr Storage -> Expr Storage
writeStorage k@(Lit key) v@(Lit val) store = case store of
  EmptyStore -> ConcreteStore (Map.singleton key val)
  ConcreteStore s -> ConcreteStore (Map.insert key val s)
  _ -> SStore k v store
writeStorage key val store = SStore key val store
