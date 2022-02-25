{-# Language DataKinds #-}

{-|
   Helper functions for working with Expr instances.
   All functions here will return a concrete result if given a concrete input.
-}
module EVM.Expr where

import Prelude hiding (LT, GT)
import Data.Bits

import EVM.Types
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map


-- ** Stack Ops ** --------------------------------------------------------------------------------


op1 :: (Expr EWord -> Expr EWord)
    -> (W256 -> W256)
    -> Expr EWord -> Expr EWord
op1 _ concrete (Lit x) = Lit (concrete x)
op1 symbolic _ x = symbolic x

op2 :: (Expr EWord -> Expr EWord -> Expr EWord)
    -> (W256 -> W256 -> W256)
    -> Expr EWord -> Expr EWord -> Expr EWord
op2 _ concrete (Lit x) (Lit y) = Lit (concrete x y)
op2 symbolic _ x y = symbolic x y

op3 :: (Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord)
    -> (W256 -> W256 -> W256 -> W256)
    -> Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
op3 _ concrete (Lit x) (Lit y) (Lit z) = Lit (concrete x y z)
op3 symbolic _ x y z = symbolic x y z

-- Integers

add :: Expr EWord -> Expr EWord -> Expr EWord
add = op2 Add (+)

sub :: Expr EWord -> Expr EWord -> Expr EWord
sub = op2 Sub (-)

mul :: Expr EWord -> Expr EWord -> Expr EWord
mul = op2 Mul (*)

div :: Expr EWord -> Expr EWord -> Expr EWord
div = op2 Div Prelude.div

sdiv :: Expr EWord -> Expr EWord -> Expr EWord
sdiv = op2 SDiv (\x y -> let sx, sy :: Integer
                             sx = fromIntegral x
                             sy = fromIntegral y
                         in fromIntegral (sx `quot` sy))

mod :: Expr EWord -> Expr EWord -> Expr EWord
mod = op2 Mod (\x y -> if y == 0 then 0 else x `Prelude.mod` y)

smod :: Expr EWord -> Expr EWord -> Expr EWord
smod = op2 SMod (\x y ->
  let sx, sy :: Integer
      sx = fromIntegral x
      sy = fromIntegral y
  in if y == 0
     then 0
     else fromIntegral (sx `rem` sy))

addmod :: Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
addmod = op3 AddMod (\x y z ->
  if z == 0
  then 0
  else fromIntegral $ ((to512 x) + (to512 y)) `Prelude.mod` (to512 z))

mulmod :: Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
mulmod = op3 MulMod (\x y z ->
   if z == 0
   then 0
   else fromIntegral $ ((to512 x) * (to512 y)) `Prelude.mod` (to512 z))

exp :: Expr EWord -> Expr EWord -> Expr EWord
exp = op2 Exp (^)

sex :: Expr EWord -> Expr EWord -> Expr EWord
sex = op2 SEx (\bytes x ->
  if bytes >= 32 then x
  else let n = num bytes * 8 + 7 in
    if testBit x n
    then x .|. complement (bit n - 1)
    else x .&. (bit n - 1))

-- Booleans

lt :: Expr EWord -> Expr EWord -> Expr EWord
lt = op2 LT (\x y -> if x < y then 1 else 0)

gt :: Expr EWord -> Expr EWord -> Expr EWord
gt = op2 GT (\x y -> if x > y then 1 else 0)

leq :: Expr EWord -> Expr EWord -> Expr EWord
leq = op2 LEq (\x y -> if x <= y then 1 else 0)

geq :: Expr EWord -> Expr EWord -> Expr EWord
geq = op2 GEq (\x y -> if x >= y then 1 else 0)

slt :: Expr EWord -> Expr EWord -> Expr EWord
slt = op2 SLT (\x y ->
  let sx, sy :: Integer
      sx = fromIntegral x
      sy = fromIntegral y
  in if sx < sy then 1 else 0)

sgt :: Expr EWord -> Expr EWord -> Expr EWord
sgt = op2 SLT (\x y ->
  let sx, sy :: Integer
      sx = fromIntegral x
      sy = fromIntegral y
  in if sx > sy then 1 else 0)

eq :: Expr EWord -> Expr EWord -> Expr EWord
eq = op2 Eq (\x y -> if x == y then 1 else 0)

iszero :: Expr EWord -> Expr EWord
iszero = op1 IsZero (\x -> if x == 0 then 1 else 0)

-- Bits

and :: Expr EWord -> Expr EWord -> Expr EWord
and = op2 And (.&.)

or :: Expr EWord -> Expr EWord -> Expr EWord
or = op2 Or (.|.)

xor :: Expr EWord -> Expr EWord -> Expr EWord
xor = op2 Xor (Data.Bits.xor)

not :: Expr EWord -> Expr EWord
not = op1 Not complement

shl :: Expr EWord -> Expr EWord -> Expr EWord
shl = op2 SHL (\x y -> shiftL x (fromIntegral y))

shr :: Expr EWord -> Expr EWord -> Expr EWord
shr = op2 SHR (\x y -> shiftR x (fromIntegral y))

sar :: Expr EWord -> Expr EWord -> Expr EWord
sar = shr -- TODO: almost certainly wrong


-- ** Bufs ** -------------------------------------------------------------------------------------


-- | Extracts the byte at a given index from a Buf.
--
-- We do our best to return a concrete value wherever possible, but fallback to
-- an abstract expresion if nescessary. Note that a Buf is an infinite
-- structure, so reads outside of the bounds of a ConcreteBuf return 0. This is
-- inline with the semantics of calldata and memory, but not of returndata.
readByte :: Expr EWord -> Expr Buf -> Expr Byte
readByte _ EmptyBuf = LitByte 0x0
readByte i AbstractBuf = ReadByte i AbstractBuf

-- reads from concrete indices
readByte (Lit x) (ConcreteBuf b)
  = if fromIntegral x < BS.length b
    then LitByte (BS.index b (fromIntegral x))
    else LitByte 0x0
readByte i@(Lit x) (WriteByte (Lit idx) val src)
  = if fromIntegral x == idx
    then val
    else readByte i src
readByte i@(Lit x) (WriteWord (Lit idx) val src)
  = if fromIntegral x <= idx && idx < fromIntegral (x + 8)
    then Index (litByte $ idx - fromIntegral x) val -- TODO: pull a concrete value here if we can
    else readByte i src
readByte i@(Lit x) (CopySlice (Lit dstOffset) (Lit srcOffset) (Lit size) src dst)
  = if dstOffset <= fromIntegral x && fromIntegral x < (dstOffset + size)
    then readByte (Lit $ fromIntegral x - (dstOffset - srcOffset)) src
    else readByte i dst

-- we can ignore some partially symbolic CopySlice's if x is not within the region written to dst
readByte i@(Lit x) buf@(CopySlice (Lit dstOffset) _ (Lit size) _ dst)
  = if fromIntegral x < dstOffset || dstOffset + size < fromIntegral x
    then readByte i dst
    else ReadByte (lit x) buf
readByte i@(Lit x) buf@(CopySlice (Lit dstOffset) _ _ _ dst)
  = if fromIntegral x < dstOffset
    then readByte i dst
    else ReadByte (lit x) buf

-- if we have writes to abstract indices, then return a ReadByte expr
readByte i buf = ReadByte i buf


-- | Copies a slice of src into dst
copySlice :: Expr EWord -> Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf -> Expr Buf
copySlice _ _ _ EmptyBuf EmptyBuf = EmptyBuf
copySlice _ _ _ EmptyBuf (ConcreteBuf dst) = ConcreteBuf dst
copySlice _ _ _ (ConcreteBuf src) EmptyBuf = ConcreteBuf src
copySlice (Lit srcOffset) (Lit dstOffset) (Lit size) (ConcreteBuf src) (ConcreteBuf dst) = let
    {-
            0           srcOffset       srcOffset + size     length src
            ┌--------------┬------------------┬-----------------┐
       src: |              | ------ sl ------ |                 |
            └--------------┴------------------┴-----------------┘

            0     dstOffset       dstOffset + size     length dst
            ┌--------┬------------------┬-----------------┐
       dst: |   hd   |                  |       tl        |
            └--------┴------------------┴-----------------┘
    -}
    hd = padRight (fromEnum dstOffset) $ BS.take (fromEnum dstOffset) dst
    sl = padRight (fromEnum size) $ BS.take (fromEnum size) (BS.drop (fromEnum srcOffset) src)
    tl = BS.drop (fromEnum dstOffset + fromEnum size) dst
  in ConcreteBuf $ hd <> sl <> tl
copySlice srcOffset dstOffset size src dst = CopySlice srcOffset dstOffset size src dst


-- ** Storage ** ----------------------------------------------------------------------------------


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


-- ** Helpers ** ----------------------------------------------------------------------------------


to512 :: W256 -> Word512
to512 = fromIntegral
