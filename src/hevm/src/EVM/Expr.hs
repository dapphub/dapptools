{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}

{-|
   Helper functions for working with Expr instances.
   All functions here will return a concrete result if given a concrete input.
-}
module EVM.Expr where

import Prelude hiding (LT, GT)
import Data.Bits
import Data.Word
import Data.Maybe
import Data.List

import Control.Lens (lens)

import EVM.Types
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map


-- ** Stack Ops ** ---------------------------------------------------------------------------------


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
  else fromIntegral $ (to512 x + to512 y) `Prelude.mod` to512 z)

mulmod :: Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
mulmod = op3 MulMod (\x y z ->
   if z == 0
   then 0
   else fromIntegral $ (to512 x * to512 y) `Prelude.mod` to512 z)

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
xor = op2 Xor Data.Bits.xor

not :: Expr EWord -> Expr EWord
not = op1 Not complement

shl :: Expr EWord -> Expr EWord -> Expr EWord
shl = op2 SHL (\x y -> shiftL x (fromIntegral y))

shr :: Expr EWord -> Expr EWord -> Expr EWord
shr = op2 SHR (\x y -> shiftR x (fromIntegral y))

sar :: Expr EWord -> Expr EWord -> Expr EWord
sar = shr -- TODO: almost certainly wrong


-- ** Bufs ** --------------------------------------------------------------------------------------


-- | Extracts the byte at a given index from a Buf.
--
-- We do our best to return a concrete value wherever possible, but fallback to
-- an abstract expresion if nescessary. Note that a Buf is an infinite
-- structure, so reads outside of the bounds of a ConcreteBuf return 0. This is
-- inline with the semantics of calldata and memory, but not of returndata.
readByte :: Expr EWord -> Expr Buf -> Expr Byte
readByte _ EmptyBuf = LitByte 0x0

-- fuly concrete reads
readByte (Lit x) (ConcreteBuf b)
  = if num x < BS.length b
    then LitByte (BS.index b (num x))
    else LitByte 0x0
readByte i@(Lit x) (WriteByte (Lit idx) val src)
  = if num x == idx
    then val
    else readByte i src
readByte i@(Lit x) (WriteWord (Lit idx) val src)
  = if num x <= idx && idx < num (x + 8)
    then case val of
           (Lit _) -> indexWord i val
           _ -> IndexWord (Lit $ idx - num x) val
    else readByte i src
readByte i@(Lit x) (CopySlice (Lit dstOffset) (Lit srcOffset) (Lit size) src dst)
  = if dstOffset <= num x && num x < (dstOffset + size)
    then readByte (Lit $ num x - (dstOffset - srcOffset)) src
    else readByte i dst

-- reads from partially symbolic copySlice exprs
readByte i@(Lit x) buf@(CopySlice (Lit dstOffset) _ (Lit size) _ dst)
  = if num x < dstOffset || dstOffset + size < num x
    then readByte i dst
    else ReadByte (Lit x) buf
readByte i@(Lit x) buf@(CopySlice (Lit dstOffset) _ _ _ dst)
  = if num x < dstOffset
    then readByte i dst
    else ReadByte (Lit x) buf

-- fully abstract reads
readByte i buf = ReadByte i buf


-- | Reads n bytes starting from idx in buf and returns a left padded word
--
-- If n is >= 32 this is the same as readWord
readBytes :: Int -> Expr EWord -> Expr Buf -> Expr EWord
readBytes (Prelude.min 32 -> n) idx buf
  = joinBytes [readByte (add idx (Lit . num $ i)) buf | i <- [0 .. n - 1]]

-- | Reads the word starting at idx from the given buf
readWord :: Expr EWord -> Expr Buf -> Expr EWord
readWord i@(Lit idx) buf = let
    bytes = [readByte (Lit i') buf | i' <- [idx .. idx + 31]]
  in if Prelude.and . (fmap isLitByte) $ bytes
     then Lit (bytesToW256 . mapMaybe unlitByte $ bytes)
     else ReadWord i buf
readWord idx buf = ReadWord idx buf


{- | Copies a slice of src into dst.

        0           srcOffset       srcOffset + size     length src
        ┌--------------┬------------------┬-----------------┐
   src: |              | ------ sl ------ |                 |
        └--------------┴------------------┴-----------------┘

        0     dstOffset       dstOffset + size     length dst
        ┌--------┬------------------┬-----------------┐
   dst: |   hd   |                  |       tl        |
        └--------┴------------------┴-----------------┘
-}
copySlice :: Expr EWord -> Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf -> Expr Buf

-- copies from empty bufs
copySlice _ _ _ EmptyBuf EmptyBuf = EmptyBuf
copySlice _ _ _ EmptyBuf (ConcreteBuf dst) = ConcreteBuf dst

-- fully concrete copies
copySlice (Lit srcOffset) (Lit dstOffset) (Lit size) (ConcreteBuf src) EmptyBuf = let
    hd = BS.replicate (num dstOffset) 0
    sl = padRight (num size) $ BS.take (num size) (BS.drop (num srcOffset) src)
  in ConcreteBuf $ hd <> sl
copySlice (Lit srcOffset) (Lit dstOffset) (Lit size) (ConcreteBuf src) (ConcreteBuf dst) = let
    hd = padRight (num dstOffset) $ BS.take (num dstOffset) dst
    sl = padRight (num size) $ BS.take (num size) (BS.drop (num srcOffset) src)
    tl = BS.drop (num dstOffset + num size) dst
  in ConcreteBuf $ hd <> sl <> tl

-- concrete indicies & abstract src (may produce a concrete result if we are
-- copying from a concrete region of src)
copySlice s@(Lit srcOffset) d@(Lit dstOffset) sz@(Lit size) src ds@(ConcreteBuf dst) = let
    hd = padRight (num dstOffset) $ BS.take (num dstOffset) dst
    sl = [readByte (Lit i) src | i <- [srcOffset .. srcOffset + size]]
  in if Prelude.and . (fmap isLitByte) $ sl
     then ConcreteBuf $ hd <> (BS.pack . (mapMaybe unlitByte) $ sl)
     else CopySlice s d sz src ds

-- abstract indicies
copySlice srcOffset dstOffset size src dst = CopySlice srcOffset dstOffset size src dst


writeByte :: Expr EWord -> Expr Byte -> Expr Buf -> Expr Buf
writeByte (Lit offset) (LitByte byte) (ConcreteBuf src)
  = ConcreteBuf $ BS.take (num offset) src
               <> BS.pack [byte]
               <> BS.drop ((num offset) + 1) src
writeByte offset byte src = WriteByte offset byte src


writeWord :: Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf
writeWord (Lit offset) (Lit val) (ConcreteBuf src)
  = ConcreteBuf $ BS.take (num offset) src
               <> asBE val
               <> BS.drop ((num offset) + 32) src
writeWord offset val src = WriteWord offset val src


-- | Returns the length of a given buffer
--
-- If there are any writes to abstract locations, or CopySlices with an
-- abstract size or dstOffset, an abstract expresion will be returned.
bufLength :: Expr Buf -> Expr EWord
bufLength buf = case go 0 buf of
                  Just len -> len
                  Nothing -> BufLength buf
  where
    go :: W256 -> Expr Buf -> Maybe (Expr EWord)
    go l EmptyBuf = Just . Lit $ l
    go l (ConcreteBuf b) = Just . Lit $ max (num . BS.length $ b) l
    go l (WriteWord (Lit idx) _ b) = go (max l (idx + 31)) b
    go l (WriteByte (Lit idx) _ b) = go (max l idx) b
    go l (CopySlice _ (Lit dstOffset) (Lit size) _ dst) = go (max (dstOffset + size - 1) l) dst
    go _ _ = Nothing

-- | Returns the smallest possible size of a given buffer.
--
-- All data past this index will be symbolic (i.e. unexecutable).
minLength :: Expr Buf -> Maybe Int
minLength = go 0
  where
    go :: W256 -> Expr Buf -> Maybe Int
    -- base cases
    go l EmptyBuf = Just . num $ l
    go _ AbstractBuf = Nothing
    go l (ConcreteBuf b) = Just . num $ max (num . BS.length $ b) l

    -- writes to a concrete index
    go l (WriteWord (Lit idx) _ b) = go (max l (idx + 31)) b
    go l (WriteByte (Lit idx) _ b) = go (max l idx) b
    go l (CopySlice _ (Lit dstOffset) (Lit size) _ dst) = go (max (dstOffset + size - 1) l) dst

    -- writes to an abstract index are ignored
    go l (WriteWord _ _ b) = go l b
    go l (WriteByte _ _ b) = go l b
    go l (CopySlice _ _ _ _ dst) = go l dst


word256At
  :: Functor f
  => Expr EWord -> (Expr EWord -> f (Expr EWord))
  -> Expr Buf -> f (Expr Buf)
word256At i = lens getter setter where
  getter = readWord i
  setter m x = writeWord i x m


-- | Returns the first n bytes of buf
take :: W256 -> Expr Buf -> Expr Buf
take n = slice (Lit 0) (Lit n)


-- | Returns the last n bytes of buf
drop :: W256 -> Expr Buf -> Expr Buf
drop n buf = slice (sub (Lit n) (sub (bufLength buf) (Lit 1))) (Lit n) buf


slice :: Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf
slice offset size src = copySlice offset (Lit 0) size src EmptyBuf


toList :: Expr Buf -> Maybe [Expr Byte]
toList EmptyBuf = Just []
toList AbstractBuf = Nothing
toList (ConcreteBuf bs) = Just . (fmap LitByte) $ BS.unpack bs
toList buf = case bufLength buf of
  Lit l -> Just $ go l
  _ -> Nothing
  where
    go 0 = [readByte (Lit 0) buf]
    go i = readByte (Lit i) buf : go (i - 1)


fromList :: [Expr Byte] -> Expr Buf
fromList bs = case Prelude.and (fmap isLitByte bs) of
  True -> ConcreteBuf . BS.pack . mapMaybe unlitByte $ bs
  -- we want the resulting buffer to be a concrete base with any symbolic
  -- writes stacked on top, so we write all concrete bytes in a first pass and
  -- then write any symbolic bytes afterwards
  False -> applySyms . applyConcrete $ bs
    where
      applyConcrete :: [Expr Byte] -> (Expr Buf, [(W256, Expr Byte)])
      applyConcrete bytes = let
          go :: (Expr Buf, [(W256, Expr Byte)]) -> (W256, Expr Byte) -> (Expr Buf, [(W256, Expr Byte)])
          go (buf, syms) b = case b of
                       (idx, LitByte b') -> (writeByte (Lit idx) (LitByte b') buf, syms)
                       _ -> (buf, b : syms)
        in foldl' go (EmptyBuf, []) (zip [0..] bytes)

      applySyms :: (Expr Buf, [(W256, Expr Byte)]) -> Expr Buf
      applySyms (buf, syms) = foldl' (\acc (idx, b) -> writeByte (Lit idx) b acc) buf syms

instance Semigroup (Expr Buf) where
  a <> b = copySlice (Lit 0) (bufLength a) (bufLength b) a b

instance Monoid (Expr Buf) where
  mempty = EmptyBuf

-- ** Storage ** -----------------------------------------------------------------------------------


-- | Reads the word at the given slot from the given storage expression.
--
-- Note that we return a Nothing instead of a 0x0 if we are reading from a
-- store that is backed by a ConcreteStore or an EmptyStore and there have been
-- no explicit writes to the requested slot. This makes implementing rpc
-- storage lookups much easier. If the store is backed by an AbstractStore we
-- always return a symbolic value.
readStorage :: Expr Storage -> Expr EWord -> Expr EWord -> Maybe (Expr EWord)
readStorage EmptyStore _ _ = Nothing
readStorage store@(ConcreteStore s) addr loc = case (addr, loc) of
  (Lit a, Lit l) -> do
    ctrct <- Map.lookup a s
    val <- Map.lookup l ctrct
    pure $ Lit val
  _ -> Just $ SLoad addr loc store
readStorage s@AbstractStore addr' loc = Just $ SLoad addr' loc s
readStorage s@(SStore addr slot val prev) addr' loc = case (addr, slot, addr', loc) of
  (Lit _, Lit _, Lit _, Lit _) -> if loc == slot && addr == addr' then Just val else readStorage prev addr' loc
  _ -> Just $ SLoad addr' addr' s


-- | Writes a value to a key in a storage expression.
--
-- Concrete writes on top of a concrete or empty store will produce a new
-- ConcreteStore, otherwise we add a new write to the storage expression.
writeStorage :: Expr EWord -> Expr EWord -> Expr EWord -> Expr Storage -> Expr Storage
writeStorage a@(Lit addr) k@(Lit key) v@(Lit val) store = case store of
  EmptyStore -> ConcreteStore (Map.singleton addr (Map.singleton key val))
  ConcreteStore s -> let
      ctrct = Map.findWithDefault Map.empty addr s
    in ConcreteStore (Map.insert addr (Map.insert key val ctrct) s)
  _ -> SStore a k v store
writeStorage addr key val store = SStore addr key val store


-- ** Conversions ** -------------------------------------------------------------------------------


litAddr :: Addr -> Expr EWord
litAddr = Lit . num

litCode :: BS.ByteString -> [Expr Byte]
litCode bs = fmap LitByte (BS.unpack bs)

to512 :: W256 -> Word512
to512 = fromIntegral


-- ** Helpers ** -----------------------------------------------------------------------------------


-- Is the given expr a literal word?
isLitByte :: Expr Byte -> Bool
isLitByte (LitByte _) = True
isLitByte _ = False

-- | Returns the byte at idx from the given word.
indexWord :: Expr EWord -> Expr EWord -> Expr Byte
indexWord (Lit idx) (Lit w) = LitByte . fromIntegral $ shiftR w (num idx * 8)
indexWord (Lit idx) (JoinBytes zero        one        two       three
                               four        five       six       seven
                               eight       nine       ten       eleven
                               twelve      thirteen   fourteen  fifteen
                               sixteen     seventeen  eighteen  nineteen
                               twenty      twentyone  twentytwo twentythree
                               twentyfour  twentyfive twentysix twentyseven
                               twentyeight twentynine thirty    thirtyone)
  | idx == 0 = zero
  | idx == 1 = one
  | idx == 2 = two
  | idx == 3 = three
  | idx == 4 = four
  | idx == 5 = five
  | idx == 6 = six
  | idx == 7 = seven
  | idx == 8 = eight
  | idx == 9 = nine
  | idx == 10 = ten
  | idx == 11 = eleven
  | idx == 12 = twelve
  | idx == 13 = thirteen
  | idx == 14 = fourteen
  | idx == 15 = fifteen
  | idx == 16 = sixteen
  | idx == 17 = seventeen
  | idx == 18 = eighteen
  | idx == 19 = nineteen
  | idx == 20 = twenty
  | idx == 21 = twentyone
  | idx == 22 = twentytwo
  | idx == 23 = twentythree
  | idx == 24 = twentyfour
  | idx == 25 = twentyfive
  | idx == 26 = twentysix
  | idx == 27 = twentyseven
  | idx == 28 = twentyeight
  | idx == 29 = twentynine
  | idx == 30 = thirty
  | idx == 31 = thirtyone
  | otherwise = LitByte 0
indexWord idx w = IndexWord idx w


padByte :: Expr Byte -> Expr EWord
padByte (LitByte b) = Lit . bytesToW256 $ [b]
padByte b = joinBytes [b]

-- | Converts a list of bytes into a W256.
-- TODO: semantics if the input is too large?
bytesToW256 :: [Word8] -> W256
bytesToW256 = word . BS.pack

padBytesLeft :: Int -> [Expr Byte] -> [Expr Byte]
padBytesLeft n bs
  | length bs > n = Prelude.take n bs
  | length bs == n = bs
  | otherwise = padBytesLeft n (LitByte 0 : bs)

joinBytes :: [Expr Byte] -> Expr EWord
joinBytes bs
  | Prelude.and . (fmap isLitByte) $ bs = Lit . bytesToW256 . (mapMaybe unlitByte) $ bs
  | otherwise = let
      bytes = padBytesLeft 32 bs
    in JoinBytes
      (bytes !! 0)  (bytes !! 1)  (bytes !! 2)  (bytes !! 3)
      (bytes !! 4)  (bytes !! 5)  (bytes !! 6)  (bytes !! 7)
      (bytes !! 8)  (bytes !! 9)  (bytes !! 10) (bytes !! 11)
      (bytes !! 12) (bytes !! 13) (bytes !! 14) (bytes !! 15)
      (bytes !! 16) (bytes !! 17) (bytes !! 18) (bytes !! 19)
      (bytes !! 20) (bytes !! 21) (bytes !! 22) (bytes !! 23)
      (bytes !! 24) (bytes !! 25) (bytes !! 26) (bytes !! 27)
      (bytes !! 28) (bytes !! 29) (bytes !! 30) (bytes !! 31)

eqByte :: Expr Byte -> Expr Byte -> Expr EWord
eqByte (LitByte x) (LitByte y) = Lit $ if x == y then 1 else 0
eqByte x y = EqByte x y

min :: Expr EWord -> Expr EWord -> Expr EWord
min (Lit x) (Lit y) = if x < y then Lit x else Lit y
min x y = Min x y
