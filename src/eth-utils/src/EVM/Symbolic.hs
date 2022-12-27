{-# Language NamedFieldPuns #-}
{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language ScopedTypeVariables #-}

module EVM.Symbolic where

import Prelude hiding  (Word, LT, GT)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Lens hiding (op, (:<), (|>), (.>))
import Data.Maybe                   (fromMaybe, fromJust)

import EVM.Types
import qualified EVM.Concrete as Concrete
import qualified Data.ByteArray       as BA
import Data.SBV hiding (runSMT, newArray_, addAxiom, Word)
import Data.SBV.Tools.Overflow
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as Crypto

litWord :: Word -> SymWord
litWord (C whiff a) = S whiff (literal $ toSizzle a)

litAddr :: Addr -> SAddr
litAddr = SAddr . literal . toSizzle

maybeLitAddr :: SAddr -> Maybe Addr
maybeLitAddr (SAddr a) = fmap fromSizzle (unliteral a)

maybeLitBytes :: [SWord 8] -> Maybe ByteString
maybeLitBytes xs = fmap (\x -> BS.pack (fmap fromSized x)) (mapM unliteral xs)

-- | Note: the (force*) functions are crude and in general,
-- the continuation passing style `forceConcrete`
-- alternatives should be prefered for better error
-- handling when used during EVM execution
forceLit :: SymWord -> Word
forceLit (S whiff a) = case unliteral a of
  Just c -> C whiff (fromSizzle c)
  Nothing -> error "unexpected symbolic argument"

forceLitBytes :: [SWord 8] -> ByteString
forceLitBytes = BS.pack . fmap (fromSized . fromJust . unliteral)

forceBuffer :: Buffer -> ByteString
forceBuffer (ConcreteBuffer b) = b
forceBuffer (SymbolicBuffer b) = forceLitBytes b

sdiv :: SymWord -> SymWord -> SymWord
sdiv (S a x) (S b y) = let sx, sy :: SInt 256
                           sx = sFromIntegral x
                           sy = sFromIntegral y
                       in S (Div a b) (sFromIntegral (sx `sQuot` sy))

smod :: SymWord -> SymWord -> SymWord
smod (S a x) (S b y) = let sx, sy :: SInt 256
                           sx = sFromIntegral x
                           sy = sFromIntegral y
                       in S (Mod a b) $ ite (y .== 0) 0 (sFromIntegral (sx `sRem` sy))

addmod :: SymWord -> SymWord -> SymWord -> SymWord
addmod (S a x) (S b y) (S c z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in  S (Todo "addmod" [a, b, c]) $ ite (z .== 0) 0 $ sFromIntegral $ ((to512 x) + (to512 y)) `sMod` (to512 z)

mulmod :: SymWord -> SymWord -> SymWord -> SymWord
mulmod (S a x) (S b y) (S c z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in S (Todo "mulmod" [a, b, c]) $ ite (z .== 0) 0 $ sFromIntegral $ ((to512 x) * (to512 y)) `sMod` (to512 z)

-- | Signed less than
slt :: SymWord -> SymWord -> SymWord
slt (S xw x) (S yw y) =
  iteWhiff (SLT xw yw) (sFromIntegral x .< (sFromIntegral y :: (SInt 256))) 1 0

-- | Signed greater than
sgt :: SymWord -> SymWord -> SymWord
sgt (S xw x) (S yw y) =
  iteWhiff (SGT xw yw) (sFromIntegral x .> (sFromIntegral y :: (SInt 256))) 1 0

-- * Operations over symbolic memory (list of symbolic bytes)
swordAt :: Int -> [SWord 8] -> SymWord
swordAt i bs = let bs' = truncpad 32 $ drop i bs
               in S (FromBytes (SymbolicBuffer bs')) (fromBytes bs')

readByteOrZero' :: Int -> [SWord 8] -> SWord 8
readByteOrZero' i bs = fromMaybe 0 (bs ^? ix i)

sliceWithZero' :: Int -> Int -> [SWord 8] -> [SWord 8]
sliceWithZero' o s m = truncpad s $ drop o m

writeMemory' :: [SWord 8] -> Word -> Word -> Word -> [SWord 8] -> [SWord 8]
writeMemory' bs1 (C _ n) (C _ src) (C _ dst) bs0 =
  let
    (a, b) = splitAt (num dst) bs0
    a'     = replicate (num dst - length a) 0
    c      = if src > num (length bs1)
             then replicate (num n) 0
             else sliceWithZero' (num src) (num n) bs1
    b'     = drop (num (n)) b
  in
    a <> a' <> c <> b'

readMemoryWord' :: Word -> [SWord 8] -> SymWord
readMemoryWord' (C _ i) m =
  let bs = truncpad 32 (drop (num i) m)
  in S (FromBytes (SymbolicBuffer bs)) (fromBytes bs)

readMemoryWord32' :: Word -> [SWord 8] -> SWord 32
readMemoryWord32' (C _ i) m = fromBytes $ truncpad 4 (drop (num i) m)

setMemoryWord' :: Word -> SymWord -> [SWord 8] -> [SWord 8]
setMemoryWord' (C _ i) (S _ x) =
  writeMemory' (toBytes x) 32 0 (num i)

setMemoryByte' :: Word -> SWord 8 -> [SWord 8] -> [SWord 8]
setMemoryByte' (C _ i) x =
  writeMemory' [x] 1 0 (num i)

readSWord' :: Word -> [SWord 8] -> SymWord
readSWord' (C _ i) x =
  if i > num (length x)
  then 0
  else swordAt (num i) x


select' :: (Ord b, Num b, SymVal b, Mergeable a) => [a] -> a -> SBV b -> a
select' xs err ind = walk xs ind err
    where walk []     _ acc = acc
          walk (e:es) i acc = walk es (i-1) (ite (i .== 0) e acc)

-- | Read 32 bytes from index from a bounded list of bytes.
readSWordWithBound :: SymWord -> Buffer -> SymWord -> SymWord
readSWordWithBound sind@(S _ ind) (SymbolicBuffer xs) (S _ bound) = case (num <$> maybeLitWord sind, num <$> fromSizzle <$> unliteral bound) of
  (Just i, Just b) ->
    let bs = truncpad 32 $ drop i (take b xs)
    in S (FromBytes (SymbolicBuffer bs)) (fromBytes bs)
  _ ->
    -- Generates a ridiculously large set of constraints (roughly 25k) when
    -- the index is symbolic, but it still seems (kind of) manageable
    -- for the solvers.

    -- The proper solution here is to use smt arrays instead.

    let boundedList = [ite (i .<= bound) x' 0 | (x', i) <- zip xs [1..]]
        res = [select' boundedList 0 (ind + j) | j <- [0..31]]
    in S (FromBytes $ SymbolicBuffer res) $ fromBytes res

readSWordWithBound sind (ConcreteBuffer xs) bound =
  case maybeLitWord sind of
    Nothing -> readSWordWithBound sind (SymbolicBuffer (litBytes xs)) bound
    Just x' ->
       -- INVARIANT: bound should always be length xs for concrete bytes
       -- so we should be able to safely ignore it here
         litWord $ Concrete.readMemoryWord x' xs

-- a whole foldable instance seems overkill, but length is always good to have!
len :: Buffer -> Int
len (SymbolicBuffer bs) = length bs
len (ConcreteBuffer bs) = BS.length bs

readByteOrZero :: Int -> Buffer -> SWord 8
readByteOrZero i (SymbolicBuffer bs) = readByteOrZero' i bs
readByteOrZero i (ConcreteBuffer bs) = num $ Concrete.readByteOrZero i bs

sliceWithZero :: Int -> Int -> Buffer -> Buffer
sliceWithZero o s (SymbolicBuffer m) = SymbolicBuffer (sliceWithZero' o s m)
sliceWithZero o s (ConcreteBuffer m) = ConcreteBuffer (Concrete.byteStringSliceWithDefaultZeroes o s m)

writeMemory :: Buffer -> Word -> Word -> Word -> Buffer -> Buffer
writeMemory (ConcreteBuffer bs1) n src dst (ConcreteBuffer bs0) =
  ConcreteBuffer (Concrete.writeMemory bs1 n src dst bs0)
writeMemory (ConcreteBuffer bs1) n src dst (SymbolicBuffer bs0) =
  SymbolicBuffer (writeMemory' (litBytes bs1) n src dst bs0)
writeMemory (SymbolicBuffer bs1) n src dst (ConcreteBuffer bs0) =
  SymbolicBuffer (writeMemory' bs1 n src dst (litBytes bs0))
writeMemory (SymbolicBuffer bs1) n src dst (SymbolicBuffer bs0) =
  SymbolicBuffer (writeMemory' bs1 n src dst bs0)

readMemoryWord :: Word -> Buffer -> SymWord
readMemoryWord i (SymbolicBuffer m) = readMemoryWord' i m
readMemoryWord i (ConcreteBuffer m) = litWord $ Concrete.readMemoryWord i m

readMemoryWord32 :: Word -> Buffer -> SWord 32
readMemoryWord32 i (SymbolicBuffer m) = readMemoryWord32' i m
readMemoryWord32 i (ConcreteBuffer m) = num $ Concrete.readMemoryWord32 i m

setMemoryWord :: Word -> SymWord -> Buffer -> Buffer
setMemoryWord i x (SymbolicBuffer z) = SymbolicBuffer $ setMemoryWord' i x z
setMemoryWord i x (ConcreteBuffer z) = case maybeLitWord x of
  Just x' -> ConcreteBuffer $ Concrete.setMemoryWord i x' z
  Nothing -> SymbolicBuffer $ setMemoryWord' i x (litBytes z)

setMemoryByte :: Word -> SWord 8 -> Buffer -> Buffer
setMemoryByte i x (SymbolicBuffer m) = SymbolicBuffer $ setMemoryByte' i x m
setMemoryByte i x (ConcreteBuffer m) = case fromSized <$> unliteral x of
  Nothing -> SymbolicBuffer $ setMemoryByte' i x (litBytes m)
  Just x' -> ConcreteBuffer $ Concrete.setMemoryByte i x' m

readSWord :: Word -> Buffer -> SymWord
readSWord i (SymbolicBuffer x) = readSWord' i x
readSWord i (ConcreteBuffer x) = num $ Concrete.readMemoryWord i x

index :: Int -> Buffer -> SWord8
index x (ConcreteBuffer b) = literal $ BS.index b x
index x (SymbolicBuffer b) = fromSized $ b !! x

-- * Uninterpreted functions

symSHA256N :: SInteger -> SInteger -> SWord 256
symSHA256N = uninterpret "sha256"

symkeccakN :: SInteger -> SInteger -> SWord 256
symkeccakN = uninterpret "keccak"

toSInt :: [SWord 8] -> SInteger
toSInt bs = sum $ zipWith (\a (i :: Integer) -> sFromIntegral a * 256 ^ i) bs [0..]


-- | Although we'd like to define this directly as an uninterpreted function,
-- we cannot because [a] is not a symbolic type. We must convert the list into a suitable
-- symbolic type first. The only important property of this conversion is that it is injective.
-- We embedd the bytestring as a pair of symbolic integers, this is a fairly easy solution.
symkeccak' :: [SWord 8] -> SWord 256
symkeccak' bytes = case length bytes of
  0 -> literal $ toSizzle $ keccak ""
  n -> symkeccakN (num n) (toSInt bytes)

symSHA256 :: [SWord 8] -> [SWord 8]
symSHA256 bytes = case length bytes of
  0 -> litBytes $ BS.pack $ BA.unpack $ (Crypto.hash BS.empty :: Digest SHA256)
  n -> toBytes $ symSHA256N (num n) (toSInt bytes)

rawVal :: SymWord -> SWord 256
rawVal (S _ v) = v

-- | Reconstruct the smt/sbv value from a whiff
-- Should satisfy (rawVal x .== whiffValue x)
whiffValue :: Whiff -> SWord 256
whiffValue w = case w of
  w'@(Todo _ _) -> error $ "unable to get value of " ++ show w'
  And x y       -> whiffValue x .&. whiffValue y
  Or x y        -> whiffValue x .|. whiffValue y
  Eq x y        -> ite (whiffValue x .== whiffValue y) 1 0
  LT x y        -> ite (whiffValue x .< whiffValue y) 1 0
  GT x y        -> ite (whiffValue x .> whiffValue y) 1 0
  ITE b x y     -> ite (whiffValue b .== 1) (whiffValue x) (whiffValue y)
  SLT x y       -> rawVal $ slt (S x (whiffValue x)) (S y (whiffValue y))
  SGT x y       -> rawVal $ sgt (S x (whiffValue x)) (S y (whiffValue y))
  IsZero x      -> ite (whiffValue x .== 0) 1 0
  SHL x y       -> sShiftLeft  (whiffValue x) (whiffValue y)
  SHR x y       -> sShiftRight (whiffValue x) (whiffValue y)
  SAR x y       -> sSignedShiftArithRight (whiffValue x) (whiffValue y)
  Add x y       -> whiffValue x + whiffValue y
  Sub x y       -> whiffValue x - whiffValue y
  Mul x y       -> whiffValue x * whiffValue y
  Div x y       -> whiffValue x `sDiv` whiffValue y
  Mod x y       -> whiffValue x `sMod` whiffValue y
  Exp x y       -> whiffValue x .^ whiffValue y
  Neg x         -> complement $ whiffValue x
  Var _ v       -> v
  FromKeccak (ConcreteBuffer bstr) -> literal $ num $ keccak bstr
  FromKeccak (SymbolicBuffer buf)  -> symkeccak' buf
  Literal x -> literal $ num $ x
  FromBytes buf -> rawVal $ readMemoryWord 0 buf
  FromStorage ind arr -> readArray arr (whiffValue ind)

-- | Special cases that have proven useful in practice
simplifyCondition :: SBool -> Whiff -> SBool
simplifyCondition _ (IsZero (IsZero (IsZero a))) = whiffValue a .== 0



-- | Overflow safe math can be difficult for smt solvers to deal with,
-- especially for 256-bit words. When we recognize terms arising from
-- overflow checks, we translate our queries into a more bespoke form,
-- outlined in:
-- Modular Bug-finding for Integer Overflows in the Large:
-- Sound, Efficient, Bit-precise Static Analysis
-- www.microsoft.com/en-us/research/wp-content/uploads/2016/02/z3prefix.pdf
--
-- Addition overflow.
-- Written as
--    require (x <= (x + y))
-- or require (y <= (x + y))
-- or require (!(y < (x + y)))
simplifyCondition b (IsZero (IsZero (LT (Add x y) z))) =
  let x' = whiffValue x
      y' = whiffValue y
      z' = whiffValue z
      (_, overflow) = bvAddO x' y'
  in
    ite (x' .== z' .||
         y' .== z')
    overflow
    b

-- Multiplication overflow.
-- Written as
--    require (y == 0 || x * y / y == x)
-- or require (y == 0 || x == x * y / y)

-- proveWith cvc4 $ \x y z -> ite (y .== (z :: SWord 8)) (((x * y) `sDiv` z ./= x) .<=> (snd (bvMulO x y) .|| (z .== 0 .&& x .> 0))) (sTrue)
-- Q.E.D.
simplifyCondition b (IsZero (Eq x (Div (Mul y z) w))) =
  simplifyCondition b (IsZero (Eq (Div (Mul y z) w) x))
simplifyCondition b (IsZero (Eq (Div (Mul y z) w) x)) =
  let x' = whiffValue x
      y' = whiffValue y
      z' = whiffValue z
      w' = whiffValue w
      (_, overflow) = bvMulO y' z'
  in
    ite
    ((y' .== x' .&& z' .== w') .||
      (z' .== x' .&& y' .== w'))
    (overflow .|| (w' .== 0 .&& x' ./= 0))
    b
simplifyCondition b _ = b
