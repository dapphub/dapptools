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
import EVM.Expr
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
forceBuffer (ConcreteBuffer _ b) = b
forceBuffer (SymbolicBuffer _ b) = forceLitBytes b


sdiv :: SymWord -> SymWord -> SymWord
sdiv (S w1 x) (S w2 y) = let sx, sy :: SInt 256
                             sx = sFromIntegral x
                             sy = sFromIntegral y
                       in S (Div w1 w2) $ sFromIntegral (sx `sQuot` sy)

smod :: SymWord -> SymWord -> SymWord
smod (S w1 x) (S w2 y) = let sx, sy :: SInt 256
                             sx = sFromIntegral x
                             sy = sFromIntegral y
                       in S (Mod w1 w2) $ ite (y .== 0) 0 (sFromIntegral (sx `sRem` sy))

addmod :: SymWord -> SymWord -> SymWord -> SymWord
addmod (S a x) (S b y) (S c z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in S (Todo "addmod" [a, b, c]) $ sFromIntegral $ ((to512 x) + (to512 y)) `sMod` (to512 z)

mulmod :: SymWord -> SymWord -> SymWord -> SymWord
mulmod (S a x) (S b y) (S c z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in S (Todo "mulmod" [a, b, c]) $ sFromIntegral $ ((to512 x) * (to512 y)) `sMod` (to512 z)

-- | Signed less than
slt :: SymWord -> SymWord -> SymWord
slt (S xw x) (S yw y) =
  iteExpr (SLT xw yw) (sFromIntegral x .< (sFromIntegral y :: (SInt 256))) 1 0

-- | Signed greater than
sgt :: SymWord -> SymWord -> SymWord
sgt (S xw x) (S yw y) =
  iteExpr (SGT xw yw) (sFromIntegral x .> (sFromIntegral y :: (SInt 256))) 1 0

-- * Operations over symbolic memory (list of symbolic bytes)
swordAt :: Int -> [SWord 8] -> SymWord
swordAt i bs = let bs' = truncpad 32 $ drop i bs
               in S (Todo "fromBytes" []) (fromBytes bs')

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
readMemoryWord' (C a i) m =
  let bs = truncpad 32 (drop (num i) m)
  in S (Todo "readMemoryWord'" []) (fromBytes bs)

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
readSWordWithBound sind@(S indexpr ind) (SymbolicBuffer buffexpr xs) (S bwhiff bound) = case (num <$> maybeLitWord sind, num <$> fromSizzle <$> unliteral bound) of
  (Just i, Just b) ->
    let bs = truncpad 32 $ drop i (take b xs)
    in S (ReadWord indexpr buffexpr) (fromBytes bs)

  _ ->
    -- Generates a ridiculously large set of constraints (roughly 25k) when
    -- the index is symbolic, but it still seems (kind of) manageable
    -- for the solvers.

    -- The proper solution here is to use smt arrays instead.

    let boundedList = [ite (i .<= bound) x' 0 | (x', i) <- zip xs [1..]]
        res = [select' boundedList 0 (ind + j) | j <- [0..31]]
    in S (ReadWord indexpr buffexpr) $ fromBytes res

readSWordWithBound sind (ConcreteBuffer w xs) bound =
  case maybeLitWord sind of
    Nothing -> readSWordWithBound sind (SymbolicBuffer w (litBytes xs)) bound
    Just x' ->
       -- INVARIANT: bound should always be length xs for concrete bytes
       -- so we should be able to safely ignore it here
         litWord $ Concrete.readMemoryWord x' xs

-- a whole foldable instance seems overkill, but length is always good to have!
len :: Buffer -> Int
len (SymbolicBuffer _ bs) = length bs
len (ConcreteBuffer _ bs) = BS.length bs

grab :: Int -> Buffer -> Buffer
grab n (SymbolicBuffer _ bs) = SymbolicBuffer (Todo "grab1" []) $ take n bs
grab n (ConcreteBuffer _ bs) = ConcreteBuffer (Todo "grab2" []) $ BS.take n bs

ditch :: Int -> Buffer -> Buffer
ditch n (SymbolicBuffer _ bs) = SymbolicBuffer (Todo "ditch1" []) $ drop n bs
ditch n (ConcreteBuffer _ bs) = ConcreteBuffer (Todo "ditch2" []) $ BS.drop n bs

readByteOrZero :: Int -> Buffer -> SWord 8
readByteOrZero i (SymbolicBuffer _ bs) = readByteOrZero' i bs
readByteOrZero i (ConcreteBuffer _ bs) = num $ Concrete.readByteOrZero i bs

-- TODO  why are those not symwords?
sliceWithZero :: Int -> Int -> Buffer -> Buffer
sliceWithZero o s (SymbolicBuffer w m) = SymbolicBuffer (Slice (Literal (num o)) (Literal (num s)) w) (sliceWithZero' o s m)
sliceWithZero o s (ConcreteBuffer w m) = ConcreteBuffer (Slice (Literal (num o)) (Literal (num s)) w) (Concrete.byteStringSliceWithDefaultZeroes o s m)

writeMemory :: Buffer -> Word -> Word -> Word -> Buffer -> Buffer
writeMemory (ConcreteBuffer w bs1) n src dst (ConcreteBuffer w2 bs0) =
  ConcreteBuffer (Write w (whiff n) (whiff src) (whiff dst) w2) (Concrete.writeMemory bs1 n src dst bs0)
writeMemory (ConcreteBuffer w bs1) n src dst (SymbolicBuffer w2 bs0) =
  SymbolicBuffer (Write w (whiff n) (whiff src) (whiff dst) w2) (writeMemory' (litBytes bs1) n src dst bs0)
writeMemory (SymbolicBuffer w bs1) n src dst (ConcreteBuffer w2 bs0) =
  SymbolicBuffer (Write w (whiff n) (whiff src) (whiff dst) w2) (writeMemory' bs1 n src dst (litBytes bs0))
writeMemory (SymbolicBuffer w bs1) n src dst (SymbolicBuffer w2 bs0) =
  SymbolicBuffer (Write w (whiff n) (whiff src) (whiff dst) w2) (writeMemory' bs1 n src dst bs0)


readMemoryWord :: Word -> Buffer -> SymWord
readMemoryWord (C wfrom i) buff@(SymbolicBuffer wbuff m) = S (ReadWord wfrom wbuff) $ fromBytes $ truncpad 32 (drop (num i) m)
-- TODO - propagate whiff
readMemoryWord i (ConcreteBuffer _ m) = litWord $ Concrete.readMemoryWord i m

readMemoryWord32 :: Word -> Buffer -> SWord 32
readMemoryWord32 (C wfrom i) (SymbolicBuffer wbuff m) = fromBytes $ truncpad 4 (drop (num i) m)
readMemoryWord32 i (ConcreteBuffer wbuff m) = num $ Concrete.readMemoryWord32 i m

setMemoryWord :: Word -> SymWord -> Buffer -> Buffer
setMemoryWord i@(C _ (W256 l)) (S wword x) (SymbolicBuffer wbuff z) = SymbolicBuffer (WriteWord (Literal l) wword wbuff) $ setMemoryWord' i (S wword x) z
setMemoryWord i@(C _ (W256 l)) (S wword x) (ConcreteBuffer wbuff z) = case maybeLitWord (S wword x) of
  Just x' -> ConcreteBuffer (WriteWord (Literal l) wword wbuff) $ Concrete.setMemoryWord i x' z
  Nothing -> SymbolicBuffer (WriteWord (Literal l) wword wbuff) $ setMemoryWord' i (S wword x) (litBytes z)

setMemoryByte :: Word -> SWord 8 -> Buffer -> Buffer
setMemoryByte i x (SymbolicBuffer wbuff m) = SymbolicBuffer (Todo "setMemoryByte" []) $ setMemoryByte' i x m
setMemoryByte i x (ConcreteBuffer wbuff m) = case fromSized <$> unliteral x of
  Nothing -> SymbolicBuffer (Todo "setMemoryByte2" []) $ setMemoryByte' i x (litBytes m)
  Just x' -> ConcreteBuffer (Todo "setMemoryByte3" []) $ Concrete.setMemoryByte i x' m

readSWord :: Word -> Buffer -> SymWord
readSWord i (SymbolicBuffer _ x) = readSWord' i x
readSWord i (ConcreteBuffer _ x) = num $ Concrete.readMemoryWord i x

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

