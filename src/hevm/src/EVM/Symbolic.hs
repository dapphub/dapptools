{-# Language NamedFieldPuns #-}
{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}

module EVM.Symbolic where

import Prelude hiding  (Word)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Control.Lens hiding (op, (:<), (|>), (.>))
import Data.Maybe                   (fromMaybe, fromJust)

import EVM.Types
import EVM.Concrete
import Data.SBV hiding (runSMT, newArray_, addAxiom, Word)


-- | Symbolic words of 256 bits, possibly annotated with additional
--   "insightful" information
data SymWord = S Whiff (SWord 256)

-- | Convenience functions transporting between the concrete and symbolic realm
sw256 :: SWord 256 -> SymWord
sw256 = S Dull

litWord :: Word -> (SymWord)
litWord (C whiff a) = S whiff (literal $ toSizzle a)

w256lit :: W256 -> SymWord
w256lit = S Dull . literal . toSizzle

litAddr :: Addr -> SAddr
litAddr = SAddr . literal . toSizzle

litBytes :: ByteString -> [SWord 8]
litBytes bs = fmap (toSized . literal) (BS.unpack bs)

maybeLitWord :: SymWord -> Maybe Word
maybeLitWord (S whiff a) = fmap (C whiff . fromSizzle) (unliteral a)

maybeLitAddr :: SAddr -> Maybe Addr
maybeLitAddr (SAddr a) = fmap fromSizzle (unliteral a)

maybeLitBytes :: [SWord 8] -> Maybe ByteString
maybeLitBytes xs = fmap (\x -> BS.pack (fmap fromSized x)) (mapM unliteral xs)

-- | Note: these forms are crude and in general,
-- the continuation passing style `forceConcrete`
-- alternatives should be prefered for better error
-- handling when used during EVM execution

forceLit :: SymWord -> Word
forceLit (S whiff a) = case unliteral a of
  Just c -> C whiff (fromSizzle c)
  Nothing -> error "unexpected symbolic argument"

forceLitBytes :: [SWord 8] -> ByteString
forceLitBytes = BS.pack . fmap (fromSized . fromJust . unliteral)


-- | Arithmetic operations on SymWord

sdiv :: SymWord -> SymWord -> SymWord
sdiv (S _ x) (S _ y) = let sx, sy :: SInt 256
                           sx = sFromIntegral x
                           sy = sFromIntegral y
                       in sw256 $ sFromIntegral (sx `sQuot` sy)

smod :: SymWord -> SymWord -> SymWord
smod (S _ x) (S _ y) = let sx, sy :: SInt 256
                           sx = sFromIntegral x
                           sy = sFromIntegral y
                       in sw256 $ ite (y .== 0) 0 (sFromIntegral (sx `sRem` sy))

addmod :: SymWord -> SymWord -> SymWord -> SymWord
addmod (S _ x) (S _ y) (S _ z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in sw256 $ sFromIntegral $ ((to512 x) + (to512 y)) `sMod` (to512 z)

mulmod :: SymWord -> SymWord -> SymWord -> SymWord
mulmod (S _ x) (S _ y) (S _ z) = let to512 :: SWord 256 -> SWord 512
                                     to512 = sFromIntegral
                                 in sw256 $ sFromIntegral $ ((to512 x) * (to512 y)) `sMod` (to512 z)

slt :: SymWord -> SymWord -> SymWord
slt (S _ x) (S _ y) =
  sw256 $ ite (sFromIntegral x .< (sFromIntegral y :: (SInt 256))) 1 0

sgt :: SymWord -> SymWord -> SymWord
sgt (S _ x) (S _ y) =
  sw256 $ ite (sFromIntegral x .> (sFromIntegral y :: (SInt 256))) 1 0


-- | Operations over symbolic memory (list of symbolic bytes)

swordAt :: Int -> [SWord 8] -> SymWord
swordAt i bs = sw256 . fromBytes $ truncpad 32 $ drop i bs

readByteOrZero :: Int -> [SWord 8] -> SWord 8
readByteOrZero i bs = fromMaybe 0 (bs ^? ix i)

sliceWithZero :: Int -> Int -> [SWord 8] -> [SWord 8]
sliceWithZero o s m = truncpad s $ drop o m

writeMemory :: [SWord 8] -> Word -> Word -> Word -> [SWord 8] -> [SWord 8]
writeMemory bs1 (C _ n) (C _ src) (C _ dst) bs0 =
  let
    (a, b) = splitAt (num dst) bs0
    a'     = replicate (num dst - length a) 0
    c      = if src > num (length bs1)
             then replicate (num n) 0
             else sliceWithZero (num src) (num n) bs1
    b'     = drop (num (n)) b
  in
    a <> a' <> c <> b'

readMemoryWord :: Word -> [SWord 8] -> SymWord
readMemoryWord (C _ i) m = sw256 $ fromBytes $ truncpad 32 (drop (num i) m)

readMemoryWord32 :: Word -> [SWord 8] -> SymWord
readMemoryWord32 (C _ i) m = sw256 $ fromBytes $ truncpad 4 (drop (num i) m)

setMemoryWord :: Word -> SymWord -> [SWord 8] -> [SWord 8]
setMemoryWord (C _ i) (S _ x) =
  writeMemory (toBytes x) 32 0 (num i)

setMemoryByte :: Word -> SWord 8 -> [SWord 8] -> [SWord 8]
setMemoryByte (C _ i) x =
  writeMemory [x] 1 0 (num i)

readSWord :: Word -> [SWord 8] -> SymWord
readSWord (C _ i) x =
  if i > num (length x)
  then 0
  else swordAt (num i) x


select' :: (Ord b, Num b, SymVal b, Mergeable a) => [a] -> a -> SBV b -> a
select' xs err ind = walk xs ind err
    where walk []     _ acc = acc
          walk (e:es) i acc = walk es (i-1) (ite (i .== 0) e acc)

-- Generates a ridiculously large set of constraints (roughly 25k) when
-- the index is symbolic, but it still seems (kind of) manageable
-- for the solvers.
readSWordWithBound :: SWord 32 -> [SWord 8] -> SWord 32 -> SymWord
readSWordWithBound ind xs bound =
  let boundedList = [ite (i .<= bound) x 0 | (x, i) <- zip xs [1..]]
  in sw256 . fromBytes $ [select' boundedList 0 (ind + j) | j <- [0..31]]


-- | Custom instances for SymWord, many of which have direct
-- analogues for concrete words defined in Concrete.hs

instance Show SymWord where
  show s@(S Dull _) = case maybeLitWord s of
    Nothing -> "<symbolic>"
    Just w  -> show w
  show (S (Var var) x) = var ++ ": " ++ show x
  show (S (InfixBinOp symbol x y) z) = show x ++ symbol ++ show y  ++ ": " ++ show z
  show (S (BinOp symbol x y) z) = symbol ++ show x ++ show y  ++ ": " ++ show z
  show (S (UnOp symbol x) z) = symbol ++ show x ++ ": " ++ show z
  show (S whiff x) = show whiff ++ ": " ++ show x

instance EqSymbolic SymWord where
  (.==) (S _ x) (S _ y) = x .== y

instance Num SymWord where
  (S _ x) + (S _ y) = sw256 (x + y)
  (S _ x) * (S _ y) = sw256 (x * y)
  abs (S _ x) = sw256 (abs x)
  signum (S _ x) = sw256 (signum x)
  fromInteger x = sw256 (fromInteger x)
  negate (S _ x) = sw256 (negate x)

instance Bits SymWord where
  (S _ x) .&. (S _ y) = sw256 (x .&. y)
  (S _ x) .|. (S _ y) = sw256 (x .|. y)
  (S _ x) `xor` (S _ y) = sw256 (x `xor` y)
  complement (S _ x) = sw256 (complement x)
  shift (S _ x) i = sw256 (shift x i)
  rotate (S _ x) i = sw256 (rotate x i)
  bitSize (S _ x) = bitSize x
  bitSizeMaybe (S _ x) = bitSizeMaybe x
  isSigned (S _ x) = isSigned x
  testBit (S _ x) i = testBit x i
  bit i = sw256 (bit i)
  popCount (S _ x) = popCount x

instance SDivisible SymWord where
  sQuotRem (S _ x) (S _ y) = let (a, b) = x `sQuotRem` y
                             in (sw256 a, sw256 b)
  sDivMod (S _ x) (S _ y) = let (a, b) = x `sDivMod` y
                             in (sw256 a, sw256 b)

instance Mergeable SymWord where
  symbolicMerge a b (S _ x) (S _ y) = sw256 $ symbolicMerge a b x y
  select xs (S _ x) b = let ys = fmap (\(S _ y) -> y) xs
                        in sw256 $ select ys x b

instance Bounded SymWord where
  minBound = sw256 minBound
  maxBound = sw256 maxBound

instance Eq SymWord where
  (S _ x) == (S _ y) = x == y

instance Enum SymWord where
  toEnum i = sw256 (toEnum i)
  fromEnum (S _ x) = fromEnum x

instance OrdSymbolic SymWord where
  (.<) (S _ x) (S _ y) = (.<) x y
