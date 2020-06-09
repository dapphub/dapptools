{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language StrictData #-}


module EVM.Concrete where

import Prelude hiding (Word, (^))

import EVM.Keccak (keccak)
import EVM.RLP
import EVM.Types (Addr(..), SAddr(..), W256 (..), num, toWord512, fromWord512, fromSizzle, toSizzle)
import EVM.Types (word, padRight, word160Bytes, word256Bytes, truncpad)

import Control.Lens    ((^?), ix)
import Data.Bits       (Bits (..), FiniteBits (..), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Maybe      (fromMaybe)
import Data.Semigroup  ((<>))
import Data.Word       (Word8)
import Data.SBV hiding (Word)
import qualified Data.SBV as SBV
import Data.SBV.Internals hiding (Word)
import qualified Data.ByteString as BS

wordAt :: Int -> ByteString -> W256
wordAt i bs =
  word (padRight 32 (BS.drop i bs))

swordAt :: Int -> [SWord 8] -> SymWord
swordAt i bs = sw256 . fromBytes $ truncpad 32 $ drop i bs

readByteOrZero :: Int -> [SWord 8] -> SWord 8
readByteOrZero i bs = fromMaybe 0 (bs ^? ix i)

byteStringSliceWithDefaultZeroes :: Int -> Int -> ByteString -> ByteString
byteStringSliceWithDefaultZeroes offset size bs =
  if size == 0
  then ""
  -- else if offset > BS.length bs
  -- then BS.replicate size 0
  -- todo: this ^^ should work, investigate why it causes more GST fails
  else
    let bs' = BS.take size (BS.drop offset bs)
    in bs' <> BS.replicate (size - BS.length bs') 0


-- This type can give insight into where some (random looking) number came from
data Whiff = Dull
           | FromKeccak ByteString
           | Var String
           | InfixBinOp String Whiff Whiff
           | BinOp String Whiff Whiff
           | UnOp String Whiff
  deriving Show

w256 :: W256 -> Word
w256 = C Dull

data Word = C Whiff W256 --maybe to remove completely in the future

data SymWord = S Whiff (SWord 256)

sw256 :: SWord 256 -> SymWord
sw256 = S Dull

litWord :: Word -> (SymWord)
litWord (C whiff a) = S whiff (literal $ toSizzle a)

maybeLitWord :: SymWord -> Maybe Word
maybeLitWord (S whiff a) = fmap (C whiff . fromSizzle) (unliteral a)

maybeLitAddr :: SAddr -> Maybe Addr
maybeLitAddr (SAddr a) = fmap fromSizzle (unliteral a)

w256lit :: W256 -> SymWord
w256lit = S Dull . literal . toSizzle


wordToByte :: Word -> Word8
wordToByte (C _ x) = num (x .&. 0xff)

exponentiate :: Word -> Word -> Word
exponentiate (C _ x) (C _ y) = w256 (x ^ y)

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

wordValue :: Word -> W256
wordValue (C _ x) = x

sliceMemory :: (Integral a, Integral b) => a -> b -> ByteString -> ByteString
sliceMemory o s =
  byteStringSliceWithDefaultZeroes (num o) (num s)

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

readBlobWord :: Word -> ByteString -> Word
readBlobWord (C _ i) x =
  if i > num (BS.length x)
  then 0
  else w256 (wordAt (num i) x)

readSWord :: Word -> [SWord 8] -> SymWord
readSWord (C _ i) x =
  if i > num (length x)
  then 0
  else swordAt (num i) x


blobSize :: ByteString -> Word
blobSize x = w256 (num (BS.length x))

keccakBlob :: ByteString -> Word
keccakBlob x = C (FromKeccak x) (keccak x)

instance Show Word where
  show (C Dull x) = show x
  show (C (Var var) x) = var ++ ": " ++ show x
  show (C (InfixBinOp symbol x y) z) = show x ++ symbol ++ show y  ++ ": " ++ show z
  show (C (BinOp symbol x y) z) = symbol ++ show x ++ show y  ++ ": " ++ show z
  show (C (UnOp symbol x) z) = symbol ++ show x ++ ": " ++ show z
  show (C whiff x) = show whiff ++ ": " ++ show x

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
-- instance SFiniteBits SymWord where
--   sFiniteBitSize (S _ x) = sFiniteBitSize x
--   sCountLeadingZeros (S _ x) = sCountLeadingZeros x
--   sCountTrailingZeros (S _ x) = sCountTrailingZeros x

instance Bounded SymWord where
  minBound = sw256 minBound
  maxBound = sw256 maxBound

instance Eq SymWord where
  (S _ x) == (S _ y) = x == y

instance Enum SymWord where
  toEnum i = sw256 (toEnum i)
  fromEnum (S _ x) = fromEnum x

--deriving instance SIntegral SymWord
  -- quotRem (S _ x) (S _ y) =
  --   let (a, b) = quotRem x y
  --   in (sw256 a, sw256 b)
--  toInteger (S _ x) = toInteger x

-- instance Real SymWord where
--   toRational (S _ x) = toRational x

instance OrdSymbolic SymWord where
  (.<) (S _ x) (S _ y) = (.<) x y

instance Read Word where
  readsPrec n s =
    case readsPrec n s of
      [(x, r)] -> [(C Dull x, r)]
      _ -> []

instance Bits Word where
  (C _ x) .&. (C _ y) = w256 (x .&. y)
  (C _ x) .|. (C _ y) = w256 (x .|. y)
  (C _ x) `xor` (C _ y) = w256 (x `xor` y)
  complement (C _ x) = w256 (complement x)
  shift (C _ x) i = w256 (shift x i)
  rotate (C _ x) i = w256 (rotate x i)
  bitSize (C _ x) = bitSize x
  bitSizeMaybe (C _ x) = bitSizeMaybe x
  isSigned (C _ x) = isSigned x
  testBit (C _ x) = testBit x
  bit i = w256 (bit i)
  popCount (C _ x) = popCount x

instance FiniteBits Word where
  finiteBitSize (C _ x) = finiteBitSize x
  countLeadingZeros (C _ x) = countLeadingZeros x
  countTrailingZeros (C _ x) = countTrailingZeros x

instance Bounded Word where
  minBound = w256 minBound
  maxBound = w256 maxBound

instance Eq Word where
  (C _ x) == (C _ y) = x == y

instance Enum Word where
  toEnum i = w256 (toEnum i)
  fromEnum (C _ x) = fromEnum x

instance Integral Word where
  quotRem (C _ x) (C _ y) =
    let (a, b) = quotRem x y
    in (w256 a, w256 b)
  toInteger (C _ x) = toInteger x

instance Num Word where
  (C _ x) + (C _ y) = w256 (x + y)
  (C _ x) * (C _ y) = w256 (x * y)
  abs (C _ x) = w256 (abs x)
  signum (C _ x) = w256 (signum x)
  fromInteger x = w256 (fromInteger x)
  negate (C _ x) = w256 (negate x)

instance Real Word where
  toRational (C _ x) = toRational x

instance Ord Word where
  compare (C _ x) (C _ y) = compare x y

-- Copied from the standard library just to get specialization.
-- We also use bit operations instead of modulo and multiply.
-- (This operation was significantly slow.)
(^) :: W256 -> W256 -> W256
x0 ^ y0 | y0 < 0    = errorWithoutStackTrace "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where
          f x y | not (testBit y 0) = f (x * x) (y `shiftR` 1)
                | y == 1      = x
                | otherwise   = g (x * x) ((y - 1) `shiftR` 1) x
          g x y z | not (testBit y 0) = g (x * x) (y `shiftR` 1) z
                  | y == 1      = x * z
                  | otherwise   = g (x * x) ((y - 1) `shiftR` 1) (x * z)

createAddress :: Addr -> W256 -> Addr
createAddress a n = num $ keccak $ rlpList [rlpWord160 a, rlpWord256 n]

create2Address :: Addr -> W256 -> ByteString -> Addr
create2Address a s b = num $ keccak $ mconcat
  [BS.singleton 0xff, word160Bytes a, word256Bytes $ num s, word256Bytes $ keccak b]
