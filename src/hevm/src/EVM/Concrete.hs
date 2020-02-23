{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module EVM.Concrete where

import Prelude hiding (Word, (^))

import EVM.Keccak (keccak)
import EVM.RLP
import EVM.Types (Addr, W256 (..), num, toWord512, fromWord512)
import EVM.Types (word, padRight, word160Bytes, word256Bytes)

import Control.Lens    ((^?), ix)
import Data.Bits       (Bits (..), FiniteBits (..), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.DoubleWord (signedWord, unsignedWord)
import Data.Maybe      (fromMaybe)
import Data.Semigroup  ((<>))
import Data.Word       (Word8)

import qualified Data.ByteString as BS

wordAt :: Int -> ByteString -> W256
wordAt i bs =
  word (padRight 32 (BS.drop i bs))

readByteOrZero :: Int -> ByteString -> Word8
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

data Whiff = Dull | FromKeccak ByteString
  deriving Show

w256 :: W256 -> Word
w256 = C Dull

data Word = C Whiff W256

wordToByte :: Word -> Word8
wordToByte (C _ x) = num (x .&. 0xff)

exponentiate :: Word -> Word -> Word
exponentiate (C _ x) (C _ y) = w256 (x ^ y)

sdiv :: Word -> Word -> Word
sdiv _ (C _ (W256 0)) = 0
sdiv (C _ (W256 x)) (C _ (W256 y)) =
  let sx = signedWord x
      sy = signedWord y
  in w256 . W256 . unsignedWord $ quot sx sy

smod :: Word -> Word -> Word
smod _ (C _ (W256 0)) = 0
smod (C _ (W256 x)) (C _ (W256 y)) =
  let sx = signedWord x
      sy = signedWord y
  in w256 . W256 . unsignedWord $ rem sx sy

addmod :: Word -> Word -> Word -> Word
addmod _ _ (C _ (W256 0)) = 0
addmod (C _ x) (C _ y) (C _ z) =
  w256 $
    fromWord512
      ((toWord512 x + toWord512 y) `mod` (toWord512 z))

mulmod :: Word -> Word -> Word -> Word
mulmod _ _ (C _ (W256 0)) = 0
mulmod (C _ x) (C _ y) (C _ z) =
  w256 $
    fromWord512
      ((toWord512 x * toWord512 y) `mod` (toWord512 z))

slt :: Word -> Word -> Word
slt (C _ (W256 x)) (C _ (W256 y)) =
  if signedWord x < signedWord y then w256 1 else w256 0

sgt :: Word -> Word -> Word
sgt (C _ (W256 x)) (C _ (W256 y)) =
  if signedWord x > signedWord y then w256 1 else w256 0

shr :: Word -> Word -> Word
shr x n =
  if n > 255 then 0
  else shiftR x (num n)

shl :: Word -> Word -> Word
shl x n =
  if n > 255 then 0
  else shiftL x (num n)

sar :: Word -> Word -> Word
sar (C _ (W256 x)) (C _ (W256 n)) =
  let
    sx = signedWord x
  in
    if n > 255 && sx > 0 then 0
    else if n > 255 && sx < 0 then -1
    else w256 (num (unsignedWord (shiftR sx (num n))))

wordValue :: Word -> W256
wordValue (C _ x) = x

sliceMemory :: (Integral a, Integral b) => a -> b -> ByteString -> ByteString
sliceMemory o s m =
  byteStringSliceWithDefaultZeroes (num o) (num s) m

writeMemory :: ByteString -> Word -> Word -> Word -> ByteString -> ByteString
writeMemory bs1 (C _ n) (C _ src) (C _ dst) bs0 =
  let
    (a, b) = BS.splitAt (num dst) bs0
    a'     = BS.replicate (num dst - BS.length a) 0
    -- sliceMemory should work for both cases, but we are using 256 bit
    -- words, whereas ByteString is only defined up to 64 bit. For large n,
    -- src, dst this will cause problems (often in GeneralStateTests).
    -- Later we could reimplement ByteString for 256 bit arguments.
    c      = if src > num (BS.length bs1)
             then BS.replicate (num n) 0
             else sliceMemory src n bs1
    b'     = BS.drop (num (n)) b
  in
    a <> a' <> c <> b'

readMemoryWord :: Word -> ByteString -> Word
readMemoryWord (C _ i) m =
  let
    go !a (-1) = a
    go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                              (8 * (31 - n))) (n - 1)
  in {-# SCC readMemoryWord #-}
    w256 $ go (0 :: W256) (31 :: Int)

readMemoryWord32 :: Word -> ByteString -> Word
readMemoryWord32 (C _ i) m =
  let
    go !a (-1) = a
    go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                              (8 * (3 - n))) (n - 1)
  in {-# SCC readMemoryWord32 #-}
    w256 $ go (0 :: W256) (3 :: Int)

setMemoryWord :: Word -> Word -> ByteString -> ByteString
setMemoryWord (C _ i) (C _ x) m =
  writeMemory (word256Bytes x) 32 0 (num i) m

setMemoryByte :: Word -> Word8 -> ByteString -> ByteString
setMemoryByte (C _ i) x m =
  writeMemory (BS.singleton x) 1 0 (num i) m

readBlobWord :: Word -> ByteString -> Word
readBlobWord (C _ i) x =
  if i > num (BS.length x)
  then 0
  else w256 (wordAt (num i) x)

blobSize :: ByteString -> Word
blobSize x = w256 (num (BS.length x))

keccakBlob :: ByteString -> Word
keccakBlob x = C (FromKeccak x) (keccak x)

instance Show Word where
  show (C Dull x) = show x
  show (C whiff x) = show whiff ++ ": " ++ show x

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
  testBit (C _ x) i = testBit x i
  bit i = w256 (bit i)
  popCount (C _ x) = popCount x

instance FiniteBits Word where
  finiteBitSize (C _ x) = finiteBitSize x
  countLeadingZeros (C _ x) = countLeadingZeros x
  countTrailingZeros (C _ x) = countTrailingZeros x

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
create2Address a s b = num $ keccak $ mconcat $
  [BS.singleton 0xff, word160Bytes a, word256Bytes $ num s, word256Bytes $ keccak b]
