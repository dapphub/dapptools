{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}

module EVM.Concrete
  ( Concrete
  , word
  , Word (..)
  , Blob (..)
  , wordAt
  , word256Bytes
  ) where

import Prelude hiding (Word, (^))

import EVM.Machine
import EVM.Types (W256 (..), num, toWord512, fromWord512)
import EVM.Types (word, padRight, byteAt)
import EVM.Keccak (keccak)

import Control.Lens    ((^?), ix)
import Data.Bits       (Bits (..), FiniteBits (..))
import Data.ByteString (ByteString)
import Data.DoubleWord (signedWord, unsignedWord)
import Data.Maybe      (fromMaybe)
import Data.Monoid     ((<>))
import Data.String     (IsString)
import Data.Word       (Word8)

import qualified Data.ByteString as BS

data Concrete

wordAt :: Int -> ByteString -> W256
wordAt i bs = word (padRight 32 (BS.drop i bs))

word256Bytes :: W256 -> ByteString
word256Bytes x = BS.pack [byteAt x (31 - i) | i <- [0..31]]

readByteOrZero :: Int -> ByteString -> Word8
readByteOrZero i bs = fromMaybe 0 (bs ^? ix i)

byteStringSliceWithDefaultZeroes :: Int -> Int -> ByteString -> ByteString
byteStringSliceWithDefaultZeroes offset size bs =
  if size == 0
  then ""
  else
    let bs' = BS.take size (BS.drop offset bs)
    in bs' <> BS.replicate (size - BS.length bs') 0

data Whiff = Dull | FromKeccak ByteString
  deriving Show

instance Machine' Concrete where
  w256 = C Dull
  blob = B

  data Word Concrete = C Whiff W256
  newtype Blob Concrete = B ByteString
  newtype Byte Concrete = ConcreteByte Word8
  newtype Memory Concrete = ConcreteMemory ByteString

  wordToByte (C _ x) = ConcreteByte (num (x .&. 0xff))

  exponentiate (C _ x) (C _ y) = w256 (x ^ y)

  sdiv _ (C _ (W256 0)) = 0
  sdiv (C _ (W256 x)) (C _ (W256 y)) =
    let sx = signedWord x
        sy = signedWord y
        k  = if (sx < 0) /= (sy < 0)
             then (-1)
             else 1
    in w256 . W256 . unsignedWord $ k * div (abs sx) (abs sy)

  smod _ (C _ (W256 0)) = 0
  smod (C _ (W256 x)) (C _ (W256 y)) =
    let sx = signedWord x
        sy = signedWord y
        k  = if sx < 0 then (-1) else 1
    in w256 . W256 . unsignedWord $ k * mod (abs sx) (abs sy)

  addmod _ _ (C _ (W256 0)) = 0
  addmod (C _ x) (C _ y) (C _ z) =
    w256 $
      fromWord512
        ((toWord512 x + toWord512 y) `mod` (toWord512 z))

  mulmod _ _ (C _ (W256 0)) = 0
  mulmod (C _ x) (C _ y) (C _ z) =
    w256 $
      fromWord512
        ((toWord512 x * toWord512 y) `mod` (toWord512 z))

  slt (C _ (W256 x)) (C _ (W256 y)) =
    if signedWord x < signedWord y then w256 1 else w256 0

  sgt (C _ (W256 x)) (C _ (W256 y)) =
    if signedWord x > signedWord y then w256 1 else w256 0

  forceConcreteBlob (B x) = x
  forceConcreteWord (C _ x) = x

  sliceMemory o s (ConcreteMemory m) =
    B $ byteStringSliceWithDefaultZeroes (num o) (num s) m

  writeMemory (B bs1) (C _ n) (C _ src) (C _ dst) (ConcreteMemory bs0) =
    let
      (a, b) = BS.splitAt (num dst) bs0
      c      = BS.take (num n) (BS.drop (num src) bs1)
      b'     = BS.drop (num n) b
    in
      ConcreteMemory $
        a <> BS.replicate (num dst - BS.length a) 0 <> c <> b'

  readMemoryWord (C _ i) (ConcreteMemory m) =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                                (8 * (31 - n))) (n - 1)
    in {-# SCC readMemoryWord #-}
      w256 $ go (0 :: W256) (31 :: Int)

  readMemoryWord32 (C _ i) (ConcreteMemory m) =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                                (8 * (3 - n))) (n - 1)
    in {-# SCC readMemoryWord32 #-}
      w256 $ go (0 :: W256) (3 :: Int)

  setMemoryWord (C _ i) (C _ x) m =
    writeMemory (B (word256Bytes x)) 32 0 (num i) m

  setMemoryByte (C _ i) (ConcreteByte x) m =
    writeMemory (B (BS.singleton x)) 1 0 (num i) m

  readBlobWord (C _ i) (B x) =
    w256 (wordAt (num i) x)

  blobSize (B x) = w256 (num (BS.length x))

  keccakBlob (B x) = C (FromKeccak x) (keccak x)

deriving instance Bits (Byte Concrete)
deriving instance FiniteBits (Byte Concrete)
deriving instance Enum (Byte Concrete)
deriving instance Eq (Byte Concrete)
deriving instance Integral (Byte Concrete)
deriving instance IsString (Blob Concrete)
deriving instance Monoid (Blob Concrete)
deriving instance Monoid (Memory Concrete)
deriving instance Num (Byte Concrete)
deriving instance Ord (Byte Concrete)
deriving instance Real (Byte Concrete)
deriving instance Show (Blob Concrete)

instance Show (Word Concrete) where
  show (C Dull x) = show x
  show (C whiff x) = show whiff ++ ": " ++ show x

instance Read (Word Concrete) where
  readsPrec n s =
    case readsPrec n s of
      [(x, r)] -> [(C Dull x, r)]
      _ -> []

instance Bits (Word Concrete) where
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

instance FiniteBits (Word Concrete) where
  finiteBitSize (C _ x) = finiteBitSize x
  countLeadingZeros (C _ x) = countLeadingZeros x
  countTrailingZeros (C _ x) = countTrailingZeros x

instance Eq (Word Concrete) where
  (C _ x) == (C _ y) = x == y

instance Enum (Word Concrete) where
  toEnum i = w256 (toEnum i)
  fromEnum (C _ x) = fromEnum x

instance Integral (Word Concrete) where
  quotRem (C _ x) (C _ y) =
    let (a, b) = quotRem x y
    in (w256 a, w256 b)
  toInteger (C _ x) = toInteger x

instance Num (Word Concrete) where
  (C _ x) + (C _ y) = w256 (x + y)
  (C _ x) * (C _ y) = w256 (x * y)
  abs (C _ x) = w256 (abs x)
  signum (C _ x) = w256 (signum x)
  fromInteger x = w256 (fromInteger x)
  negate (C _ x) = w256 (negate x)

instance Real (Word Concrete) where
  toRational (C _ x) = toRational x

instance Ord (Word Concrete) where
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
