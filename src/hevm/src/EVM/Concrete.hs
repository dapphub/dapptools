{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language StrictData #-}


module EVM.Concrete where

import Prelude hiding (Word, (^))

import EVM.Keccak (keccak)
import EVM.RLP
import EVM.Types (Addr(..), W256 (..), num)
import EVM.Types (word, padRight, word160Bytes, word256Bytes)

import Data.Bits       (Bits (..), FiniteBits (..), shiftR)
import Data.ByteString (ByteString)
import Data.Semigroup  ((<>))
import Data.Word       (Word8)
import qualified Data.ByteString as BS

wordAt :: Int -> ByteString -> W256
wordAt i bs =
  word (padRight 32 (BS.drop i bs))

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


-- This type can give insight into the provenance of a term
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

wordToByte :: Word -> Word8
wordToByte (C _ x) = num (x .&. 0xff)

wordValue :: Word -> W256
wordValue (C _ x) = x

sliceMemory :: (Integral a, Integral b) => a -> b -> ByteString -> ByteString
sliceMemory o s =
  byteStringSliceWithDefaultZeroes (num o) (num s)

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
  show (C (Var var) x) = var ++ ": " ++ show x
  show (C (InfixBinOp symbol x y) z) = show x ++ symbol ++ show y  ++ ": " ++ show z
  show (C (BinOp symbol x y) z) = symbol ++ show x ++ show y  ++ ": " ++ show z
  show (C (UnOp symbol x) z) = symbol ++ show x ++ ": " ++ show z
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
