{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}

module EVM.Concrete where

import Prelude hiding (Word, (^))

import EVM.Machine
import EVM.Types (W256 (..), num, toWord512, fromWord512)
import EVM.Keccak (keccak)

import Control.Lens    ((^?), ix, lens)
import Data.Bits       (Bits, testBit, shiftL, shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.DoubleWord (signedWord, unsignedWord, fromHiAndLo)
import Data.Maybe      (fromMaybe)
import Data.Monoid     ((<>))
import Data.String     (IsString)
import Data.Word       (Word8, Word32)

import qualified Data.ByteString    as BS
import qualified Data.Serialize.Get as Cereal

data Concrete

pad :: Int -> ByteString -> ByteString
pad n xs = BS.replicate (n - BS.length xs) 0 <> xs

padBig :: Int -> ByteString -> ByteString
padBig n xs = xs <> BS.replicate (n - BS.length xs) 0

word :: ByteString -> W256
word xs = case Cereal.runGet m (pad 32 xs) of
            Left _ -> error "internal error"
            Right x -> W256 x
  where
    m = do a <- Cereal.getWord64be
           b <- Cereal.getWord64be
           c <- Cereal.getWord64be
           d <- Cereal.getWord64be
           return $ fromHiAndLo (fromHiAndLo a b) (fromHiAndLo c d)

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

wordAt :: Int -> ByteString -> W256
wordAt i bs = word (padBig 32 (BS.drop i bs))

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
    in bs' <> BS.replicate (BS.length bs' - size) 0

word32 :: Integral a => [a] -> Word32
word32 xs = sum [ num x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

{-#
  SPECIALIZE word256At
    :: Functor f => Word Concrete -> (Word Concrete -> f (Word Concrete))
    -> Memory Concrete -> f (Memory Concrete)
 #-}

word256At
  :: (Machine e, Functor f)
  => Word e -> (Word e -> f (Word e))
  -> Memory e -> f (Memory e)
word256At i = lens getter setter where
  getter m = readMemoryWord i m
  setter m x = setMemoryWord i x m

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

instance Machine' Concrete where
  w256 = C
  blob = B

  newtype Word Concrete = C W256
  newtype Blob Concrete = B ByteString
  newtype Byte Concrete = ConcreteByte Word8
  newtype Memory Concrete = ConcreteMemory ByteString

  wordToByte (C x) = ConcreteByte (num (x .&. 0xff))

  exponentiate (C x) (C y) = C (x ^ y)

  sdiv _ (C (W256 0)) = 0
  sdiv (C (W256 x)) (C (W256 y)) =
    let sx = signedWord x
        sy = signedWord y
        k  = if (sx < 0) /= (sy < 0)
             then (-1)
             else 1
    in C . W256 . unsignedWord $ k * div (abs sx) (abs sy)

  smod _ (C (W256 0)) = 0
  smod (C (W256 x)) (C (W256 y)) =
    let sx = signedWord x
        sy = signedWord y
        k  = if sx < 0 then (-1) else 1
    in C . W256 . unsignedWord $ k * mod (abs sx) (abs sy)

  addmod _ _ (C (W256 0)) = 0
  addmod (C x) (C y) (C z) =
    C $ fromWord512
         ((toWord512 x + toWord512 y) `mod` (toWord512 z))

  mulmod _ _ (C (W256 0)) = 0
  mulmod (C x) (C y) (C z) =
    C $ fromWord512
          ((toWord512 x * toWord512 y) `mod` (toWord512 z))

  slt (C (W256 x)) (C (W256 y)) =
    if signedWord x < signedWord y then 1 else 0

  sgt (C (W256 x)) (C (W256 y)) =
    if signedWord x > signedWord y then 1 else 0

  forceConcreteBlob (B x) = x
  forceConcreteWord (C x) = x

  sliceMemory o s (ConcreteMemory m) =
    B $ byteStringSliceWithDefaultZeroes (num o) (num s) m

  writeMemory (B bs1) (C n) (C src) (C dst) (ConcreteMemory bs0) =
    let
      (a, b) = BS.splitAt (num dst) bs0
      c      = BS.take (num n) (BS.drop (num src) bs1)
      b'     = BS.drop (num n) b
    in
      ConcreteMemory $
        a <> BS.replicate (num dst - BS.length a) 0 <> c <> b'

  readMemoryWord (C i) (ConcreteMemory m) =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                                (8 * (31 - n))) (n - 1)
    in {-# SCC word256At_getter #-}
      C $ go (0 :: W256) (31 :: Int)

  setMemoryWord (C i) (C x) m =
    writeMemory (B (word256Bytes x)) 32 0 (num i) m

  setMemoryByte (C i) (ConcreteByte x) m =
    writeMemory (B (BS.singleton x)) 1 0 (num i) m

  readBlobWord (C i) (B x) =
    C (wordAt (num i) x)

  blobSize (B x) = C (num (BS.length x))

  keccakBlob (B x) = C (keccak x)

deriving instance Bits (Byte Concrete)
deriving instance Bits (Word Concrete)
deriving instance Enum (Byte Concrete)
deriving instance Enum (Word Concrete)
deriving instance Eq (Byte Concrete)
deriving instance Eq (Word Concrete)
deriving instance Integral (Byte Concrete)
deriving instance Integral (Word Concrete)
deriving instance IsString (Blob Concrete)
deriving instance Monoid (Blob Concrete)
deriving instance Monoid (Memory Concrete)
deriving instance Num (Byte Concrete)
deriving instance Num (Word Concrete)
deriving instance Ord (Byte Concrete)
deriving instance Ord (Word Concrete)
deriving instance Real (Byte Concrete)
deriving instance Real (Word Concrete)
deriving instance Show (Blob Concrete)
deriving instance Show (Word Concrete)

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
