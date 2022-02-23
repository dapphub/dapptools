{-# Language FlexibleInstances #-}
{-# Language StrictData #-}

module EVM.Concrete where

import Prelude hiding (Word)

import EVM.RLP
import EVM.Types

import Control.Lens    ((^?), ix)
import Data.Bits       (Bits (..), shiftL, shiftR)
import Data.ByteString (ByteString)
import Data.Maybe      (fromMaybe)
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


sliceMemory :: (Integral a, Integral b) => a -> b -> ByteString -> ByteString
sliceMemory o s =
  byteStringSliceWithDefaultZeroes (num o) (num s)

--writeMemory :: ByteString -> Word -> Word -> Word -> ByteString -> ByteString
--writeMemory bs1 (C _ n) (C _ src) (C _ dst) bs0 =
  --let
    --(a, b) = BS.splitAt (num dst) bs0
    --a'     = BS.replicate (num dst - BS.length a) 0
    -- sliceMemory should work for both cases, but we are using 256 bit
    -- words, whereas ByteString is only defined up to 64 bit. For large n,
    -- src, dst this will cause problems (often in GeneralStateTests).
    -- Later we could reimplement ByteString for 256 bit arguments.
    --c      = if src > num (BS.length bs1)
             --then BS.replicate (num n) 0
             --else sliceMemory src n bs1
    --b'     = BS.drop (num n) b
  --in
    --a <> a' <> c <> b'

--readMemoryWord :: Word -> ByteString -> Word
--readMemoryWord (C _ i) m =
  --if i > (num $ BS.length m) then 0 else
  --let
    --go !a (-1) = a
    --go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                              --(8 * (31 - n))) (n - 1)
    --w = go (0 :: W256) (31 :: Int)
  --in [># SCC "readMemoryWord" #<]
    --C (Lit w) w

--readMemoryWord32 :: Word -> ByteString -> Word
--readMemoryWord32 (C _ i) m =
  --let
    --go !a (-1) = a
    --go !a !n = go (a + shiftL (num $ readByteOrZero (num i + n) m)
                              --(8 * (3 - n))) (n - 1)
  --in [># SCC "readMemoryWord32" #<]
    --w256 $ go (0 :: W256) (3 :: Int)

--setMemoryWord :: Word -> Word -> ByteString -> ByteString
--setMemoryWord (C _ i) (C _ x) =
  --writeMemory (word256Bytes x) 32 0 (num i)

--setMemoryByte :: Word -> Word8 -> ByteString -> ByteString
--setMemoryByte (C _ i) x =
  --writeMemory (BS.singleton x) 1 0 (num i)

--keccakBlob :: ByteString -> Word
--keccakBlob x = C (FromKeccak (ConcreteBuffer x)) (keccak x)

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
createAddress a n = num $ keccak $ rlpList [rlpAddrFull a, rlpWord256 n]

create2Address :: Addr -> W256 -> ByteString -> Addr
create2Address a s b = num $ keccak $ mconcat
  [BS.singleton 0xff, word160Bytes a, word256Bytes $ num s, word256Bytes $ keccak b]
