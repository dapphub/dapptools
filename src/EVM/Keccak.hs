module EVM.Keccak (keccak, abiKeccak, newContractAddress) where

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Data.DoubleWord
import qualified Data.ByteArray as BA

import Crypto.Hash

word :: [Int] -> Word256
word xs = sum [ fromIntegral x `shiftL` (8*n)
              | (n, x) <- zip [0..] (reverse xs) ]

word32 :: [Word8] -> Word32
word32 xs = sum [ fromIntegral x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

octets :: Word256 -> [Word8]
octets x = [fromIntegral (shiftR x (8 * i)) | i <- [0..31]]

octets160 :: Word160 -> [Word8]
octets160 x = [fromIntegral (shiftR x (8 * i)) | i <- [0..19]]

keccak :: ByteString -> Word256
keccak bs =
   word (map fromIntegral (take 32 (BA.unpack (hash bs :: Digest Keccak_256))))

abiKeccak :: ByteString -> Word32
abiKeccak bs =
  word32 $ take 4 (BA.unpack (hash bs :: Digest Keccak_256))

rlpWord256 :: Word256 -> ByteString
rlpWord256 x = BS.pack ([0x80, 32] ++ octets x)

rlpWord160 :: Word160 -> ByteString
rlpWord160 x = BS.pack ([0x80, 20] ++ octets160 x)

rlpWord :: Word256 -> ByteString
rlpWord x | x <= 0x7f = BS.pack [fromIntegral x]
rlpWord x =
  let xs = BS.dropWhile (== 0) (rlpWord256 x)
  in BS.cons (0x80 + fromIntegral (BS.length xs)) xs

rlpList :: [ByteString] -> ByteString
rlpList xs =
  let n = sum (map BS.length xs)
  in if n <= 55
     then BS.cons (fromIntegral (0xc0 + n)) (BS.concat xs)
     else
       let ns = rlpWord (fromIntegral n)
       in BS.cons (fromIntegral (0xf7 + BS.length ns)) (BS.concat (ns : xs))

newContractAddress :: Word160 -> Word256 -> Word160
newContractAddress a n =
  fromIntegral
    (keccak $ rlpList [rlpWord160 a, rlpWord n])
