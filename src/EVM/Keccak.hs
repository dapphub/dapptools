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

keccak :: ByteString -> Word256
keccak bs =
   word (map fromIntegral (take 32 (BA.unpack (hash bs :: Digest Keccak_256))))

abiKeccak :: ByteString -> Word32
abiKeccak bs =
  word32 $ take 4 (BA.unpack (hash bs :: Digest Keccak_256))

newContractAddress :: Word256 -> Word256 -> Word256
newContractAddress a n | n < 0x7f =
  0xffffffffffffffffffffffffffffffffffffffff .&.
    (keccak $ BS.pack ([0x80, 32] ++ octets a ++ [fromIntegral n]))
newContractAddress _ _ | otherwise = error "foo"
