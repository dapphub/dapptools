module EVM.Keccak (keccak, abiKeccak) where

import EVM.Types

import Control.Arrow ((>>>))

import Data.Bits
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import Data.Word

import Crypto.Hash
import qualified Data.ByteArray as BA

keccakBytes :: ByteString -> ByteString
keccakBytes =
  (hash :: ByteString -> Digest Keccak_256)
    >>> BA.unpack
    >>> BS.pack


word32 :: [Word8] -> Word32
word32 xs = sum [ fromIntegral x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

keccak :: ByteString -> W256
keccak =
  keccakBytes
    >>> BS.take 32
    >>> word

abiKeccak :: ByteString -> Word32
abiKeccak =
  keccakBytes
    >>> BS.take 4
    >>> BS.unpack
    >>> word32
