{-# Language CPP #-}

#ifdef __GHCJS__
{-# Language JavaScriptFFI #-}
#endif

module EVM.Keccak (word256Bytes, keccak, abiKeccak, newContractAddress, newContractAddressCREATE2) where

import EVM.Types

import Control.Arrow ((>>>))

import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word

#ifdef __GHCJS__
import qualified Data.JSString as JS
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text
import qualified Data.ByteString.Base64 as BS64

foreign import javascript unsafe
  "keccakBase64($1)"
  keccakBase64 :: JS.JSString -> JS.JSString

keccakBytes =
  BS64.encode
    >>> Text.decodeUtf8
    >>> Text.unpack
    >>> JS.pack
    >>> keccakBase64
    >>> JS.unpack
    >>> Text.pack
    >>> Text.encodeUtf8
    >>> BS64.decodeLenient

#else
import Crypto.Hash
import qualified Data.ByteArray as BA

keccakBytes =
  (hash :: ByteString -> Digest Keccak_256)
    >>> BA.unpack
    >>> BS.pack

#endif

keccakBytes :: ByteString -> ByteString

word256Bytes :: W256 -> ByteString
word256Bytes x = BS.pack [byteAt x (31 - i) | i <- [0..31]]

word160Bytes :: Addr -> ByteString
word160Bytes x = BS.pack [byteAt (addressWord160 x) (19 - i) | i <- [0..19]]

word32 :: [Word8] -> Word32
word32 xs = sum [ fromIntegral x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

octets :: W256 -> [Word8]
octets x =
  dropWhile (== 0) [fromIntegral (shiftR x (8 * i)) | i <- reverse [0..31]]

octets160 :: Addr -> [Word8]
octets160 x =
  dropWhile (== 0) [fromIntegral (shiftR x (8 * i)) | i <- reverse [0..19]]

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

rlpWord256 :: W256 -> ByteString
rlpWord256 0 = BS.pack [0x80]
rlpWord256 x | x <= 0x7f = BS.pack [fromIntegral x]
rlpWord256 x =
  let xs = octets x
  in BS.pack ([0x80 + fromIntegral (length xs)] ++ xs)

rlpWord160 :: Addr -> ByteString
rlpWord160 0 = BS.pack [0x80]
rlpWord160 x =
  let xs = octets160 x
  in BS.pack ([0x80 + fromIntegral (length xs)] ++ xs)

rlpList :: [ByteString] -> ByteString
rlpList xs =
  let n = sum (map BS.length xs)
  in if n <= 55
     then BS.cons (fromIntegral (0xc0 + n)) (BS.concat xs)
     else
       let ns = rlpWord256 (fromIntegral n)
       in BS.cons (fromIntegral (0xf7 + BS.length ns)) (BS.concat (ns : xs))

newContractAddress :: Addr -> W256 -> Addr
newContractAddress a n =
  fromIntegral
    (keccak $ rlpList [rlpWord160 a, rlpWord256 n])

newContractAddressCREATE2 :: Addr -> W256 -> ByteString -> Addr
newContractAddressCREATE2 a s b =
  fromIntegral (keccak $ BS.cons (fromIntegral (0xff :: Integer)) (BS.concat $ [word160Bytes a, word256Bytes $ num s, word256Bytes $ keccak b]))
