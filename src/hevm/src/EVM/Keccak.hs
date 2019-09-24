{-# Language CPP #-}

#ifdef __GHCJS__
{-# Language JavaScriptFFI #-}
#endif

module EVM.Keccak (keccak, abiKeccak) where

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
