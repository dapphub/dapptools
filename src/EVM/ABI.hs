{-# Language OverloadedStrings #-}
{-# Language BangPatterns #-}
{-# Language LambdaCase #-}
{-# Language FlexibleContexts #-}
{-# Language TemplateHaskell #-}
{-# Language DeriveGeneric, DeriveAnyClass #-}

module EVM.ABI where

import EVM.Keccak

import Control.Monad
import Data.Binary.Put
import Data.Bits
import Data.DoubleWord
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Vector (Vector)
import Text.Printf (printf)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.Vector as Vector
import qualified Data.Text as Text

data AbiValue
  = AbiUInt         !Int !Word256
  | AbiInt          !Int !Int256
  | AbiAddress      !Word160
  | AbiBool         !Bool
  | AbiBytes        !Int !BS.ByteString
  | AbiBytesDynamic !BS.ByteString
  | AbiString       !Text
  | AbiArrayDynamic !AbiType !(Vector AbiValue)
  | AbiArray        !Int !AbiType !(Vector AbiValue)
  deriving (Show, Read, Eq, Ord)

data AbiType
  = AbiUIntType         !Int
  | AbiIntType          !Int
  | AbiAddressType
  | AbiBoolType
  | AbiBytesType        !Int
  | AbiBytesDynamicType
  | AbiStringType
  | AbiArrayDynamicType !AbiType
  | AbiArrayType        !Int !AbiType
  deriving (Show, Read, Eq, Ord)

data AbiKind = Dynamic | Static

abiKind :: AbiType -> AbiKind
abiKind = \case
  AbiBytesDynamicType   -> Dynamic
  AbiStringType         -> Dynamic
  AbiArrayDynamicType _ -> Dynamic
  AbiArrayType _ t      -> abiKind t
  _                     -> Static

abiValueType :: AbiValue -> AbiType
abiValueType = \case
  AbiUInt n _         -> AbiUIntType n
  AbiInt n _          -> AbiIntType  n
  AbiAddress _        -> AbiAddressType
  AbiBool _           -> AbiBoolType
  AbiBytes n _        -> AbiBytesType n
  AbiBytesDynamic _   -> AbiBytesDynamicType
  AbiString _         -> AbiStringType
  AbiArrayDynamic t _ -> AbiArrayDynamicType t
  AbiArray n t _      -> AbiArrayType n t

putAbi :: AbiValue -> Put
putAbi = \case
  AbiUInt n x -> do
    let word32Count = 8 * (div (n + 255) 256)
    forM_ (reverse [0 .. word32Count - 1]) $
      \i -> putWord32be (fromIntegral (shiftR x (i * 32) .&. 0xffffffff) )
  AbiInt n x -> putAbi (AbiUInt n (fromIntegral x))
  AbiAddress x -> putAbi (AbiUInt 160 (fromIntegral x))
  AbiBool x -> putAbi (AbiUInt 8 (if x then 1 else 0))
  AbiBytes n xs -> do
    let
      word32Count = 8 * (div (n + 255) 256)
      word8Count = word32Count * 4
    forM_ [0 .. n - 1] $
      \i -> putWord8 (BS.index xs i)
    replicateM_ (word8Count - n) (putWord8 0)
  AbiBytesDynamic xs -> do
    let
      n = BS.length xs
      word32Count = 8 * (div (n + 255) 256)
      word8Count = word32Count * 4
    putAbi (AbiUInt 256 (fromIntegral n))
    forM_ [0 .. n - 1] $
      \i -> putWord8 (BS.index xs i)
    replicateM_ (word8Count - n) (putWord8 0)
  AbiString s -> putAbi (AbiBytesDynamic (encodeUtf8 s))
  AbiArrayDynamic _ xs -> do
    putAbi (AbiUInt 256 (fromIntegral (Vector.length xs)))
    abiSeq xs
  AbiArray _ _ xs ->
    abiSeq xs

abiTail :: AbiValue -> Put
abiTail x =
  case abiKind (abiValueType x) of
    Static  -> return ()
    Dynamic -> putAbi x

abiValueSize :: AbiValue -> Int
abiValueSize x = 
  case x of
    AbiUInt n _  -> 32 * (div (n + 255) 256)
    AbiInt  n _  -> 32 * (div (n + 255) 256)
    AbiBytes n _ -> 32 * (div (n + 255) 256)
    AbiAddress _ -> 32
    AbiBool _    -> 32
    AbiArray _ _ xs -> Vector.sum (Vector.map abiHeadSize xs) +
                       Vector.sum (Vector.map abiTailSize xs)
    AbiBytesDynamic xs -> 32 + 32 * (div (BS.length xs + 255) 256)
    AbiArrayDynamic _ xs -> 32 + Vector.sum (Vector.map abiHeadSize xs) +
                                Vector.sum (Vector.map abiTailSize xs)
    AbiString s -> 32 + 32 * (div (Text.length s + 255) 256)

abiTailSize :: AbiValue -> Int
abiTailSize x =
  case abiKind (abiValueType x) of
    Static -> 0
    Dynamic ->
      case x of
        AbiString s -> 32 + 32 * (div (Text.length s + 255) 256)
        AbiBytesDynamic s -> 32 + 32 * (div (BS.length s + 255) 256)
        AbiArrayDynamic _ xs -> 32 + Vector.sum (Vector.map abiValueSize xs)
        AbiArray _ _ xs -> Vector.sum (Vector.map abiValueSize xs)
        _ -> error "impossible"
        

abiHeadSize :: AbiValue -> Int
abiHeadSize x =
  case abiKind (abiValueType x) of
    Dynamic -> 32
    Static ->
      case x of
        AbiUInt n _  -> 32 * (div (n + 255) 256)
        AbiInt  n _  -> 32 * (div (n + 255) 256)
        AbiBytes n _ -> 32 * (div (n + 255) 256)
        AbiAddress _ -> 32
        AbiBool _    -> 32
        AbiArray _ _ xs -> Vector.sum (Vector.map abiHeadSize xs) +
                           Vector.sum (Vector.map abiTailSize xs)
        AbiBytesDynamic _ -> 32
        AbiArrayDynamic _ _ -> 32
        AbiString _       -> 32

abiSeq :: Vector AbiValue -> Put
abiSeq xs =
  do snd $ Vector.foldl' f (headSize, return ()) (Vector.zip xs tailSizes)
     Vector.sequence_ (Vector.map abiTail xs)
  where
    headSize = Vector.sum $ Vector.map abiHeadSize xs
    tailSizes = Vector.map abiTailSize xs
    f (i, m) (x, j) = 
      case abiKind (abiValueType x) of
        Static -> (i, m >> putAbi x)
        Dynamic -> (i + j, m >> putAbi (AbiUInt 256 (fromIntegral i)))

encodeAbiValue :: AbiValue -> BS.ByteString
encodeAbiValue = BSLazy.toStrict . runPut . putAbi

abiCalldata :: Text -> Vector AbiValue -> BS.ByteString
abiCalldata s xs = BSLazy.toStrict . runPut $ do
  putWord32be (abiKeccak (encodeUtf8 s))
  abiSeq xs
  
hexify :: BS.ByteString -> Text
hexify s = Text.pack (concatMap (printf "%02x") (BS.unpack s))

-- This code turned out kind of horrible.
-- There should be a nicer way to write it.
