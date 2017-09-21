{-# Language ConstraintKinds #-}

module EVM.Machine
  ( Machine' (..)
  , Machine
  ) where

import Prelude hiding (Word)

import EVM.Types (W256)

import Data.Bits (Bits, FiniteBits)
import Data.ByteString (ByteString)
import Data.String (IsString)

-- | This abstraction lets us reuse the EVM code with different data
-- representations.  In particular, we will define "concrete" and
-- "symbolic" machines.
class Machine' e where
  data Byte e
  data Word e
  data Blob e
  data Memory e

  w256 :: W256 -> Word e
  blob :: ByteString -> Blob e
  wordToByte :: Word e -> Byte e

  sdiv :: Word e -> Word e -> Word e
  slt  :: Word e -> Word e -> Word e
  sgt  :: Word e -> Word e -> Word e
  smod :: Word e -> Word e -> Word e
  addmod :: Word e -> Word e -> Word e -> Word e
  mulmod :: Word e -> Word e -> Word e -> Word e

  exponentiate :: Word e -> Word e -> Word e

  forceConcreteBlob :: Blob e -> ByteString
  forceConcreteWord :: Word e -> W256

  sliceMemory :: Word e -> Word e -> Memory e -> Blob e
  writeMemory :: Blob e -> Word e -> Word e -> Word e -> Memory e -> Memory e

  setMemoryByte :: Word e -> Byte e -> Memory e -> Memory e

  readMemoryWord :: Word e -> Memory e -> Word e
  readMemoryWord32 :: Word e -> Memory e -> Word e
  setMemoryWord :: Word e -> Word e -> Memory e -> Memory e

  readBlobWord :: Word e -> Blob e -> Word e
  blobSize :: Blob e -> Word e

  keccakBlob :: Blob e -> Word e

type Machine e =
  ( Machine' e
  , Show (Word e)
  , Num (Word e)
  , Num (Byte e)
  , Integral (Word e)
  , Bits (Word e)
  , FiniteBits (Word e)
  , Monoid (Memory e)
  , Monoid (Blob e)
  , IsString (Blob e)
  , Show (Blob e)
  )
