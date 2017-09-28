{-# Language GADTs #-}
{-# Language NamedFieldPuns #-}

module EVM.Stepper
  ( Action (..)
  , Failure (..)
  , Stepper
  , exec
  , execFully
  , execFullyOrFail
  , decode
  , fail
  , quiz
  , evm
  , note
  )
where

-- This module is an abstract definition of EVM steppers.
-- Steppers can be run as TTY debuggers or as CLI test runners.
--
-- The implementation uses the operational monad pattern
-- as the framework for monadic interpretation.
--
-- Note: this is a sketch of a work in progress!

import Prelude hiding (fail)

import Control.Monad.Operational (Program, singleton)
import Data.Binary.Get (runGetOrFail)
import Data.Text (Text)

import EVM (EVM, VMResult (VMFailure, VMSuccess), Error (Query), Query)

import EVM.ABI (AbiType, AbiValue, getAbi)
import EVM.Machine (Machine, Blob)
import EVM.Concrete (Concrete, Blob (B))

import qualified Data.ByteString.Lazy as LazyByteString

-- | The instruction type of the operational monad
data Action e a where

  -- | Keep executing until an intermediate result is reached
  Exec    :: Machine e => Action e        (VMResult e)
  
  -- | Short-circuit with a failure
  Fail    :: Machine e => Failure e -> Action e a
  
  -- | Wait for a query to be resolved
  Quiz    :: Machine e => Query e   -> Action e ()

  -- | Embed a VM state transformation
  EVM     :: Machine e => EVM e a      -> Action e a

  -- | Write something to the log or terminal
  Note    :: Machine e => Text      -> Action e ()

-- | Some failure raised by a stepper
data Failure e where
  ContractNotFound :: Failure e
  DecodingError    :: Failure e
  VMFailed         :: Error e -> Failure e

-- | Type alias for an operational monad of @Action@
type Stepper e a = Program (Action e) a

-- Singleton actions

exec :: Machine e => Stepper e (VMResult e)
exec = singleton Exec

fail :: Machine e => Failure e -> Stepper e a
fail = singleton . Fail

quiz :: Machine e => Query e -> Stepper e ()
quiz = singleton . Quiz

evm :: Machine e => EVM e a -> Stepper e a
evm = singleton . EVM

note :: Machine e => Text -> Stepper e ()
note = singleton . Note

-- | Run the VM until final result, resolving all queries
execFully :: Machine e => Stepper e (Either (Error e) (Blob e))
execFully =
  exec >>= \case
    VMFailure (Query q) ->
      quiz q >> execFully
    VMFailure x ->
      pure (Left x)
    VMSuccess x ->
      pure (Right x)

execFullyOrFail :: Machine e => Stepper e (Blob e)
execFullyOrFail = execFully >>= either (fail . VMFailed) pure

-- | Decode a blob as an ABI value, failing if ABI encoding wrong
decode :: AbiType -> Blob Concrete -> Stepper Concrete AbiValue
decode abiType (B bytes) =
  case runGetOrFail (getAbi abiType) (LazyByteString.fromStrict bytes) of
    Right ("", _, x) ->
      pure x
    Right _ ->
      fail DecodingError
    Left _ ->
      fail DecodingError
