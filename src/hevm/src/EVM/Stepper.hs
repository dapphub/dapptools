{-# Language GADTs #-}

module EVM.Stepper
  ( Action (..)
  , Failure (..)
  , Stepper
  , exec
  , execFully
  , execFullyOrFail
  , decode
  , fail
  , wait
  , evm
  , note
  , entering
  , enter
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
import Data.SBV

import EVM (EVM, VMResult (VMFailure, VMSuccess), Error (Query), Query)
import qualified EVM

import EVM.ABI (AbiType, AbiValue, getAbi)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString

-- | The instruction type of the operational monad
data Action a where

  -- | Keep executing until an intermediate result is reached
  Exec ::            Action VMResult

  -- | Short-circuit with a failure
  Fail :: Failure -> Action a

  -- | Wait for a query to be resolved
  Wait :: Query   -> Action ()

  -- | Embed a VM state transformation
  EVM  :: EVM a   -> Action a

  -- | Write something to the log or terminal
  Note :: Text    -> Action ()

-- | Some failure raised by a stepper
data Failure
  = ContractNotFound
  | DecodingError
  | VMFailed Error
  deriving Show

-- | Type alias for an operational monad of @Action@
type Stepper a = Program Action a

-- Singleton actions

exec :: Stepper VMResult
exec = singleton Exec

fail :: Failure -> Stepper a
fail = singleton . Fail

wait :: Query -> Stepper ()
wait = singleton . Wait

evm :: EVM a -> Stepper a
evm = singleton . EVM

note :: Text -> Stepper ()
note = singleton . Note

-- | Run the VM until final result, resolving all queries
execFully :: Stepper (Either Error [SWord 8])
execFully =
  exec >>= \case
    VMFailure (Query q) ->
      wait q >> execFully
    VMFailure x ->
      pure (Left x)
    VMSuccess x ->
      pure (Right x)

execFullyOrFail :: Stepper [SWord 8]
execFullyOrFail = execFully >>= either (fail . VMFailed) pure

-- | Decode a blob as an ABI value, failing if ABI encoding wrong
decode :: AbiType -> ByteString -> Stepper AbiValue
decode abiType bytes =
  case runGetOrFail (getAbi abiType) (LazyByteString.fromStrict bytes) of
    Right ("", _, x) ->
      pure x
    Right _ ->
      fail DecodingError
    Left _ ->
      fail DecodingError

entering :: Text -> Stepper a -> Stepper a
entering t stepper = do
  evm (EVM.pushTrace (EVM.EntryTrace t))
  x <- stepper
  evm EVM.popTrace
  pure x

enter :: Text -> Stepper ()
enter t = evm (EVM.pushTrace (EVM.EntryTrace t))
