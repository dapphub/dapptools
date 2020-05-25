{-# Language GADTs #-}
{-# Language DataKinds #-}

module EVM.Stepper
  ( Action (..)
  , Failure (..)
  , Stepper
  , exec
  , execFully
  , execFullyOrFail
  , runFully
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
import Control.Lens (view)
import Data.Binary.Get (runGetOrFail)
import Data.Text (Text)
import Data.SBV hiding (options)

import EVM (EVM, VM, VMResult (VMFailure, VMSuccess), Error (Query, Choose), Query, Choose)
import qualified EVM

import EVM.ABI (AbiType, AbiValue, getAbi)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString

-- | The instruction type of the operational monad
data Action a where

  -- | Keep executing until an intermediate result is reached
  Exec ::           Action VMResult

  -- | Keep executing until the final state is reached
  Run ::            Action VM

  -- | Short-circuit with a failure
  Fail :: Failure -> Action a

  -- | Wait for a query to be resolved
  Wait :: Query   -> Action ()

  -- | Multiple things can happen
  Option :: Choose -> Action ()

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

run :: Stepper VM
run = singleton Run

fail :: Failure -> Stepper a
fail = singleton . Fail

wait :: Query -> Stepper ()
wait = singleton . Wait

option :: Choose -> Stepper ()
option = singleton . Option

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
    VMFailure (Choose q) ->
      option q >> execFully
    VMFailure x ->
      pure (Left x)
    VMSuccess x ->
      pure (Right x)

-- | Run the VM until its final state
runFully :: Stepper EVM.VM
runFully = do
  vm <- run
  case view EVM.result vm of
    Nothing -> error "should not occur"
    Just (VMFailure (Query q)) ->
      wait q >> runFully
    Just (VMFailure (Choose q)) ->
      option q >> runFully
    Just _ -> 
      pure vm

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
