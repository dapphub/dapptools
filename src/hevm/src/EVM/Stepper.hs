{-# Language GADTs #-}
{-# Language DataKinds #-}

module EVM.Stepper
  ( Action (..)
  , Stepper
  , exec
  , execFully
  , run
  , runFully
  , wait
  , ask
  , evm
  , evmIO
  , entering
  , enter
  , interpret
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

import Control.Monad.Operational (Program, singleton, view, ProgramViewT(..), ProgramView)
import Control.Monad.State.Strict (runState, liftIO, StateT)
import qualified Control.Monad.State.Class as State
import qualified EVM.Exec
import Data.Text (Text)
import EVM.Types (Expr, EType(..))

import EVM (EVM, VM, VMResult (VMFailure, VMSuccess), Error (Query, Choose), Query, Choose)
import qualified EVM

import qualified EVM.Fetch as Fetch

-- | The instruction type of the operational monad
data Action a where

  -- | Keep executing until an intermediate result is reached
  Exec ::           Action VMResult

  -- | Keep executing until an intermediate state is reached
  Run ::             Action VM

  -- | Wait for a query to be resolved
  Wait :: Query   -> Action ()

  -- | Multiple things can happen
  Ask :: Choose -> Action ()

  -- | Embed a VM state transformation
  EVM  :: EVM a   -> Action a

  -- | Perform an IO action
  IOAct :: StateT VM IO a -> Action a -- they should all just be this?

-- | Type alias for an operational monad of @Action@
type Stepper a = Program Action a

-- Singleton actions

exec :: Stepper VMResult
exec = singleton Exec

run :: Stepper VM
run = singleton Run

wait :: Query -> Stepper ()
wait = singleton . Wait

ask :: Choose -> Stepper ()
ask = singleton . Ask

evm :: EVM a -> Stepper a
evm = singleton . EVM

evmIO :: StateT VM IO a -> Stepper a
evmIO = singleton . IOAct

-- | Run the VM until final result, resolving all queries
execFully :: Stepper (Either Error (Expr Buf))
execFully =
  exec >>= \case
    VMFailure (Query q) ->
      wait q >> execFully
    VMFailure (Choose q) ->
      ask q >> execFully
    VMFailure x ->
      pure (Left x)
    VMSuccess x ->
      pure (Right x)

-- | Run the VM until its final state
runFully :: Stepper EVM.VM
runFully = do
  vm <- run
  case EVM._result vm of
    Nothing -> error "should not occur"
    Just (VMFailure (Query q)) ->
      wait q >> runFully
    Just (VMFailure (Choose q)) ->
      ask q >> runFully
    Just _ ->
      pure vm

entering :: Text -> Stepper a -> Stepper a
entering t stepper = do
  evm (EVM.pushTrace (EVM.EntryTrace t))
  x <- stepper
  evm EVM.popTrace
  pure x

enter :: Text -> Stepper ()
enter t = evm (EVM.pushTrace (EVM.EntryTrace t))

interpret :: Fetch.Fetcher -> Stepper a -> StateT VM IO a
interpret fetcher =
  eval . view

  where
    eval
      :: ProgramView Action a
      -> StateT VM IO a

    eval (Return x) =
      pure x

    eval (action :>>= k) =
      case action of
        Exec ->
          EVM.Exec.exec >>= interpret fetcher . k
        Run ->
          EVM.Exec.run >>= interpret fetcher . k
        Wait q ->
          do m <- liftIO (fetcher q)
             State.state (runState m) >> interpret fetcher (k ())
        Ask _ ->
          error "cannot make choices with this interpreter"
        IOAct m ->
          do m >>= interpret fetcher . k
        EVM m -> do
          r <- State.state (runState m)
          interpret fetcher (k r)
