{-# Language GADTs #-}
{-# Language NamedFieldPuns #-}

module EVM.Stepper where

import Prelude hiding (fail)

import Control.Lens
import Control.Monad (void)
import Control.Monad.Operational (Program, singleton)
import Data.Binary.Get    (runGetOrFail)
import Data.Text (Text, isPrefixOf)

import EVM          (EVM, VMResult (VMFailure, VMSuccess), Error (Query), Query)
import EVM          (state, contract, env, contracts, balance)
import EVM          (replaceCodeOfSelf)

import EVM.ABI      (AbiType (AbiBoolType), AbiValue (AbiBool), getAbi)
import EVM.Machine  (Machine, Blob, w256)
import EVM.Concrete (Concrete, Blob (B))
import EVM.UnitTest (UnitTestOptions (..), setupCall)

import qualified Data.ByteString.Lazy as LazyByteString

-- This module is an abstract definition of EVM steppers.
-- Steppers can be run as TTY debuggers or as CLI test runners.
--
-- The implementation uses the operational monad pattern
-- as the framework for monadic interpretation.
--
-- Note: this is a sketch of a work in progress!

data Action e a where
  Exec    :: Machine e => Action e        (VMResult e)
  Step    :: Machine e => Action e (Maybe (VMResult e))
  Back    :: Machine e => Action e ()

  Fail    :: Machine e => Failure e -> Action e a
  Note    :: Machine e => Text      -> Action e ()
  Quiz    :: Machine e => Query e   -> Action e ()

  EVM     :: Machine e => EVM e a      -> Action e a
  Try     :: Machine e => Stepper e () -> Action e (VMResult e)

data Failure e where
  ContractNotFound :: Failure e
  DecodingError :: Failure e
  VMFailed :: Error e -> Failure e

type Stepper e a = Program (Action e) a

type ABIMethod = Text

step :: Machine e => Stepper e (Maybe (VMResult e))
step = singleton Step

exec :: Machine e => Stepper e (VMResult e)
exec = singleton Exec

fail :: Machine e => Failure e -> Stepper e a
fail = singleton . Fail

note :: Machine e => Text -> Stepper e ()
note = singleton . Note

quiz :: Machine e => Query e -> Stepper e ()
quiz = singleton . Quiz

evm :: Machine e => EVM e a -> Stepper e a
evm = singleton . EVM

execFully :: Machine e => Stepper e (Blob e)
execFully =
  exec >>= \case
    VMFailure (Query q) ->
      quiz q >> execFully
    VMFailure x ->
      fail (VMFailed x)
    VMSuccess x ->
      pure x

try :: Stepper Concrete () -> Stepper Concrete (VMResult Concrete)
try = singleton . Try

decode :: AbiType -> Blob Concrete -> Stepper Concrete AbiValue
decode abiType (B bytes) =
  case runGetOrFail (getAbi abiType) (LazyByteString.fromStrict bytes) of
    Right ("", _, x) ->
      pure x
    Right _ ->
      fail DecodingError
    Left _ ->
      fail DecodingError

initializeUnitTest :: UnitTestOptions -> Stepper Concrete ()
initializeUnitTest UnitTestOptions { ..} = do
  -- Constructor is loaded; run until it returns code
  B code <- execFully
  addr <- evm (use (state . contract))

  -- Mutate the current contract to use the new code
  evm $ replaceCodeOfSelf code

  -- Give a balance to the test target
  evm $ env . contracts . ix addr . balance += w256 balanceForCreated

  -- Initialize the test contract
  evm $ setupCall addr "setUp()" gasForInvoking

  -- Let `setUp()' run to completion
  void execFully

runUnitTest :: UnitTestOptions -> ABIMethod -> Stepper Concrete Bool
runUnitTest UnitTestOptions { .. } method = do

  -- Decide whether the test is supposed to fail or succeed
  let shouldFail = "testFail" `isPrefixOf` method

  -- The test subject should be loaded and initialized already
  addr <- evm $ use (state . contract)

  -- Set up the call to the test method
  evm $ setupCall addr method gasForInvoking

  -- Try running the test method
  bailed <- try (void execFully) >>= \case
    VMFailure _ -> pure True
    VMSuccess _ -> pure False

  -- Ask whether any assertions failed
  evm $ setupCall addr "failed()" 10000
  AbiBool failed <- execFully >>= decode AbiBoolType

  -- Return true if the test was successful
  pure (shouldFail == bailed || failed)
