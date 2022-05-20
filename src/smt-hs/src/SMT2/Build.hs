{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : SMT2.Build
Description : Monadic interface for runtime SMT2 generation
-}
module SMT2.Build (
  SMT2(..),
  Ref,
  declare,
  include,
  assert,
  checkSat,
  getModel,
  reset,
  resetAssertions,
  getProof,
  getUnsatAssumptions,
  getUnsatCore,
  exit,
  getAssertions,
  getAssignment,
  checkSatAssuming,
  echo,
  getInfo,
  getOption,
  getValue,
  pop,
  push,
  setInfo,
  setLogic,
  setOption
)where

import Data.Kind
import Data.Function
import Data.Typeable
import GHC.TypeLits
import GHC.Natural

import Data.Parameterized.List
import Data.Parameterized.Classes
import Control.Monad.State
import Data.Map (Map)

import SMT2.Syntax.Typed


-- | Wrapper type for the indexed state monad we use
newtype SMT2 ret = SMT2 (State Script ret)
  deriving (Functor, Applicative, Monad, MonadState Script)

-- | Wrapper type for name references.
--
-- A Ref can be constructed at runtime using the `declare` smart constructor, or at compile time using the `asRef` smart constructor.
data Ref (a :: Ty) where
  Ref :: String -> STy a -> Ref a

deriving instance (Show (Ref t))

-- | Declare a new name at runtime, returns a Ref
declare :: String -> STy a -> SMT2 (Ref a)
declare name typ = do
  Script exp <- get
  put $ Script (Declare name typ : exp)
  return $ Ref name typ

-- | Extend the SMT2 expression with some static fragment
include :: Script -> SMT2 ()
include (Script fragment) = do
  Script exp <- get
  put $ Script (fragment <> exp)

-- | Extend the SMT2 expression with a single command
include' :: Command -> SMT2 ()
include' cmd = include (Script [cmd])

-- | Assert some boolean expression
assert :: Exp Boolean -> SMT2 ()
assert e = do
  Script exp <- get
  put $ Script (Assert e : exp)

checkSat :: SMT2 ()
checkSat = include' CheckSat

getModel :: SMT2 ()
getModel = include' GetModel

reset :: SMT2 ()
reset = include' Reset

resetAssertions :: SMT2 ()
resetAssertions = include' ResetAssertions

getProof :: SMT2 ()
getProof = include' GetProof

getUnsatAssumptions :: SMT2 ()
getUnsatAssumptions = include' GetUnsatAssumptions

getUnsatCore :: SMT2 ()
getUnsatCore = include' GetUnsatCore

exit :: SMT2 ()
exit = include' Exit

getAssertions :: SMT2 ()
getAssertions = include' GetAssertions

getAssignment :: SMT2 ()
getAssignment = include' GetAssignment

checkSatAssuming :: Exp Boolean -> SMT2 ()
checkSatAssuming = include' . CheckSatAssuming

echo :: String -> SMT2 ()
echo = include' . Echo

getInfo :: InfoFlag -> SMT2 ()
getInfo = include' . GetInfo

getOption :: String -> SMT2 ()
getOption = include' . GetOption

getValue :: String -> SMT2 ()
getValue = include' . GetValue

pop :: Natural -> SMT2 ()
pop = include' . Pop

push :: Natural -> SMT2 ()
push = include' . Push

setInfo :: String -> SMT2 ()
setInfo = include' . SetInfo

setLogic :: String -> SMT2 ()
setLogic = include' . SetLogic

setOption :: Option -> SMT2 ()
setOption = include' . SetOption
