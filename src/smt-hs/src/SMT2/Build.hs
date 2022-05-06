{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMT2.Build where

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
-- TODO: make this a newtype
newtype SMT2 ret = SMT2 (State Script ret)
  deriving (Functor, Applicative, Monad, MonadState Script)

-- | Wrapper type for name references.
--
-- A Ref can be constructed at runtime using the `declare` smart constructor, or at compile time using the `asRef` smart constructor.
data Ref (a :: Ty) where
  Ref :: String -> STy a -> Ref a

deriving instance (Show (Ref t))

-- | Construct a Ref from a statically known string
asRef :: forall nm a . KnownSymbol nm => STy a -> Ref a
asRef = Ref (symbolVal (Proxy @nm))

-- | Declare a new name at runtime, returns a Ref
--
-- N.B. Does not perform any freshness checks. You are responsible for ensuring
-- that names declared via the rutime interface are distinct
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

getValue :: List Exp ts -> SMT2 ()
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
