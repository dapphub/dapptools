{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

module SMT2 where

import Prelude hiding (Eq,Word)
import GHC.TypeLits
import Data.Kind
import Data.Function

-- TODO: rm these deps
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr


-- base types --------------------------------------------------------------------------------------


data Atom = Boolean

data SAtom (a :: Atom) where
  SBool :: SAtom Boolean


-- type checking environment -----------------------------------------------------------------------


type Env = [(Symbol, Atom)]
type Empty = '[]

-- | A runtime representation of the typechecking environment
data Dict :: Env -> Type where
   Nil  :: Dict '[]
   (:>) :: Entry s o -> Dict tl -> Dict ('(s,o) : tl)

infixr 5 :>

-- | An entry in the dictionary
data Entry :: Symbol -> Atom -> Type where
   E :: forall s a. SAtom a -> Entry s a


-- environment lookup ------------------------------------------------------------------------------


-- A proof that a particular (name, type) pair has been declared in the environment
data Elem (n :: Symbol) (a :: Atom) (e :: Env) where
  DH :: Elem n a ('(n,a):e)
  DT :: Elem n a e -> Elem n a (t:s)

type Find :: Symbol -> Atom -> Env -> Elem n a e
type family Find n a e where
  Find n a ('(n,a): s) = DH
  Find n a ('(t,p): s) = DT (Find n a s)
  Find n a '[] = TypeError (Text "variable '" :<>: Text n :<>: Text "' not found in typechecking env")

-- TODO: haaalllpppp
class Found p where
instance (Found (Elem n a e)) where


-- sequenced solver commands -----------------------------------------------------------------------


data SMT2 (env :: Env) where
  Declare   :: symbolRepr nm
            -> SAtom a
            -> SMT2 e
            -> SMT2 ('(nm, a) : e)

  Assert    :: Exp e Boolean
            -> SMT2 e
            -> SMT2 e

  CheckSat  :: SMT2 e
            -> SMT2 e

  EmptySMT2 :: SMT2 '[]


-- smt expressions ---------------------------------------------------------------------------------


data Exp (e :: Env) (k :: Atom) where
  -- basic types
  Lit   :: Bool -> Exp e Boolean
  Var   :: Found (Find nm a e) => symbolRepr nm -> Exp e a

  -- boolean ops
  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean
  ITE       :: Exp e Boolean   -> Exp e Boolean -> Exp e Boolean -> Exp e Boolean


-- tests -------------------------------------------------------------------------------------------


--test :: SMT2 e
test
  = EmptySMT2
  & Declare (knownSymbol @"hi") SBool
  & Assert (Var (knownSymbol @"hi"))
  & CheckSat
