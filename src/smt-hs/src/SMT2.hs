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

{- | An embedding of the SMT2 typing rules in the haskell type system

     SMT2 scripts are made up of sequences of solver commands. These commands
     can declare new variables and assert statements about these variables.

     Each node in the script AST is assigned a type that represents the
     available typing context. Any attempt to extend the script with a new
     command will produce a type error if any sub term references a variable that
     has not yet been declared.
-}
module SMT2 where

import Prelude hiding (Eq,Word)
import GHC.TypeLits
import Data.Kind
import Data.Function


-- base types --------------------------------------------------------------------------------------


-- | Atomic data types
data Atom = Boolean

-- | Singleton type for Atom
data SAtom (a :: Atom) where
  SBool :: SAtom Boolean

-- | The typechecking environment
type Env = [(Symbol, Atom)]


-- name declaration --------------------------------------------------------------------------------


-- | Extends the typechecking env with (n, a) iff n is not already present in e
type Decl n a e = DeclH n a e e

type DeclH :: Symbol -> Atom -> Env -> Env -> Env
type family DeclH n a e e' where
  DeclH n a '[] e = '(n, a) : e
  DeclH n a ('(n, _) : tl) e = TypeError (Text "'" :<>: Text n :<>: Text "' is already declared")


-- environment lookup ------------------------------------------------------------------------------


-- | A proof that a (n, a) is present in e
data Elem (n :: Symbol) (a :: Atom) (e :: Env) where
  DH :: Elem n a ('(n,a):e)
  DT :: Elem n a e -> Elem n a (t:e)

-- | Compile time type env lookup
type Find :: Symbol -> Atom -> Env -> Elem n a e
type family Find n a e where
  Find n a ('(n,a): e) = DH
  Find n a ('(_,_): e) = DT (Find n a e)
  Find n a '[] = TypeError (Text "'" :<>: Text n :<>: Text "' is undeclared")

-- | Found resolves iff it is passed a valid prood of inclusion in a given typechecking env
class Found (p :: Elem n a e) where
instance Found DH where
instance (Found tl) => Found (DT tl) where


-- SMT2 AST ----------------------------------------------------------------------------------------


-- | The language of top level solver commands
data SMT2 (env :: Env) where
  EmptySMT2 :: SMT2 '[]

  Declare   :: KnownSymbol nm
            => SAtom a
            -> SMT2 e
            -> SMT2 (Decl nm a e)

  Assert    :: Exp e Boolean
            -> SMT2 e
            -> SMT2 e

  CheckSat  :: SMT2 e
            -> SMT2 e


-- | The language of assertable statements
data Exp (e :: Env) (a :: Atom) where
  Lit       :: Bool -> Exp e Boolean
  Var       :: (KnownSymbol n, Found (Find n a e :: Elem n a e)) => Exp e a

  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean
  ITE       :: Exp e Boolean   -> Exp e Boolean -> Exp e Boolean -> Exp e Boolean


-- tests -------------------------------------------------------------------------------------------


-- TODO: compile error when adding an explicit type
--test :: SMT2 e
test
  = EmptySMT2
  & Declare @"hi" SBool
  & Assert (Var @"hi")

  -- produces a type error: 'hi' is already declared
  -- & Declare @"hi" SBool

  -- produces a type error: 'yo' is undeclared
  -- & Assert (Var @"yo")

  & CheckSat
