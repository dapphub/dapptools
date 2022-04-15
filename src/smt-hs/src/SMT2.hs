{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
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
import Data.Typeable

import Data.Parameterized.Context
import Control.Monad.State


-- base types --------------------------------------------------------------------------------------


-- | Atomic data types
data Atom = Boolean

-- | Define the haskell datatype used to declare literals of a given atomic type
type LitType :: Atom -> Type
type family LitType a where
  LitType Boolean = Bool

-- | Singleton type for Atom
data SAtom (a :: Atom) where
  SBool :: SAtom Boolean

deriving instance Show (SAtom a)

-- | The typechecking environment
data Env
  = Env [(Symbol, Atom)]     -- The statically declared names
        (Ctx Atom)           -- The dynamically declared names

type Dyn :: Env -> Ctx Atom
type family Dyn env where
  Dyn ('Env _ dyn) = dyn


-- name declaration --------------------------------------------------------------------------------


-- | Extends the static part of the typechecking env with (name, typ) iff name is not already present in env
type Decl name typ env = DeclH name typ env env

type DeclH :: Symbol -> Atom -> Env -> Env -> Env
type family DeclH name typ env orig where
  DeclH name typ ('Env '[] _) ('Env st dy) = 'Env ('(name, typ) : st) dy
  DeclH name _ ('Env ('(name, _) : _) _) _ = TypeError (Text "duplicate name declaration")

-- | Extends the dynamic part of the typechecking env with typ
type DDecl :: Atom -> Env -> Env
type family DDecl typ env where
  DeclH typ ('Env st dy) = 'Env st (dy ::> typ)


-- environment lookup ------------------------------------------------------------------------------


-- | A proof that (name, typ) is present in the static part of a given environment
data Elem :: Symbol -> Atom -> Env -> Type where
  DH :: Elem name typ ('Env ('(name, typ) : tl) dyn)
  DT :: Elem name typ ('Env st dy) -> Elem name typ ('Env (hd : st) dy)

-- | Compile time type env lookup
type Find :: Symbol -> Atom -> Env -> Elem n t e'
type family Find name typ env where
  Find name typ ('Env ('(name,typ) : _) _) = DH
  Find name typ ('Env ('(_,_): tl) dyn) = DT (Find name typ ('Env tl dyn))
  Find name typ ('Env '[] _) = TypeError (Text "undeclared name")

-- | Found resolves iff it is passed a valid prood of inclusion in a given typechecking env
class Found (proof :: Elem name typ env) where
instance Found DH where
instance (Found tl) => Found (DT tl) where

-- | Type alias for adding an inclusion constraint against a given typing env
type Has name typ env = Found (Find name typ env :: Elem name typ env)


-- SMT2 AST ----------------------------------------------------------------------------------------


-- | The language of top level solver commands
data SMT2 (e :: Env) where
  EmptySMT2 :: SMT2 ('Env '[] EmptyCtx)

  SDeclare  :: KnownSymbol n
            => SAtom t
            -> SMT2 e
            -> SMT2 (Decl n t e)

  DDeclare  :: String
            -> SAtom t
            -> SMT2 e
            -> SMT2 (DDecl t e)

  Assert    :: Exp e Boolean
            -> SMT2 e
            -> SMT2 e

  CheckSat  :: SMT2 e
            -> SMT2 e

deriving instance Show (SMT2 e)

-- | The language of assertable statements
data Exp (e :: Env) (t :: Atom) where

  -- polymorphic
  Lit       :: LitType t -> Exp e t
  VarS      :: (KnownSymbol n, Has n t e) => Exp e t
  VarD      :: (KnownDiff e' (Dyn e)) => Index e' t -> Exp e t
  ITE       :: Exp e Boolean   -> Exp e t -> Exp e t -> Exp e t

  -- boolean
  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean

instance Show (Exp e t) where
  show (Lit a) = undefined


-- runtime variable declaration --------------------------------------------------------------------


data Entry :: Atom -> Type where
  E :: forall typ. String -> SAtom typ -> Entry typ

-- declares a name in an environment and returns an index
declare :: String -> SAtom a -> Assignment Entry e -> SMT2 e' -> (Assignment Entry (e ::> a), SMT2 (DDecl a e'), Index (e ::> a) a)
declare name tp env prev = (env', next, idx)
  where
    env' = extend env (E name tp)
    next = DDeclare name tp prev
    idx = lastIndex (size env')

testDyn :: String -> SMT2 _
testDyn name = next'
             & Assert (VarD idx)
             & CheckSat
  where
    base = EmptySMT2
         & SDeclare @"hi" SBool
    (env', next, idx) = declare name SBool empty base
    (env'', next', idx') = declare "ooo" SBool env' next


-- tests -------------------------------------------------------------------------------------------


--testDyn :: SMT2 _
--testDyn = EmptySMT2
--declare :: SMT2 e -> SMT2 e
--testDyn :: Dyn '[] '[] (SMT2 '[] -> SMT2 _)
--testDyn = do
  --p <- declare "hi" SBool
  --(_, smt) <- get
  --pure smt


-- TODO: writing out the full typechecking env here is very annoying.
-- PartialTypeSignatures with a wildcard isn't too bad, but having to disable a compiler warning is lame
-- Why doesn't the following work?
-- test :: SMT2 e
test :: SMT2 _
test
  = EmptySMT2
  & SDeclare @"hi" SBool
  & Assert (VarS @"hi")

  -- produces a type error: "hi" is already declared
  -- & SDeclare @"hi" SBool

  -- produces a type error: "yo" is undeclared
  -- & Assert (Var @"yo")

  & CheckSat

incompleteDecl :: SMT2 e -> SMT2 _
incompleteDecl prev = prev
                    & SDeclare @"hi" SBool

-- asserting the typechecking env for fragments works
incomplete :: (Has "hi" Boolean e) => SMT2 e -> SMT2 e
incomplete prev = prev
                & Assert (And [VarS @"hi", Lit False])
                & CheckSat
