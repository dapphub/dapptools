{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
{-# OPTIONS -Wno-partial-type-signatures #-}

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

-- | The typechecking environment
--
-- A Ctx is a type level snoc list from galois with a nice inclusion proof system.
data Env
  = Env
      [(Symbol, Atom)]     -- ^ The statically declared names
      (Ctx Atom)           -- ^ The dynamically declared names


-- | Returns the dynamic part of a given Env
type Dy :: Env -> Ctx Atom
type family Dy env where
  Dy ('Env _ dy) = dy

-- | Returns the static part of a given Env
type St :: Env -> [(Symbol, Atom)]
type family St env where
  St ('Env st _) = st


-- static name declaration -------------------------------------------------------------------------


-- | Extends the static part of the typechecking env with (name, typ) iff name is not already present in env
type Decl name typ env = DeclH name typ env env

type DeclH :: Symbol -> Atom -> Env -> Env -> Env
type family DeclH name typ env orig where
  DeclH name typ ('Env '[] _) ('Env st dy) = 'Env ('(name, typ) : st) dy
  DeclH name _ ('Env ('(name, _) : _) _) _ = TypeError (Text "duplicate name declaration")


-- static environment lookup -----------------------------------------------------------------------


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
            -> SMT2 ('Env st dy)
            -> SMT2 ('Env st (dy ::> t))

  Assert    :: Exp e Boolean
            -> SMT2 e
            -> SMT2 e

  CheckSat  :: SMT2 e
            -> SMT2 e


-- | The language of assertable statements
data Exp (e :: Env) (t :: Atom) where

  -- polymorphic
  Lit       :: LitType t -> Exp e t
  VarS      :: (KnownSymbol n, Has n t e) => Exp e t
  VarD      :: (KnownDiff e' (Dy e)) => Index e' t -> Exp e t
  ITE       :: Exp e Boolean   -> Exp e t -> Exp e t -> Exp e t

  -- boolean
  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean


-- monadic interface --------------------------------------------------------------------

class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b

newtype IxStateT m si so v = IxStateT { runIxStateT:: si -> m (so,v) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT (\si -> Prelude.return (si,x))
  ibind (IxStateT m) f = IxStateT (\si -> m si Prelude.>>= (\ (sm,x) -> runIxStateT (f x) sm))

vsget :: Monad m => IxStateT m si si si
vsget = IxStateT (\si -> Prelude.return (si,si))

vsput :: Monad m => so -> IxStateT m si so ()
vsput x = IxStateT (\si -> Prelude.return (x,()))

return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) :: (Monad m) => IxStateT m p q a -> (a -> IxStateT m q r b) -> IxStateT m p r b
(>>=) = ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = v SMT2.>>= const w

-- | The type of entries in the runtime type environment
data Entry :: Atom -> Type where
  E :: forall typ. String -> SAtom typ -> Entry typ

-- | Wrapper type for the indexed state monad we use
type Writer stin dyin stout dyout ret = forall m . (Monad m) => IxStateT m
  (Assignment Entry dyin, SMT2 ('Env stin dyin))    -- ^ prestate
  (Assignment Entry dyout, SMT2 ('Env stout dyout)) -- ^ poststate
  ret                                               -- ^ return


declare :: String -> SAtom a -> Writer st dy st (dy ::> a) (Index (dy ::> a) a)
declare name typ = SMT2.do
  (env, exp) <- vsget
  let env'  = extend env (E name typ)
      exp'  = DDeclare name typ exp
      proof = lastIndex (size env')
  vsput (env', exp')
  SMT2.return proof

assert :: KnownDiff old dy => Index old Boolean -> Writer st dy st dy ()
assert proof = SMT2.do
  (env, exp) <- vsget
  let next = Assert (VarD proof) exp
  vsput (env, next)

include :: (SMT2 ('Env stin dy) -> SMT2 ('Env stout dy)) -> Writer stin dy stout dy ()
include fragment = SMT2.do
  (env, exp) <- vsget
  vsput (env, fragment exp)


-- tests -------------------------------------------------------------------------------------------


testDyn :: String -> String -> Writer _ _ _ _ _
testDyn n1 n2 = SMT2.do
  p <- declare n1 SBool
  p' <- declare n2 SBool
  assert p
  assert p'
  -- TODO: including static fragments that declare names
  --include incompleteDecl
  include CheckSat
  (env, exp) <- vsget
  SMT2.return exp


test :: SMT2 _
test
  = EmptySMT2
  & SDeclare @"hi" SBool
  & Assert (VarS @"hi")

  -- produces a type error: "hi" is already declared
  -- & SDeclare @"hi" SBool

  -- produces a type error: "yo" is undeclared
  -- & Assert (VarS @"yo")

  & CheckSat

incompleteDecl :: SMT2 e -> SMT2 _
incompleteDecl = SDeclare @"hi" SBool

-- asserting the typechecking env for fragments works
incomplete :: _ => SMT2 e -> SMT2 e
incomplete =
    Assert (And [VarS @"hi", Lit False])
  . CheckSat

-- TODO: why is the constraint needed here?
static :: _ => SMT2 e -> SMT2 _
static =
    SDeclare @"hi" SBool
  . Assert (And [VarS @"hi", Lit False])
  . CheckSat

-- composition of two incomplete fragments

composed :: SMT2 _
composed
  = EmptySMT2
  & incompleteDecl
  & incomplete
