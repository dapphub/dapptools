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
import IxState


-- base types --------------------------------------------------------------------------------------


-- | Atomic data types
data Atom = Boolean

-- | The typechecking environment
data Env
  = Env
      (Ctx (Symbol, Atom)) -- ^ The statically declared names
      (Ctx Atom)           -- ^ The dynamically declared names


-- SMT2 AST ----------------------------------------------------------------------------------------


-- | The language of top level solver commands
data SMT2 (e :: Env) where
  EmptySMT2 :: SMT2 ('Env EmptyCtx EmptyCtx)

  SDeclare  :: KnownSymbol n
            => SAtom t
            -> SMT2 ('Env st dy)
            -> SMT2 ('Env (st ::> '(n,t)) dy)

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
  ITE       :: Exp e Boolean -> Exp e t -> Exp e t -> Exp e t

  -- boolean
  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean


-- static name declaration -------------------------------------------------------------------------


-- | Extends the static part of the typechecking env with (name, typ) iff name is not already present in env
type Decl name typ st = DeclH name typ st st

type DeclH :: Symbol -> Atom -> Ctx (Symbol, Atom) -> Ctx (Symbol, Atom) -> Ctx (Symbol, Atom)
type family DeclH name typ env orig where
  DeclH name typ 'EmptyCtx st = st ::> '(name, typ)
  DeclH name _ (_ ::> '(name, _)) _ = TypeError (Text "duplicate name declaration")

-- | Compile time type env lookup
type Find :: Symbol -> Atom -> Ctx (Symbol, Atom) -> Nat
type family Find name typ env where
  Find name typ (_ ::> '(name, typ)) = 0
  Find name typ (pre ::> _) = 1 + (Find name typ pre)
  Find name typ 'EmptyCtx = TypeError (Text "undeclared name")

-- | Type alias for convenient inclusion checking
type Has n a e = ValidIx (Find n a (St e)) (St e)


-- overloaded monadic ops for QualifiedDo ----------------------------------------------------------


return :: a -> IxState si si a
return = ireturn

(>>=) :: IxState p q a -> (a -> IxState q r b) -> IxState p r b
(>>=) = ibind

(>>) :: IxState p q a -> IxState q r b -> IxState p r b
v >> w = v SMT2.>>= const w


-- monadic interface -------------------------------------------------------------------------------


-- | The type of entries in the runtime type environment
data Entry :: Atom -> Type where
  E :: forall typ. String -> SAtom typ -> Entry typ

-- | Wrapper type for the indexed state monad we use
type Writer pre post ret = IxState
  (Assignment Entry (Dy pre), SMT2 pre)    -- ^ prestate
  (Assignment Entry (Dy post), SMT2 post)  -- ^ poststate
  ret                                      -- ^ return type

-- | Declare a new name at runtime
--
-- N.B. Does not perform any freshness checks. You are responsible for ensuring
-- that names declared via the rutime interface are distinct
declare :: String -> SAtom a -> Writer ('Env st dy) ('Env st (dy ::> a)) (Index (dy ::> a) a)
declare name typ = SMT2.do
  (env, exp) <- get
  let env'  = extend env (E name typ)
      exp'  = DDeclare name typ exp
      proof = lastIndex (size env')
  put (env', exp')
  SMT2.return proof

-- | Assert some boolean variable
assert :: KnownDiff old (Dy e) => Index old Boolean -> Writer e e ()
assert proof = SMT2.do
  (env, exp) <- get
  let next = Assert (VarD proof) exp
  put (env, next)


-- utils -------------------------------------------------------------------------------------------


-- | Singleton type for Atom
data SAtom (a :: Atom) where
  SBool :: SAtom Boolean

-- | Define the haskell datatype used to declare literals of a given atomic type
type LitType :: Atom -> Type
type family LitType a where
  LitType Boolean = Bool

-- | Returns the static part of a given Env
type St :: Env -> Ctx (Symbol, Atom)
type family St env where
  St ('Env st _) = st

-- | Returns the dynamic part of a given Env
type Dy :: Env -> Ctx Atom
type family Dy env where
  Dy ('Env _ dy) = dy


-- tests -------------------------------------------------------------------------------------------


-- | Extend the SMT2 expression with some static fragment
include :: (SMT2 ('Env stin dy) -> SMT2 ('Env stout dy)) -> Writer ('Env stin dy) ('Env stout dy) ()
include fragment = SMT2.do
  (env, exp) <- get
  put (env, fragment exp)

testDyn :: String -> String -> Writer _ _ ()
testDyn n1 n2 = SMT2.do
  p <- declare n1 SBool
  p' <- declare n2 SBool
  assert p
  assert p'
  include declHi
  -- TODO: include a fragment that depends on a static name
  -- I'm gonna need a way to convince the typechecker that:
  --   a <=? a + b is always true
  -- How can I do proofs with typelits
  -- Do I need a proper induction numeric encoding here?
  include assertHi
  include CheckSat




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

declHi :: SMT2 _ -> SMT2 _
declHi = SDeclare @"hi" SBool

-- asserting the typechecking env for fragments works
assertHi :: _ => SMT2 e -> SMT2 e
assertHi = Assert (And [VarS @"hi", Lit False])

-- composition of two incomplete fragments
composed :: SMT2 _
composed
  = EmptySMT2
  & declHi
  & assertHi

-- TODO: why is the constraint needed here?
static :: _ => SMT2 _ -> SMT2 _
static prev = prev
  & SDeclare @"hi" SBool
  & Assert (And [VarS @"hi", Lit False])
  & CheckSat
