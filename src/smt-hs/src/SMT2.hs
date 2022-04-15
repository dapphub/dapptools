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
--
-- A Ctx is a type level snoc list from galois with a nice inclusion proof system.
data Env
  = Env
      [(Symbol, Atom)]     -- ^ The statically declared names
      (Ctx Atom)           -- ^ The dynamically declared names

type Dyn :: Env -> Ctx Atom
type family Dyn env where
  Dyn ('Env _ dyn) = dyn


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
  VarD      :: (KnownDiff e' (Dyn e)) => Index e' t -> Exp e t
  ITE       :: Exp e Boolean   -> Exp e t -> Exp e t -> Exp e t

  -- boolean
  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean


-- runtime variable declaration --------------------------------------------------------------------


-- | The type of entries in the runtime type environment
data Entry :: Atom -> Type where
  E :: forall typ. String -> SAtom typ -> Entry typ

-- | References to runtime delcared variables
data SymVar where
  SymVar :: forall (e :: Ctx Atom) (a :: Atom) . SAtom a -> Index e a -> SymVar

-- | State used when dynamically constructing smt
data SMTState where
  SMTState :: forall (st :: [ (Symbol, Atom)]) (dy :: Ctx Atom)
            . Assignment Entry dy
           -> SMT2 ('Env st dy)
           -> SMTState

-- | State monad wrapper over an SMTState
type Writer = State (SMTState)

declare :: String -> SAtom a -> Writer SymVar
declare name typ = do
  s <- get
  case s of
    SMTState env exp -> do
      let env' = extend env (E name typ)
          proof = lastIndex (size env')
          next = DDeclare name typ exp
      put $ SMTState env' next
      pure $ SymVar typ proof

assert :: SymVar -> Writer ()
assert v = do
  s <- get
  case s of
    SMTState env exp -> case v of
      SymVar SBool proof -> do
        {-
          [typecheck -Wdeferred-type-errors] [E] • Could not deduce (KnownDiff e dy) arising from a use of ‘VarD’
            from the context: a ~ 'Boolean
              bound by a pattern with constructor: SBool :: SAtom 'Boolean,
                       in a case alternative
              at /dapptools/src/smt-hs/src/SMT2.hs:183:14-18
          • In the first argument of ‘Assert’, namely ‘(VarD proof)’
            In the expression: Assert (VarD proof) exp
            In an equation for ‘next’: next = Assert (VarD proof) exp
          -}
        let next = Assert (VarD proof) exp
        put $ SMTState env next

checkSat :: Writer ()
checkSat = do
  s <- get
  case s of
    SMTState env exp -> do
      put $ SMTState env (CheckSat exp)


-- tests -------------------------------------------------------------------------------------------


-- Interface for dynamic name declaration
testDyn :: String -> Writer ()
testDyn s = do
  v <- declare "hi" SBool
  v' <- declare "ho" SBool
  assert v
  assert v'
  checkSat


{-
 - TODO: type signatures here are pretty awkward:
 -
 - test :: SMT2 e
 -
 - This doesn't work becuase `SMT2 e` means "I can return an SMT2 over any Type", but
 - actually `test` returns a specific type and the typechecker knows about it.
 - The thing that's awkward is that this type depends on the terms inside it, and
 - includes the full typechecking environment for the final expression, so what
 - GHC actually wants us to write is:
 -
 - test :: SMT2 ('Env '[ '("hi", 'Boolean)] 'EmptyCtx)
 -
 - If we enable PartialTypeSignatures, then you can use a wildcard for the
 - dependent type, which is probablly actually not such a bad workflow, but it
 - also means having to force library users to disable a compiler warning, which
 - is kinda gross.
 -
 - test :: SMT2 _
 -
 - So far the best I've come up with is to hide everything behind an
 - existential type, I dunno if this is going to result in some crazy pattern
 - matching down the line though...
 -}
data Static where
  Static :: forall (e :: Env) . SMT2 e -> Static

test :: Static
test
  = Static $
    EmptySMT2
  & SDeclare @"hi" SBool
  & Assert (VarS @"hi")

  -- TODO: this is broken now? :(
  -- produces a type error: "hi" is already declared
  -- & SDeclare @"hi" SBool

  -- produces a type error: "yo" is undeclared
  -- & Assert (VarS @"yo")

  & CheckSat

incompleteDecl :: Static -> Static
incompleteDecl (Static prev) = Static $ prev & SDeclare @"hi" SBool

-- asserting the typechecking env for fragments works
incomplete :: (Has "hi" Boolean e) => SMT2 e -> SMT2 e
incomplete prev = prev
                & Assert (And [VarS @"hi", Lit False])
                & CheckSat
