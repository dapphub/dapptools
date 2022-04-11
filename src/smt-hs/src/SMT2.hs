{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}

import Prelude hiding (Eq,Word)
import Data.ByteString (ByteString)
import GHC.TypeLits
import Data.Vector.Sized (Vector, fromList)
import Data.Parameterized.List
import Data.Parameterized.Context
import Data.Parameterized.Classes
import Data.Parameterized.NatRepr
import Data.Parameterized.SymbolRepr
import Data.Word
import Data.Function
import Data.Foldable (foldl')
import Data.BitVector.Sized (BV, mkBV)


-- base types --------------------------------------------------------------------------------------


data Atom
  = Boolean
  | SmtInt
  | BitVec Nat

data Tp
  = A Atom
  | F (Ctx Atom) Atom


-- singleton types ---------------------------------------------------------------------------------


-- atomic types
data SAtom (a :: Atom) where
  SBool :: SAtom Boolean
  SInt :: SAtom SmtInt
  SBV :: NatRepr n -> SAtom (BitVec n)

deriving instance (Show (SAtom a))
deriving instance (ShowF SAtom)

-- uninterpreted functions
data UF (args :: Ctx Atom) (ret :: Atom) where
  UF :: Assignment SAtom args -> SAtom ret -> UF args ret

deriving instance (Show (UF args ret))

-- all types
data STp (tp :: Tp) where
  SA :: SAtom a -> STp (A a)
  SF :: UF args ret -> STp (F args ret)

-- variable names
data Name (nm :: Symbol) where
  Name :: symbolRepr nm -> Name nm

data NamedBV (uid :: (Symbol, Nat)) where
  NamedBV :: symbolRepr nm -> natRepr sz -> NamedBV '(nm, sz)

data NamedFun (uid :: (Symbol, Ctx Atom, Atom)) where
  NamedFun :: symbolRepr nm -> Assignment SAtom args -> SAtom ret -> NamedFun '(nm, args, ret)


-- type checking environment -----------------------------------------------------------------------


data Env
    (bools :: Ctx Symbol)
    (ints  :: Ctx Symbol)
    (bvs   :: Ctx (Symbol, Nat))
    (fns   :: Ctx (Symbol, Ctx Atom, Atom))
  where
    Env :: Assignment Name bools
        -> Assignment Name ints
        -> Assignment NamedBV bvs
        -> Assignment NamedFun fns
        -> Env bools ints bvs fns

-- | Declares a new name in the given environment
type family Decl
              (tp :: Tp)
              (s :: Symbol)
              (env :: Env bools ints bvs fns)
              :: Env a b c d
            where
  --Decl (A Boolean)    s ('Env bools ints bvs fns) = 'Env (bools ::> s) ints bvs fns
  --Decl (A SmtInt)     s ('Env bools ints bvs fns) = 'Env bools (ints ::> s) bvs fns
  --Decl (A (BitVec n)) s ('Env bools ints bvs fns) = 'Env bools ints (bvs ::> '(s, n)) fns
  --Decl (F args ret)   s ('Env bools ints bvs fns) = 'Env bools ints bvs (fns ::> '(s, args, ret))


-- environment lookup ------------------------------------------------------------------------------


-- TODO: how to actually implement these...
class Fresh (nm :: Symbol) (e :: Env bools ints bvs fns) where
class Contains (tp :: Tp) (nm :: Symbol) (e :: Env bools ints bvs fns) where


-- sequenced solver commands -----------------------------------------------------------------------


data Problem (env :: Env ints bools bvs fns) where
  Declare  :: Fresh nm e
           => STp tp
           -> symbolRepr nm
           -> Problem e
           -> Problem (Decl tp nm e)

  Assert   :: Exp e Boolean
           -> Problem e
           -> Problem e

  CheckSat :: Problem e
           -> Problem e

  -- TODO: how to force an empty env here?
  EmptyProb:: Problem ('Env a b c d)


-- smt expressions ---------------------------------------------------------------------------------


data Exp (e :: Env ints bools bvs fns) (k :: Atom) where
  -- basic types
  LitBool   :: Bool -> Exp e Boolean
  LitBV     :: BV n -> Exp e (BitVec n)
  LitInt    :: Integer -> Exp e SmtInt
  SymBool   :: Contains (A Boolean) nm e => symbolRepr nm -> Exp e Boolean
  SymBV     :: Contains (A (BitVec sz)) nm e => symbolRepr nm -> Exp e Boolean
  SymInt    :: Contains (A SmtInt)  nm e => symbolRepr nm -> Exp e Boolean

  -- boolean ops
  And       :: [Exp e Boolean] -> Exp e Boolean
  Or        :: [Exp e Boolean] -> Exp e Boolean
  Eq        :: [Exp e Boolean] -> Exp e Boolean
  Xor       :: [Exp e Boolean] -> Exp e Boolean
  Impl      :: [Exp e Boolean] -> Exp e Boolean
  Distinct  :: [Exp e Boolean] -> Exp e Boolean
  ITE       :: Exp e Boolean   -> Exp e Boolean -> Exp e Boolean -> Exp e Boolean

  -- bitvector ops
  Shr       :: Exp e (BitVec n) -> Exp e (BitVec n) -> Exp e (BitVec n)
  Shl       :: Exp e (BitVec n) -> Exp e (BitVec n) -> Exp e (BitVec n)

  -- function application
  App       :: Contains (F args ret) nm e => symbolRepr nm -> Assignment (Exp e) args -> Exp e ret


-- tests -------------------------------------------------------------------------------------------

mkProblem :: Problem e
mkProblem
  = EmptyProb
  & Declare (SA SBool) (knownSymbol @"hi")
  & Assert (SymBool (knownSymbol @"hi"))
  & CheckSat

mkFuncProb :: Problem e
mkFuncProb
  = EmptyProb
  & Declare (SF (UF (Empty :> SInt :> SBool :> SBV (knownNat @256)) SBool)) (knownSymbol @"hi")
  & Assert (App (knownSymbol @"hi") (Empty :> LitInt 10 :> LitBool False :> LitBV (mkBV (knownNat @256) 100)))
  EmptyProb

