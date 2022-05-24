{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-|
Module      : SMT2.Syntax.Typed
Description : Defines typed versions of the core SMT2 datatypes
-}
module SMT2.Syntax.Typed (
  BV(..),
  Ty(..),
  Script(..),
  Command(..),
  Option(..),
  InfoFlag(..),
  Exp(..),
  STy(..),
  SNat(..),
) where


import Prelude hiding (Eq,LT,GT,Word)
import Data.Kind
import Data.Function
import Data.Typeable
import Data.Data
import GHC.TypeLits
import GHC.Natural
import GHC.Generics
import Data.Char
import Data.Map (Map)
import Data.List (intercalate)

import Data.Parameterized.List
import Data.Parameterized.Classes
import Language.Haskell.TH.Syntax (Q(..), Lift(..))
import Language.Haskell.TH (Name(..), appE, conE)
import Control.Monad.State
import qualified Language.Haskell.TH.Syntax as TH
import qualified Prelude as P


-- AST types --------------------------------------------------------------------------------------


-- | Runtime bitvector representation
data BV :: Nat -> Type where
  BV :: KnownNat n => Natural -> BV n

deriving instance (Show (BV n))
deriving instance (Lift (BV n))
deriving instance (P.Eq (BV n))

-- | Data types
data Ty
  = Boolean
  | BitVec Nat
  | Integer
  | Arr Ty Ty
  | Fun [Ty] Ty
  deriving (Typeable)


-- | Sequenced solver commands
newtype Script = Script [Command]
  deriving newtype (Semigroup, Monoid)
  deriving (Lift, P.Eq)


-- | The language of top level solver commands
data Command where
  GetModel            :: Command
  Reset               :: Command
  ResetAssertions     :: Command
  GetProof            :: Command
  GetUnsatAssumptions :: Command
  GetUnsatCore        :: Command
  Exit                :: Command
  GetAssertions       :: Command
  GetAssignment       :: Command
  Assert              :: Exp Boolean -> Command
  Echo                :: String      -> Command
  GetInfo             :: InfoFlag    -> Command
  GetOption           :: String      -> Command
  GetValue            :: String      -> Command
  Pop                 :: Natural     -> Command
  Push                :: Natural     -> Command
  SetInfo             :: String      -> Command
  SetLogic            :: String      -> Command
  SetOption           :: Option      -> Command
  Declare             :: Typeable t => String -> STy t -> Command

deriving instance (Lift Command)
instance P.Eq Command where
  GetModel == GetModel = True
  Reset == Reset = True
  ResetAssertions == ResetAssertions = True
  GetProof == GetProof = True
  GetUnsatAssumptions == GetUnsatAssumptions = True
  GetUnsatCore == GetUnsatCore = True
  Exit == Exit = True
  GetAssertions == GetAssertions = True
  GetAssignment == GetAssignment = True
  Assert a == Assert b = a == b
  (Echo a) == (Echo b) = a == b
  (GetInfo a) == (GetInfo b) = a == b
  (GetOption a) == (GetOption b) = a == b
  (GetValue a) == (GetValue b) = a == b
  (Pop a) == (Pop b) = a == b
  (Push a) == (Push b) = a == b
  (SetInfo a) == (SetInfo b) = a == b
  (SetLogic a) == (SetLogic b) = a == b
  (SetOption a) == (SetOption b) = a == b
  (Declare a (b :: STy t1)) == (Declare c (d :: STy t2))
    = a == c && case eqT @t1 @t2 of
       Just Refl -> True
       Nothing -> False

data Option
  = DiagnosticOutputChannel String
  | GlobalDeclarations Bool
  | InteractiveMode Bool
  | PrintSuccess Bool
  | ProduceAssertions Bool
  | ProduceAssignments Bool
  | ProduceModels Bool
  | ProduceProofs Bool
  | ProduceUnsatAssumptions Bool
  | ProduceUnsatCores Bool
  | RandomSeed Integer
  | RegularOutputChannel String
  | ReproducibleResourceLimit Integer
  | Verbosity Integer
  deriving (Lift, P.Eq)

data InfoFlag
  = AllStatistics
  | AssertionStackLevels
  | Authors
  | ErrorBehaviour
  | Name
  | ReasonUnknown
  | Version
  deriving (Show, Lift, P.Eq)

-- | The language of assertable statements
data Exp (t :: Ty) where

  -- literals & names
  LitBool   :: Bool -> Exp Boolean
  LitInt    :: Integer -> Exp 'Integer
  LitBV     :: BV n -> Exp (BitVec n)
  Var       :: String -> Exp t

  -- functions
  App       :: (Typeable args, Typeable ret) => Exp (Fun args ret) -> List Exp args -> Exp ret

  -- core ops
  -- http://smtlib.cs.uiowa.edu/theories-Core.shtml
  And       :: [Exp Boolean] -> Exp Boolean
  Or        :: [Exp Boolean] -> Exp Boolean
  Eq        :: Typeable t => [Exp t] -> Exp Boolean
  Xor       :: [Exp Boolean] -> Exp Boolean
  Impl      :: [Exp Boolean] -> Exp Boolean
  Distinct  :: [Exp Boolean] -> Exp Boolean
  ITE       :: Exp Boolean -> Exp t -> Exp t -> Exp t

  -- bitvector
  -- http://smtlib.cs.uiowa.edu/theories-FixedSizeBitVectors.shtml
  Concat    :: (KnownNat i, KnownNat j) => Exp (BitVec i) -> Exp (BitVec j) -> Exp (BitVec (i + j))
  Extract   :: (KnownNat i, KnownNat j, KnownNat m, 0 <= j, j <= i, i <= (m - 1))
            => SNat i -> SNat j -> Exp (BitVec m) -> Exp (BitVec (i - j + 1))

  BVNot     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m)
  BVNeg     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m)
  BVAnd     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVOr      :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVAdd     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVMul     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVUDiv    :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVURem    :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVShl     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVShr     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVULt     :: (KnownNat m, 1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp Boolean

  -- integer
  -- http://smtlib.cs.uiowa.edu/theories-Ints.shtml
  Neg       :: Exp 'Integer -> Exp 'Integer
  Sub       :: Exp 'Integer -> Exp 'Integer -> Exp 'Integer
  Add       :: Exp 'Integer -> Exp 'Integer -> Exp 'Integer
  Mul       :: Exp 'Integer -> Exp 'Integer -> Exp 'Integer
  Div       :: Exp 'Integer -> Exp 'Integer -> Exp 'Integer
  Mod       :: Exp 'Integer -> Exp 'Integer -> Exp 'Integer
  Abs       :: Exp 'Integer -> Exp 'Integer
  LEQ       :: Exp 'Integer -> Exp 'Integer -> Exp Boolean
  LT        :: Exp 'Integer -> Exp 'Integer -> Exp Boolean
  GEQ       :: Exp 'Integer -> Exp 'Integer -> Exp Boolean
  GT        :: Exp 'Integer -> Exp 'Integer -> Exp Boolean
  Divisible :: (KnownNat n, 1 <= n) => SNat n -> Exp 'Integer -> Exp Boolean

  -- arrays
  -- http://smtlib.cs.uiowa.edu/theories-ArraysEx.shtml
  Select    :: (Typeable k, Typeable v) => Exp (Arr k v) -> Exp k -> Exp v
  Store     :: Exp (Arr k v) -> Exp k -> Exp v -> Exp (Arr k v)

deriving instance (ShowF Exp)
deriving instance (Typeable (Exp t))

instance P.Eq (Exp t) where
  (LitBool a) == (LitBool b) = a == b
  (LitInt a) == (LitInt b) = a == b
  (LitBV a) == (LitBV b) = a == b
  (Var a) == (Var b) = a == b

  (App (a :: Exp (Fun args1 ret1)) (b :: List Exp args2)) == (App (c :: Exp (Fun args3 ret2)) (d :: List Exp args4))
    = case eqT @args1 @args3 of
        Nothing -> False
        Just Refl -> case eqT @ret1 @ret2 of
          Nothing -> False
          Just Refl -> case eqT @args2 @args4 of
             Nothing -> False
             Just Refl -> True

  (And a) == (And b) = a == b
  (Or a) == (Or b) = a == b
  (Xor a) == (Xor b) = a == b
  (Impl a) == (Impl b) = a == b
  (Distinct a) == (Distinct b) = a == b
  (ITE a b c) == (ITE d e f) = a == d && b == e && c == f
  (Eq (a :: [Exp t1])) == (Eq (b :: [Exp t2]))
    = case eqT @t1 @t2 of
        Just Refl -> a == b
        Nothing -> False

  (Concat (a :: Exp (BitVec i)) (b :: Exp (BitVec j))) == (Concat (c :: Exp (BitVec k)) (d :: Exp (BitVec l)))
    = case eqT @i @k of
        Nothing -> False
        Just Refl -> case eqT @j @l of
          Nothing -> False
          Just Refl -> a == c && b == d
  (Extract (a :: SNat i) (b :: SNat j) (c :: Exp (BitVec n))) == (Extract (d :: SNat k) (e :: SNat l) (f :: Exp (BitVec m)))
    = case eqT @i @k of
        Nothing -> False
        Just Refl -> case eqT @j @l of
          Nothing -> False
          Just Refl -> case eqT @n @m of
            Nothing -> False
            Just Refl -> a == d && b == e && c == f

  (BVNot a) == (BVNot b) = a == b
  (BVNeg a) == (BVNeg b) = a == b
  (BVAnd a b) == (BVAnd c d) = a == c && b == d
  (BVOr a b) == (BVOr c d) = a == c && b == d
  (BVAdd a b) == (BVAdd c d) = a == c && b == d
  (BVMul a b) == (BVMul c d) = a == c && b == d
  (BVUDiv a b) == (BVUDiv c d) = a == c && b == d
  (BVURem a b) == (BVURem c d) = a == c && b == d
  (BVShl a b) == (BVShl c d) = a == c && b == d
  (BVShr a b) == (BVShr c d) = a == c && b == d
  (BVULt (a :: Exp (BitVec i)) (b :: Exp (BitVec j))) == (BVULt (c :: Exp (BitVec k)) (d :: Exp (BitVec l)))
    = case eqT @i @k of
        Nothing -> False
        Just Refl -> case eqT @j @l of
          Nothing -> False
          Just Refl -> a == c && b == d

  (Neg a) == (Neg b) = a == b
  (Sub a b) == (Sub c d) = a == c && b == d
  (Add a b) == (Add c d) = a == c && b == d
  (Mul a b) == (Mul c d) = a == c && b == d
  (Div a b) == (Div c d) = a == c && b == d
  (Mod a b) == (Mod c d) = a == c && b == d
  (Abs a) == (Abs b) = a == b
  (LEQ a b) == (LEQ c d) = a == c && b == d
  (LT a b) == (LT c d) = a == c && b == d
  (GEQ a b) == (GEQ c d) = a == c && b == d
  (GT a b) == (GT c d) = a == c && b == d
  (Divisible (a :: SNat i) b) == (Divisible (c :: SNat j) d)
    = case eqT @i @j of
        Nothing -> False
        Just Refl -> a == c && b == d

  (Select (a :: Exp (Arr k1 v1)) (b :: Exp k2)) == (Select (c :: Exp (Arr k3 v2)) (d :: Exp k4))
    = case eqT @k1 @k3 of
        Nothing -> False
        Just Refl -> case eqT @k2 @k4 of
          Nothing -> False
          Just Refl -> case eqT @v1 @v2 of
            Nothing -> False
            Just Refl -> a == c && b == d
  (Store a b c) == (Store d e f) = a == d && b == e && c == f
  _ == _ = False

instance Lift (Exp a) where
  liftTyped (LitBool a) = [|| LitBool a ||]
  liftTyped (LitInt a) = [|| LitInt a ||]
  liftTyped (LitBV a) = [|| LitBV a ||]
  liftTyped (Var a) = [|| Var a ||]
  liftTyped (Or a) = [|| Or a ||]
  liftTyped (And a) = [|| And a ||]
  liftTyped other = error $ "TODO: impl lift for: " <> show other


-- translation into concrete syntax ----------------------------------------------------------------


instance Show Script where
  show (Script cmds) = unlines $ fmap show cmds

instance Show Command where
  show (Declare name tp) = "(declare-const " <> name <> " " <> show tp <> ")"
  show (Assert e) = "(assert " <> show e <> ")"
  show (SetOption o) = "(set-option " <> show o <> ")"
  show GetModel = "(get-model)"
  show Reset = "(reset)"
  show ResetAssertions = "(reset-assertions)"
  show GetProof = "(get-proof)"
  show GetUnsatAssumptions = "(get-unsat-assumptions)"
  show GetUnsatCore = "(get-unsat-core)"
  show Exit = "(exit)"
  show GetAssertions = "(get-assertions)"
  show GetAssignment = "(get-assignment)"
  show (Echo s) = "(echo" <> s <> ")"
  show (GetInfo f) = "(get-info" <> show f <> ")"
  show (GetOption s) = "(get-option" <> s <> ")"
  show (GetValue s) = "(get-value" <> s <> ")"
  show (Pop n) = "(pop " <> show n <> ")"
  show (Push n) = "(push " <> show n <> ")"
  show (SetInfo s) = "(set-info " <> s <> ")"
  show (SetLogic s) = "(set-logic " <> s <> ")"

instance Show Option where
  show (DiagnosticOutputChannel s) = ":diagnostic-output-channel " <> s
  show (GlobalDeclarations b) = ":global-declarations " <> (lowercase $ show b)
  show (InteractiveMode b) = ":interactive-mode " <> (lowercase $ show b)
  show (PrintSuccess b) = ":print-success " <> (lowercase $ show b)
  show (ProduceAssertions b) = ":produce-assertions " <> (lowercase $ show b)
  show (ProduceAssignments b) = ":produce-assignments " <> (lowercase $ show b)
  show (ProduceModels b) = ":produce-models " <> (lowercase $ show b)
  show (ProduceProofs b) = ":produce-proofs " <> (lowercase $ show b)
  show (ProduceUnsatAssumptions b) = ":produce-unsat-assumptions " <> (lowercase $ show b)
  show (ProduceUnsatCores b) = ":produce-unsat-cores " <> (lowercase $ show b)
  show (RandomSeed i) = ":random-seed " <> show i
  show (RegularOutputChannel s) = ":regular-output-channel " <> s
  show (ReproducibleResourceLimit i) = ":reproducible-resource-limit " <> show i
  show (Verbosity i) = ":verbosity " <> show i

instance Show (Exp a) where
  -- TODO: handle lit bv?
  -- TODO: handle lit arr?
  -- TODO: handle lit fn?
  -- vars & lits
  show (LitBool a) = lowercase $ show a
  show (LitInt a) = show a
  show (LitBV a) = show a
  show (Var a) = a

  -- core
  show (And a) = "(and " <> intercalate " " (fmap show a) <> ")"
  show (Or a) = "(or " <> intercalate " " (fmap show a) <> ")"
  show (Eq a) = "(eq " <> intercalate " " (fmap show a) <> ")"
  show (Xor a) = "(xor " <> intercalate " " (fmap show a) <> ")"
  show (Impl a) = "(=> " <> intercalate " " (fmap show a) <> ")"
  show (Distinct a) = "(distinct " <> intercalate " " (fmap show a) <> ")"
  show (ITE cond l r) = "(ite " <> show cond <> " " <> show l <> " " <> show r <> ")"

  -- euf
  show (App fn args) = "(" <> show fn <> " " <> show args <> ")"

  -- bv
  show (Concat l r) = "(concat " <> show l <> " " <> show r <> ")"
  -- TODO: is this correct?
  show (Extract i j bv) = "(extract " <> show i <> " " <> show j <> " " <> show bv <> ")"
  show (BVNot bv) = "(bvnot " <> show bv <> ")"
  show (BVNeg bv) = "(bvneg " <> show bv <> ")"
  show (BVAnd l r) = "(bvand " <> show l <> " " <> show r <> ")"
  show (BVOr l r) = "(bvor " <> show l <> " " <> show r <> ")"
  show (BVAdd l r) = "(bvadd " <> show l <> " " <> show r <> ")"
  show (BVMul l r) = "(bvmul " <> show l <> " " <> show r <> ")"
  show (BVUDiv l r) = "(bvudiv " <> show l <> " " <> show r <> ")"
  show (BVURem l r) = "(bvurem " <> show l <> " " <> show r <> ")"
  show (BVShl l r) = "(bvshl " <> show l <> " " <> show r <> ")"
  show (BVShr l r) = "(bvshr " <> show l <> " " <> show r <> ")"
  show (BVULt l r) = "(bvult " <> show l <> " " <> show r <> ")"

  -- integer
  show (Neg i) = "(- " <> show i <> ")"
  show (Sub i j) = "(- " <> show i <> " " <> show j <> ")"
  show (Add i j) = "(+ " <> show i <> " " <> show j <> ")"
  show (Mul i j) = "(* " <> show i <> " " <> show j <> ")"
  show (Div i j) = "(div " <> show i <> " " <> show j <> ")"
  show (Mod i j) = "(mod " <> show i <> " " <> show j <> ")"
  show (Abs i) = "(abs " <> show i <> ")"
  show (LEQ i j) = "(<= " <> show i <> " " <> show j <> ")"
  show (LT i j) = "(< " <> show i <> " " <> show j <> ")"
  show (GEQ i j) = "(>= " <> show i <> " " <> show j <> ")"
  show (GT i j) = "(> " <> show i <> " " <> show j <> ")"
  show (Divisible n e) = "(divisble " <> show n <> " " <> show e <> ")"

  -- arrays
  show (Select arr k) = "(select " <> show arr <> " " <> show k <> ")"
  show (Store arr k v) = "(select " <> show arr <> " " <> show k <> " " <> show v <> ")"

instance Show (STy ty) where
  show SInt = "Int"
  show SBool = "Bool"
  show (SBitVec n) = "(_ BitVec " <> show n <> ")"

instance Show (SNat n) where
  show SZ = show 0
  show (SS _ :: SNat n) = show (natVal (Proxy @n))


-- utils -------------------------------------------------------------------------------------------


-- | Singleton type for Nat
data SNat (n :: Nat) where
  SZ :: SNat 0
  SS :: (KnownNat (1 + n)) => SNat n -> SNat (1 + n)

instance KnownNat n => P.Eq (SNat n) where
  (l :: SNat i) == (r :: SNat j)
    = case eqT @i @j of
        Just Refl -> True
        Nothing -> False

instance Lift (SNat n) where
  liftTyped SZ = [|| SZ ||]
  liftTyped (SS n) = [|| SS n ||]

-- | Singleton type for Ty
data STy (a :: Ty) where
  SBool :: STy Boolean
  SBitVec :: SNat n -> STy (BitVec n)
  SInt :: STy 'Integer
  SFun :: List STy args -> STy ret -> STy (Fun args ret)
  SArr :: STy k -> STy v -> STy (Arr k v)

instance Typeable ty => P.Eq (STy ty) where
  (a :: STy t1) == (b :: STy t2)
    = case eqT @t1 @t2 of
        Just Refl -> True
        Nothing -> False

instance Lift (STy ty) where
  liftTyped SInt = [|| SInt ||]
  liftTyped SBool = [|| SBool ||]
  liftTyped (SBitVec n) = [|| SBitVec n ||]
  -- TODO: SFun, SArr

lowercase :: String -> String
lowercase = fmap toLower
