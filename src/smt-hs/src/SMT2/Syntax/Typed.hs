{-# LANGUAGE GADTs #-}
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

{- | Defines the core AST datatypes as well as the monadic interface for programatic SMT2 generation -}
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


import Prelude hiding (Eq,Word)
import Data.Kind
import Data.Function
import Data.Typeable
import GHC.TypeLits
import GHC.Natural

import Data.Parameterized.List
import Data.Parameterized.Classes
import Control.Monad.State
import Data.Map (Map)


-- AST types --------------------------------------------------------------------------------------


-- | Runtime bitvector representation
data BV :: Nat -> Type where
  BV :: KnownNat n => Natural -> BV n

deriving instance (Show (BV n))

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

-- | The language of top level solver commands
data Command where
  CheckSat            :: Command
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
  CheckSatAssuming    :: Exp Boolean -> Command
  Echo                :: String      -> Command
  GetInfo             :: InfoFlag    -> Command
  GetOption           :: String      -> Command
  GetValue            :: List Exp ts -> Command
  Pop                 :: Natural     -> Command
  Push                :: Natural     -> Command
  SetInfo             :: String      -> Command
  SetLogic            :: String      -> Command
  SetOption           :: Option      -> Command
  Declare             :: String -> STy t -> Command

deriving instance (Show Command)

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
  deriving (Show)

data InfoFlag
  = AllStatistics
  | AssertionStackLevels
  | Authors
  | ErrorBehaviour
  | Name
  | ReasonUnknown
  | Version
  deriving (Show)

-- | The language of assertable statements
data Exp (t :: Ty) where

  -- literals & names
  Lit       :: Show t => t -> Exp (ExpType t)
  Var       :: String -> Exp t

  -- functions
  App       :: Exp (Fun args ret) -> List Exp args -> Exp ret

  -- core ops
  -- http://smtlib.cs.uiowa.edu/theories-Core.shtml
  And       :: [Exp Boolean] -> Exp Boolean
  Or        :: [Exp Boolean] -> Exp Boolean
  Eq        :: [Exp t] -> Exp Boolean
  Xor       :: [Exp Boolean] -> Exp Boolean
  Impl      :: [Exp Boolean] -> Exp Boolean
  Distinct  :: [Exp Boolean] -> Exp Boolean
  ITE       :: Exp Boolean -> Exp t -> Exp t -> Exp t

  -- bitvector
  -- http://smtlib.cs.uiowa.edu/theories-FixedSizeBitVectors.shtml
  Concat    :: Exp (BitVec i) -> Exp (BitVec j) -> Exp (BitVec (i + j))
  Extract   :: (0 <= j, j <= i, i <= (m - 1)) => SNat i -> SNat j -> Exp (BitVec m) -> Exp (BitVec (i - j + 1))

  BVNot     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m)
  BVNeg     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m)
  BVAnd     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVOr      :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVAdd     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVMul     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVUDiv    :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVURem    :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVShl     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVLShr    :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp (BitVec m)
  BVULt     :: (1 <= m) => Exp (BitVec m) -> Exp (BitVec m) -> Exp Boolean

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
  Divisible :: (1 <= n) => SNat n -> Exp 'Integer -> Exp Boolean

  -- arrays
  -- http://smtlib.cs.uiowa.edu/theories-ArraysEx.shtml
  Select    :: Exp (Arr k v) -> Exp k -> Exp v
  Store     :: Exp (Arr k v) -> Exp k -> Exp v -> Exp (Arr k v)

deriving instance (ShowF Exp)
deriving instance (Show (Exp t))


-- utils -------------------------------------------------------------------------------------------


-- | Singleton type for Nat
data SNat (n :: Nat) where
  SZ :: SNat 0
  SS :: SNat n -> SNat (1 + n)

deriving instance (Show (SNat n))

-- | Singleton type for Ty
data STy (a :: Ty) where
  SBool :: STy Boolean
  SBitVec :: SNat n -> STy (BitVec n)
  SInt :: STy 'Integer
  SFun :: List STy args -> STy ret -> STy (Fun args ret)
  SArr :: STy k -> STy v -> STy (Arr k v)
deriving instance (Show (STy ty))
deriving instance (ShowF STy)

-- | Define the Ty that should be used for a given haskell datatype
type ExpType :: Type -> Ty
type family ExpType a where
  ExpType Bool = Boolean
  ExpType (BV n) = (BitVec n)
  ExpType Integer = 'Integer
