{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

{- | An embedding of the SMT2 typing rules in the haskell type system

     SMT2 scripts are made up of sequences of solver commands. These commands
     can declare new variables and assert statements about these variables.

     Each node in the script AST is assigned a type that represents the
     available typing context. Any attempt to extend the script with a new
     command will produce a type error if any sub term references a variable that
     has not yet been declared.
-}
module SMT3 where

import Prelude hiding (Eq,Word)
import Data.Kind
import Data.Function
import Data.Typeable
import Control.Monad.State
import Data.Map (Map)
import GHC.TypeLits
import GHC.Natural

import Data.Parameterized.List


-- AST types --------------------------------------------------------------------------------------


-- | Runtime bitvector representation
data BV :: Nat -> Type where
  BV :: KnownNat n => Natural -> BV n

-- | Data types
data Ty
  = Boolean
  | BitVec Nat
  | Integer
  | Arr Ty Ty
  | Fun [Ty] Ty

-- | Sequenced solver commands
newtype SMT2 = SMT2 [Command]
  deriving (Semigroup, Monoid)

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
  | RandomSeed Bool
  | RegularOutputChannel Bool
  | ReproducibleResourceLimit Bool
  | Verbosity Bool

data InfoFlag
  = AllStatistics
  | AssertionStackLevels
  | Authors
  | ErrorBehaviour
  | Name
  | ReasonUnknown
  | Version

-- | The language of assertable statements
data Exp (t :: Ty) where

  -- literals & names
  Lit       :: t -> Exp (ExpType t)
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
  Extract   :: ( 0 <= j, j <= i, i <= (m - 1))
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


-- monadic interface -------------------------------------------------------------------------------


-- | Wrapper type for the indexed state monad we use
type Writer ret = State SMT2 ret

data Ref (a :: Ty) where
  Ref :: String -> STy a -> Ref a

-- | Declare a new name at runtime
--
-- N.B. Does not perform any freshness checks. You are responsible for ensuring
-- that names declared via the rutime interface are distinct
declare :: String -> STy a -> Writer (Ref a)
declare name typ = do
  SMT2 exp <- get
  put $ SMT2 (Declare name typ : exp)
  return $ Ref name typ

-- | Assert some boolean variable
assert :: Ref Boolean -> Writer ()
assert (Ref name SBool) = do
  SMT2 exp <- get
  put $ SMT2 (Assert (Var name) : exp)


-- utils -------------------------------------------------------------------------------------------


-- | Singleton type for Nat
data SNat (n :: Nat) where
  SZ :: SNat 0
  SS :: SNat n -> SNat (1 + n)

-- | Singleton type for Ty
data STy (a :: Ty) where
  SBool :: STy Boolean
  SBitVec :: SNat n -> STy (BitVec n)
  SInt :: STy 'Integer
  SFun :: List STy args -> STy ret -> STy (Fun args ret)
  SArr :: STy k -> STy v -> STy (Arr k v)

-- | Define the Ty that should be used for a given haskell datatype
type ExpType :: Type -> Ty
type family ExpType a where
  ExpType Bool = Boolean
  ExpType (BV n) = (BitVec n)
  ExpType Integer = 'Integer


-- tests -------------------------------------------------------------------------------------------


-- | Extend the SMT2 expression with some static fragment
include :: SMT2 -> Writer ()
include (SMT2 fragment) = do
  SMT2 exp <- get
  put $ SMT2 (fragment <> exp)

testDyn :: String -> String -> Writer ()
testDyn n1 n2 = do
  p <- declare n1 SBool
  p' <- declare n2 SBool
  assert p
  assert p'
  include declHi
  include assertHi
  include $ SMT2 [CheckSat]

test :: SMT2
test = SMT2
  [ Declare "hi" SBool
  , Assert (Eq [Add (Lit (1 :: Integer)) (Lit (2 :: Integer)), Lit (3 :: Integer)])
  , Assert (Eq [Lit True, BVULt (Lit (BV @256 100)) (Lit (BV @256 1000))])
  , CheckSat
  ]

declHi :: SMT2
declHi = SMT2 [Declare "hi" SBool]

-- asserting the typechecking env for fragments works
assertHi :: SMT2
assertHi = SMT2 [Assert (And [Var "hi", Lit False])]

-- composition of two incomplete fragments
composed :: SMT2
composed = declHi <> assertHi

