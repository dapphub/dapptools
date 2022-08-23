{-# Language CPP #-}
{-# Language TemplateHaskell #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module EVM.Types where

import Prelude hiding  (Word, LT, GT)

import Data.Aeson
import Crypto.Hash hiding (SHA256)
import Data.Map (Map)
import Data.Bifunctor (first)
import Data.Char
import Data.List (isPrefixOf)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 as BS16
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Char8  as Char8
import Data.Word (Word8, Word32)
import Data.Bits (Bits, FiniteBits, shiftR, shift, shiftL, (.&.), (.|.))
import Data.DoubleWord
import Data.DoubleWord.TH
import Data.Maybe (fromMaybe)
import Numeric (readHex, showHex)
import Options.Generic
import Control.Arrow ((>>>))

import qualified Data.ByteArray       as BA
import qualified Data.Aeson           as JSON
import qualified Data.Aeson.Types     as JSON
import qualified Data.ByteString      as BS
import qualified Data.Serialize.Get   as Cereal
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text
import qualified Data.Sequence        as Seq
import qualified Text.Regex.TDFA      as Regex
import qualified Text.Read

-- Some stuff for "generic programming", needed to create Word512
import Data.Data

-- We need a 512-bit word for doing ADDMOD and MULMOD with full precision.
mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]


--data Buffer
--  = ConcreteBuffer ByteString
--  | SymbolicBuffer [SWord 8]

newtype W256 = W256 Word256
  deriving
    ( Num, Integral, Real, Ord, Generic
    , Bits , FiniteBits, Enum, Eq , Bounded
    )

--data Word = C (Expr EWord) W256 --maybe to remove completely in the future

--instance Show Word where
  --show (C _ x) = show x

--instance Read Word where
  --readsPrec n s =
    --case readsPrec n s of
      --[(x, r)] -> [(C (Lit x) x, r)]
      --_ -> []

--w256 :: W256 -> Word
--w256 w = C (Lit w) w

--instance Bits Word where
  --(C a x) .&. (C b y) = C (And a b) (x .&. y)
  --(C a x) .|. (C b y) = C (Or  a b) (x .|. y)
  --(C a x) `xor` (C b y) = C (Xor a b) (x `xor` y)
  --complement (C a x) = C (Not a) (complement x)
  --shiftL (C a x) i = C (SHL a (Lit $ fromIntegral i)) (shiftL x i)
  --shiftR (C a x) i = C (SHR a (Lit $ fromIntegral i)) (shiftR x i)
  --rotate (C a x) i = C (Todo "rotate " a) (rotate x i) -- unused.
  --bitSize (C _ x) = bitSize x
  --bitSizeMaybe (C _ x) = bitSizeMaybe x
  --isSigned (C _ x) = isSigned x
  --testBit (C _ x) i = testBit x i
  --bit i = w256 (bit i)
  --popCount (C _ x) = popCount x

--instance FiniteBits Word where
  --finiteBitSize (C _ x) = finiteBitSize x
  --countLeadingZeros (C _ x) = countLeadingZeros x
  --countTrailingZeros (C _ x) = countTrailingZeros x

--instance Bounded Word where
  --minBound = w256 minBound
  --maxBound = w256 maxBound

--instance Eq Word where
  --(C _ x) == (C _ y) = x == y

--instance Enum Word where
  --toEnum i = w256 (toEnum i)
  --fromEnum (C _ x) = fromEnum x

--instance Integral Word where
  --quotRem (C _ x) (C _ y) =
    --let (a, b) = quotRem x y
    --in (w256 a, w256 b)
  --toInteger (C _ x) = toInteger x

--instance Num Word where
  --(C a x) + (C b y) = C (Add a b) (x + y)
  --(C a x) * (C b y) = C (Mul a b) (x * y)
  --abs (C a x) = C (Todo "abs" a) (abs x)
  --signum (C a x) = C (Todo "signum" a) (signum x)
  --fromInteger x = C (Lit (fromInteger x)) (fromInteger x)
  --negate (C a x) = C (Sub (Lit 0) a) (negate x)

--instance Real Word where
  --toRational (C _ x) = toRational x

--instance Ord Word where
  --compare (C _ x) (C _ y) = compare x y

{- |
  Expr implements an abstract respresentation of an EVM program

  This type can give insight into the provenance of a term which is useful,
  both for the aesthetic purpose of printing terms in a richer way, but also to
  allow optimizations on the AST instead of letting the SMT solver do all the
  heavy lifting.

  Memory, calldata, and returndata are all represented as a Buf. Semantically
  speaking a Buf is a byte array with of size 2^256.

  Bufs have three base constructors:
    - EmptyBuf:       all elements are zero
    - AbstractBuf:    all elements are fully abstract values
    - ConcreteBuf bs: all elements past (length bs) are zero

  Bufs can be read from with:
    - ReadByte idx buf: read the byte at idx from buf
    - ReadWord idx buf: read the byte at idx from buf

  Bufs can be written to with:
    - WriteByte idx val buf: write val to idx in buf
    - WriteWord idx val buf: write val to idx in buf
    - CopySlice srcOffset dstOffset size src dst:
        overwrite dstOffset -> dstOffset + size in dst with srcOffset -> srcOffset + size from src

  Note that the shared usage of `Buf` does allow for the construction of some
  badly typed Expr instances (e.g. an MSTORE on top of the contents of calldata
  instead of some previous instance of memory), we accept this for the
  sake of simplifying pattern matches against a Buf expression.

  Storage expressions are similar, but instead of writing regions of bytes, we
  write a word to a particular key in a given addresses storage. Note that as
  with a Buf, writes can be sequenced on top of concrete, empty and fully
  abstract starting states.

  Logs are also represented as a sequence of writes, but unlike Buf and Storage
  expressions, Log writes are always sequenced on an empty starting point, and
  overwriting is not allowed.

  One important principle is that of local context: e.g. each term representing
  a write to a Buf / Storage / Logs will always contain a copy of the state
  that is being added to, this ensures that all context relevant to a given
  operation is contained within the term that represents that operation.

  When dealing with Expr instances we assume that concrete expressions have
  been reduced to their smallest possible representation (i.e. a `Lit`,
  `ConcreteBuf`, or `ConcreteStore`). Failure to adhere to this invariant will
  result in your concrete term being treated as symbolic, and may produce
  unexpected errors. In the future we may wish to consider encoding the
  concreteness of a given term directly in the type of that term, since such
  type level shenanigans tends to complicate implementation, we skip this for
  now.

  Knowledge can be attached to an expression using the `Fact` node. This
  attaches a proposition to any point in the tree. When encoding a given Expr
  instance as a constraint system for e.g. an SMT solver, these facts can be
  recursively collected from a given Expr instance and added to help reduce the
  solution search space.
-}

-- phantom type tags for AST construction
data EType
  = Buf
  | Storage
  | Logs
  | EWord
  | Byte
  | End
  deriving (Typeable)

-- add type level list of constraints
data Expr (a :: EType) where

  -- knowledge
  Fact           :: Prop -> Expr a -> Expr a

  -- identifiers

  Lit            :: W256 -> Expr EWord
  Var            :: Text -> Expr EWord

  -- bytes

  LitByte        :: Word8      -> Expr Byte
  IndexWord      :: Expr EWord -> Expr EWord -> Expr Byte
  EqByte         :: Expr Byte  -> Expr Byte  -> Expr EWord

  -- TODO: rm readWord in favour of this?
  JoinBytes      :: Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr Byte -> Expr Byte -> Expr Byte -> Expr Byte
                 -> Expr EWord
  -- control flow

  Invalid         :: Expr End
  IllegalOverflow :: Expr End
  SelfDestruct    :: Expr End
  Revert          :: Expr Buf     -> Expr End
  Return          :: Expr Buf     -> Expr Storage -> Expr End
  ITE             :: Expr EWord   -> Expr End     -> Expr End -> Expr End
  TmpErr          :: String -> Expr End

  -- integers

  Add            :: Expr EWord -> Expr EWord -> Expr EWord
  Sub            :: Expr EWord -> Expr EWord -> Expr EWord
  Mul            :: Expr EWord -> Expr EWord -> Expr EWord
  Div            :: Expr EWord -> Expr EWord -> Expr EWord
  SDiv           :: Expr EWord -> Expr EWord -> Expr EWord
  Mod            :: Expr EWord -> Expr EWord -> Expr EWord
  SMod           :: Expr EWord -> Expr EWord -> Expr EWord
  AddMod         :: Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
  MulMod         :: Expr EWord -> Expr EWord -> Expr EWord -> Expr EWord
  Exp            :: Expr EWord -> Expr EWord -> Expr EWord
  SEx            :: Expr EWord -> Expr EWord -> Expr EWord
  Min            :: Expr EWord -> Expr EWord -> Expr EWord

  -- booleans

  LT             :: Expr EWord -> Expr EWord -> Expr EWord
  GT             :: Expr EWord -> Expr EWord -> Expr EWord
  LEq            :: Expr EWord -> Expr EWord -> Expr EWord
  GEq            :: Expr EWord -> Expr EWord -> Expr EWord
  SLT            :: Expr EWord -> Expr EWord -> Expr EWord
  SGT            :: Expr EWord -> Expr EWord -> Expr EWord
  Eq             :: Expr EWord -> Expr EWord -> Expr EWord
  IsZero         :: Expr EWord -> Expr EWord

  -- bits

  And            :: Expr EWord -> Expr EWord -> Expr EWord
  Or             :: Expr EWord -> Expr EWord -> Expr EWord
  Xor            :: Expr EWord -> Expr EWord -> Expr EWord
  Not            :: Expr EWord -> Expr EWord
  SHL            :: Expr EWord -> Expr EWord -> Expr EWord
  SHR            :: Expr EWord -> Expr EWord -> Expr EWord
  SAR            :: Expr EWord -> Expr EWord -> Expr EWord

  -- Hashes

  Keccak         :: Expr Buf -> Expr EWord
  SHA256         :: Expr Buf -> Expr EWord

  -- block context

  Origin         :: Expr EWord
  BlockHash      :: Expr EWord -> Expr EWord
  Coinbase       :: Expr EWord
  Timestamp      :: Expr EWord
  BlockNumber    :: Expr EWord
  Difficulty     :: Expr EWord
  GasLimit       :: Expr EWord
  ChainId        :: Expr EWord
  BaseFee        :: Expr EWord

  -- frame context

  CallValue      :: Int                -- frame idx
                 -> Expr EWord

  Caller         :: Int                -- frame idx
                 -> Expr EWord

  Address        :: Int                -- frame idx
                 -> Expr EWord

  Balance        :: Int                -- frame idx
                 -> Int                -- PC (in case we're checking the current contract)
                 -> Expr EWord         -- address
                 -> Expr EWord

  SelfBalance    :: Int                -- frame idx
                 -> Int                -- PC
                 -> Expr EWord

  Gas            :: Int                -- frame idx
                 -> Int                -- PC
                 -> Expr EWord

  -- code

  CodeSize       :: Expr EWord         -- address
                 -> Expr EWord         -- size

  ExtCodeHash    :: Expr EWord         -- address
                 -> Expr EWord         -- size

  -- logs

  EmptyLog       :: Expr Logs

  Log            :: Expr EWord         -- address
                 -> Expr Buf           -- data
                 -> [Expr EWord]       -- topics
                 -> Expr Logs          -- old logs
                 -> Expr Logs          -- new logs

  -- Contract Creation

  Create         :: Expr EWord         -- value
                 -> Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr Buf           -- memory
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- address

  Create2        :: Expr EWord         -- value
                 -> Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr EWord         -- salt
                 -> Expr Buf           -- memory
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- address

  -- Calls

  Call           :: Expr EWord         -- gas
                 -> Maybe (Expr EWord) -- target
                 -> Expr EWord         -- value
                 -> Expr EWord         -- args offset
                 -> Expr EWord         -- args size
                 -> Expr EWord         -- ret offset
                 -> Expr EWord         -- ret size
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- success

  CallCode       :: Expr EWord         -- gas
                 -> Expr EWord         -- address
                 -> Expr EWord         -- value
                 -> Expr EWord         -- args offset
                 -> Expr EWord         -- args size
                 -> Expr EWord         -- ret offset
                 -> Expr EWord         -- ret size
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- success

  DelegeateCall  :: Expr EWord         -- gas
                 -> Expr EWord         -- address
                 -> Expr EWord         -- value
                 -> Expr EWord         -- args offset
                 -> Expr EWord         -- args size
                 -> Expr EWord         -- ret offset
                 -> Expr EWord         -- ret size
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- success

  -- storage

  EmptyStore     :: Expr Storage
  ConcreteStore  :: Map W256 (Map W256 W256) -> Expr Storage
  AbstractStore  :: Expr Storage

  SLoad          :: Expr EWord         -- address
                 -> Expr EWord         -- index
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- result

  SStore         :: Expr EWord         -- address
                 -> Expr EWord         -- index
                 -> Expr EWord         -- value
                 -> Expr Storage       -- old storage
                 -> Expr Storage       -- new storae

  -- buffers

  EmptyBuf       :: Expr Buf
  ConcreteBuf    :: ByteString -> Expr Buf
  AbstractBuf    :: Text -> Expr Buf

  ReadWord       :: Expr EWord         -- index
                 -> Expr Buf           -- src
                 -> Expr EWord

  ReadByte       :: Expr EWord         -- index
                 -> Expr Buf           -- src
                 -> Expr Byte

  WriteWord      :: Expr EWord         -- dst offset
                 -> Expr EWord         -- value
                 -> Expr Buf           -- prev
                 -> Expr Buf

  WriteByte      :: Expr EWord         -- dst offset
                 -> Expr Byte          -- value
                 -> Expr Buf           -- prev
                 -> Expr Buf

  CopySlice      :: Expr EWord         -- dst offset
                 -> Expr EWord         -- src offset
                 -> Expr EWord         -- size
                 -> Expr Buf           -- src
                 -> Expr Buf           -- dst
                 -> Expr Buf

  BufLength      :: Expr Buf -> Expr EWord

deriving instance Show (Expr a)
deriving instance Eq (Expr a)
deriving instance Ord (Expr a)

-- The language of assertable expressions.
-- This is useful when generating SMT queries based on Expr instances, since
-- the translation of Eq and other boolean operators from Expr to SMT is an
-- (ite (eq a b) 1 0). We can use the boolean operators here to remove some
-- unescessary `ite` statements from our SMT encoding.
data Prop where
  PEq :: forall a . Typeable a => Expr a -> Expr a -> Prop
  PLT :: Expr EWord -> Expr EWord -> Prop
  PGT :: Expr EWord -> Expr EWord -> Prop
  PGEq :: Expr EWord -> Expr EWord -> Prop
  PLEq :: Expr EWord -> Expr EWord -> Prop
  PNeg :: Prop -> Prop
  PAnd :: Prop -> Prop -> Prop
  POr :: Prop -> Prop -> Prop
  PBool :: Bool -> Prop
deriving instance (Show Prop)

infixr 3 .&&
(.&&) :: Prop -> Prop -> Prop
x .&& y = PAnd x y

infixr 2 .||
(.||) :: Prop -> Prop -> Prop
x .|| y = POr x y

infix 4 .<, .<=, .>, .>=
(.<) :: Expr EWord -> Expr EWord -> Prop
x .< y = PLT x y
(.<=) :: Expr EWord -> Expr EWord -> Prop
x .<= y = PLEq x y
(.>) :: Expr EWord -> Expr EWord -> Prop
x .> y = PGT x y
(.>=) :: Expr EWord -> Expr EWord -> Prop
x .>= y = PGEq x y

infix 4 .==, ./=
(.==) :: (Typeable a) => Expr a -> Expr a -> Prop
x .== y = PEq x y
(./=) :: (Typeable a) => Expr a -> Expr a -> Prop
x ./= y = PNeg (PEq x y)

instance Eq Prop where
  PBool a == PBool b = a == b
  PEq (a :: Expr x) (b :: Expr x) == PEq (c :: Expr y) (d :: Expr y)
    = case eqT @x @y of
       Just Refl -> a == c && b == d
       Nothing -> False
  PLT a b == PLT c d = a == c && b == d
  PGT a b == PGT c d = a == c && b == d
  PGEq a b == PGEq c d = a == c && b == d
  PLEq a b == PLEq c d = a == c && b == d
  PNeg a == PNeg b = a == b
  PAnd a b == PAnd c d = a == c && b == d
  POr a b == POr c d = a == c && b == d
  _ == _ = False

instance Ord Prop where
  PBool a <= PBool b = a <= b
  PEq (a :: Expr x) (b :: Expr x) <= PEq (c :: Expr y) (d :: Expr y)
    = case eqT @x @y of
       Just Refl -> a <= c && b <= d
       Nothing -> False
  PLT a b <= PLT c d = a <= c && b <= d
  PGT a b <= PGT c d = a <= c && b <= d
  PGEq a b <= PGEq c d = a <= c && b <= d
  PLEq a b <= PLEq c d = a <= c && b <= d
  PNeg a <= PNeg b = a <= b
  PAnd a b <= PAnd c d = a <= c && b <= d
  POr a b <= POr c d = a <= c && b <= d
  _ <= _ = False

foldProp :: forall b . Monoid b => (forall a . Expr a -> b) -> b -> Prop -> b
foldProp f acc p = acc <> (go p)
  where
    go :: Prop -> b
    go = \case
      PBool _ -> mempty
      PEq a b -> (foldExpr f mempty a) <> (foldExpr f mempty b)
      PLT a b -> foldExpr f mempty a <> foldExpr f mempty b
      PGT a b -> foldExpr f mempty a <> foldExpr f mempty b
      PGEq a b -> foldExpr f mempty a <> foldExpr f mempty b
      PLEq a b -> foldExpr f mempty a <> foldExpr f mempty b
      PNeg a -> go a
      PAnd a b -> go a <> go b
      POr a b -> go a <> go b

-- | Recursively folds a given function over a given expression
-- Recursion schemes do this & a lot more, but defining them over GADT's isn't worth the hassle
foldExpr :: forall b c . Monoid b => (forall a . Expr a -> b) -> b -> Expr c -> b
foldExpr f acc expr = acc <> (go expr)
  where
    go :: forall a . Expr a -> b
    go = \case
      -- knowledge
      e@(Fact p a) -> f e <> foldProp f mempty p <> go a

      -- literals & variables

      e@(Lit _) -> f e
      e@(LitByte _) -> f e
      e@(Var _) -> f e

      -- bytes

      e@(IndexWord a b) -> f e <> (go a) <> (go b)
      e@(EqByte a b) -> f e <> (go a) <> (go b)

      e@(JoinBytes
        zero one two three four five six seven
        eight nine ten eleven twelve thirteen fourteen fifteen
        sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree
        twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone)
        -> f e
        <> (go zero) <> (go one) <> (go two) <> (go three)
        <> (go four) <> (go five) <> (go six) <> (go seven)
        <> (go eight) <> (go nine) <> (go ten) <> (go eleven)
        <> (go twelve) <> (go thirteen) <> (go fourteen)
        <> (go fifteen) <> (go sixteen) <> (go seventeen)
        <> (go eighteen) <> (go nineteen) <> (go twenty)
        <> (go twentyone) <> (go twentytwo) <> (go twentythree)
        <> (go twentyfour) <> (go twentyfive) <> (go twentysix)
        <> (go twentyseven) <> (go twentyeight) <> (go twentynine)
        <> (go thirty) <> (go thirtyone)

      -- control flow

      e@Invalid -> f e
      e@SelfDestruct -> f e
      e@IllegalOverflow -> f e
      e@(Revert a) -> f e <> (go a)
      e@(Return a b) -> f e <> (go a) <> (go b)
      e@(ITE a b c) -> f e <> (go a) <> (go b) <> (go c)
      e@(TmpErr _) -> f e

      -- integers

      e@(Add a b) -> f e <> (go a) <> (go b)
      e@(Sub a b) -> f e <> (go a) <> (go b)
      e@(Mul a b) -> f e <> (go a) <> (go b)
      e@(Div a b) -> f e <> (go a) <> (go b)
      e@(SDiv a b) -> f e <> (go a) <> (go b)
      e@(Mod a b) -> f e <> (go a) <> (go b)
      e@(SMod a b) -> f e <> (go a) <> (go b)
      e@(AddMod a b c) -> f e <> (go a) <> (go b) <> (go c)
      e@(MulMod a b c) -> f e <> (go a) <> (go b) <> (go c)
      e@(Exp a b) -> f e <> (go a) <> (go b)
      e@(SEx a b) -> f e <> (go a) <> (go b)
      e@(Min a b) -> f e <> (go a) <> (go b)

      -- booleans

      e@(LT a b) -> f e <> (go a) <> (go b)
      e@(GT a b) -> f e <> (go a) <> (go b)
      e@(LEq a b) -> f e <> (go a) <> (go b)
      e@(GEq a b) -> f e <> (go a) <> (go b)
      e@(SLT a b) -> f e <> (go a) <> (go b)
      e@(SGT a b) -> f e <> (go a) <> (go b)
      e@(Eq a b) -> f e <> (go a) <> (go b)
      e@(IsZero a) -> f e <> (go a)

      -- bits

      e@(And a b) -> f e <> (go a) <> (go b)
      e@(Or a b) -> f e <> (go a) <> (go b)
      e@(Xor a b) -> f e <> (go a) <> (go b)
      e@(Not a) -> f e <> (go a)
      e@(SHL a b) -> f e <> (go a) <> (go b)
      e@(SHR a b) -> f e <> (go a) <> (go b)
      e@(SAR a b) -> f e <> (go a) <> (go b)

      -- Hashes

      e@(Keccak a) -> f e <> (go a)
      e@(SHA256 a) -> f e <> (go a)

      -- block context

      e@(Origin) -> f e
      e@(Coinbase) -> f e
      e@(Timestamp) -> f e
      e@(BlockNumber) -> f e
      e@(Difficulty) -> f e
      e@(GasLimit) -> f e
      e@(ChainId) -> f e
      e@(BaseFee) -> f e
      e@(BlockHash _) -> f e

      -- frame context

      e@(Caller _) -> f e
      e@(CallValue _) -> f e
      e@(Address _) -> f e
      e@(SelfBalance _ _) -> f e
      e@(Gas _ _) -> f e
      e@(Balance {}) -> f e

      -- code

      e@(CodeSize a) -> f e <> (go a)
      e@(ExtCodeHash a) -> f e <> (go a)

      -- logs

      e@(EmptyLog) -> f e
      e@(Log a b c d) -> f e <> (go a) <> (go b) <> (foldl (<>) mempty (fmap f c)) <> (go d)

      -- Contract Creation

      e@(Create a b c d g h)
        -> f e
        <> (go a)
        <> (go b)
        <> (go c)
        <> (go d)
        <> (go g)
        <> (go h)
      e@(Create2 a b c d g h i)
        -> f e
        <> (go a)
        <> (go b)
        <> (go c)
        <> (go d)
        <> (go g)
        <> (go h)
        <> (go i)

      -- Calls

      e@(Call a b c d g h i j k)
        -> f e
        <> (go a)
        <> (maybe mempty (go) b)
        <> (go c)
        <> (go d)
        <> (go g)
        <> (go h)
        <> (go i)
        <> (go j)
        <> (go k)

      e@(CallCode a b c d g h i j k)
        -> f e
        <> (go a)
        <> (go b)
        <> (go c)
        <> (go d)
        <> (go g)
        <> (go h)
        <> (go i)
        <> (go j)
        <> (go k)

      e@(DelegeateCall a b c d g h i j k)
        -> f e
        <> (go a)
        <> (go b)
        <> (go c)
        <> (go d)
        <> (go g)
        <> (go h)
        <> (go i)
        <> (go j)
        <> (go k)

      -- storage

      e@(EmptyStore) -> f e
      e@(ConcreteStore _) -> f e
      e@(AbstractStore) -> f e
      e@(SLoad a b c) -> f e <> (go a) <> (go b) <> (go c)
      e@(SStore a b c d) -> f e <> (go a) <> (go b) <> (go c) <> (go d)

      -- buffers

      e@(EmptyBuf) -> f e
      e@(ConcreteBuf _) -> f e
      e@(AbstractBuf _) -> f e
      e@(ReadWord a b) -> f e <> (go a) <> (go b)
      e@(ReadByte a b) -> f e <> (go a) <> (go b)
      e@(WriteWord a b c) -> f e <> (go a) <> (go b) <> (go c)
      e@(WriteByte a b c) -> f e <> (go a) <> (go b) <> (go c)

      e@(CopySlice a b c d g)
        -> f e
        <> (go a)
        <> (go b)
        <> (go c)
        <> (go d)
        <> (go g)
      e@(BufLength a) -> f e <> (go a)

mapProp :: (forall a . Expr a -> Expr a) -> Prop -> Prop
mapProp f = \case
  PBool b -> PBool b
  PEq a b -> PEq (mapExpr f (f a)) (mapExpr f (f b))
  PLT a b -> PEq (mapExpr f (f a)) (mapExpr f (f b))
  PGT a b -> PEq (mapExpr f (f a)) (mapExpr f (f b))
  PLEq a b -> PEq (mapExpr f (f a)) (mapExpr f (f b))
  PGEq a b -> PEq (mapExpr f (f a)) (mapExpr f (f b))
  PNeg a -> PNeg (mapProp f a)
  PAnd a b -> PAnd (mapProp f a) (mapProp f b)
  POr a b -> POr (mapProp f a) (mapProp f b)

-- | Recursively applies a given function to every node in a given expr instance
-- Recursion schemes do this & a lot more, but defining them over GADT's isn't worth the hassle
mapExpr :: (forall a . Expr a -> Expr a) -> Expr b -> Expr b
mapExpr f expr = case (f expr) of
  -- knowledge
  Fact p a -> Fact (mapProp f p) (mapExpr f (f a))

  -- literals & variables

  Lit a -> Lit a
  LitByte a -> LitByte a
  Var a -> Var a

  -- bytes

  IndexWord a b -> IndexWord (mapExpr f (f a)) (mapExpr f (f b))
  EqByte a b -> EqByte (mapExpr f (f a)) (mapExpr f (f b))

  JoinBytes
    zero one two three four five six seven
    eight nine ten eleven twelve thirteen fourteen fifteen
    sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree
    twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone
    -> JoinBytes
        (mapExpr f (f zero)) (mapExpr f (f one)) (mapExpr f (f two)) (mapExpr f (f three))
        (mapExpr f (f four)) (mapExpr f (f five)) (mapExpr f (f six)) (mapExpr f (f seven))
        (mapExpr f (f eight)) (mapExpr f (f nine)) (mapExpr f (f ten)) (mapExpr f (f eleven))
        (mapExpr f (f twelve)) (mapExpr f (f thirteen)) (mapExpr f (f fourteen))
        (mapExpr f (f fifteen)) (mapExpr f (f sixteen)) (mapExpr f (f seventeen))
        (mapExpr f (f eighteen)) (mapExpr f (f nineteen)) (mapExpr f (f twenty))
        (mapExpr f (f twentyone)) (mapExpr f (f twentytwo)) (mapExpr f (f twentythree))
        (mapExpr f (f twentyfour)) (mapExpr f (f twentyfive)) (mapExpr f (f twentysix))
        (mapExpr f (f twentyseven)) (mapExpr f (f twentyeight)) (mapExpr f (f twentynine))
        (mapExpr f (f thirty)) (mapExpr f (f thirtyone))

  -- control flow

  Invalid -> Invalid
  SelfDestruct -> SelfDestruct
  IllegalOverflow -> IllegalOverflow
  Revert a -> Revert (mapExpr f (f a))
  Return a b -> Return (mapExpr f (f a)) (mapExpr f (f b))
  ITE a b c -> ITE (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c))
  TmpErr a -> TmpErr a

  -- integers

  Add a b -> Add (mapExpr f (f a)) (mapExpr f (f b))
  Sub a b -> Sub (mapExpr f (f a)) (mapExpr f (f b))
  Mul a b -> Mul (mapExpr f (f a)) (mapExpr f (f b))
  Div a b -> Div (mapExpr f (f a)) (mapExpr f (f b))
  SDiv a b -> SDiv (mapExpr f (f a)) (mapExpr f (f b))
  Mod a b -> Mod (mapExpr f (f a)) (mapExpr f (f b))
  SMod a b -> SMod (mapExpr f (f a)) (mapExpr f (f b))
  AddMod a b c -> AddMod (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c))
  MulMod a b c -> MulMod (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c))
  Exp a b -> Exp (mapExpr f (f a)) (mapExpr f (f b))
  SEx a b -> SEx (mapExpr f (f a)) (mapExpr f (f b))
  Min a b -> Min (mapExpr f (f a)) (mapExpr f (f b))

  -- booleans

  LT a b ->  LT (mapExpr f (f a)) (mapExpr f (f b))
  GT a b ->  GT (mapExpr f (f a)) (mapExpr f (f b))
  LEq a b -> LEq (mapExpr f (f a)) (mapExpr f (f b))
  GEq a b -> GEq (mapExpr f (f a)) (mapExpr f (f b))
  SLT a b -> SLT (mapExpr f (f a)) (mapExpr f (f b))
  SGT a b -> SGT (mapExpr f (f a)) (mapExpr f (f b))
  Eq a b ->  Eq (mapExpr f (f a)) (mapExpr f (f b))
  IsZero a -> IsZero (mapExpr f (f a))

  -- bits

  And a b -> And (mapExpr f (f a)) (mapExpr f (f b))
  Or a b ->  Or (mapExpr f (f a)) (mapExpr f (f b))
  Xor a b -> Xor (mapExpr f (f a)) (mapExpr f (f b))
  Not a -> Not (mapExpr f (f a))
  SHL a b -> SHL (mapExpr f (f a)) (mapExpr f (f b))
  SHR a b -> SHR (mapExpr f (f a)) (mapExpr f (f b))
  SAR a b -> SAR (mapExpr f (f a)) (mapExpr f (f b))

  -- Hashes

  Keccak a -> Keccak (mapExpr f (f a))
  SHA256 a -> SHA256 (mapExpr f (f a))

  -- block context

  Origin -> Origin
  Coinbase -> Coinbase
  Timestamp -> Timestamp
  BlockNumber -> BlockNumber
  Difficulty -> Difficulty
  GasLimit -> GasLimit
  ChainId -> ChainId
  BaseFee -> BaseFee
  BlockHash a -> BlockHash (mapExpr f (f a))

  -- frame context

  Caller a -> Caller a
  CallValue a -> CallValue a
  Address a -> Address a
  SelfBalance a b -> SelfBalance a b
  Gas a b -> Gas a b
  Balance a b c -> Balance a b (mapExpr f (f c))

  -- code

  CodeSize a -> CodeSize (mapExpr f (f a))
  ExtCodeHash a -> ExtCodeHash (mapExpr f (f a))

  -- logs

  EmptyLog -> EmptyLog
  Log a b c d -> Log (mapExpr f (f a)) (mapExpr f (f b)) (fmap (mapExpr f . f) c) (mapExpr f (f d))

  -- Contract Creation

  Create a b c d e g
    -> Create
         (mapExpr f (f a))
         (mapExpr f (f b))
         (mapExpr f (f c))
         (mapExpr f (f d))
         (mapExpr f (f e))
         (mapExpr f (f g))
  Create2 a b c d e g h
    -> Create2
         (mapExpr f (f a))
         (mapExpr f (f b))
         (mapExpr f (f c))
         (mapExpr f (f d))
         (mapExpr f (f e))
         (mapExpr f (f g))
         (mapExpr f (f h))

  -- Calls

  Call a b c d e g h i j
    -> Call
         (mapExpr f (f a))
         (fmap (mapExpr f . f) b)
         (mapExpr f (f c))
         (mapExpr f (f d))
         (mapExpr f (f e))
         (mapExpr f (f g))
         (mapExpr f (f h))
         (mapExpr f (f i))
         (mapExpr f (f j))
  CallCode a b c d e g h i j
    -> CallCode
        (mapExpr f (f a))
        (mapExpr f (f b))
        (mapExpr f (f c))
        (mapExpr f (f d))
        (mapExpr f (f e))
        (mapExpr f (f g))
        (mapExpr f (f h))
        (mapExpr f (f i))
        (mapExpr f (f j))
  DelegeateCall a b c d e g h i j
    -> DelegeateCall
        (mapExpr f (f a))
        (mapExpr f (f b))
        (mapExpr f (f c))
        (mapExpr f (f d))
        (mapExpr f (f e))
        (mapExpr f (f g))
        (mapExpr f (f h))
        (mapExpr f (f i))
        (mapExpr f (f j))

  -- storage

  EmptyStore -> EmptyStore
  ConcreteStore a -> ConcreteStore a
  AbstractStore -> AbstractStore
  SLoad a b c -> SLoad (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c))
  SStore a b c d -> SStore (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c)) (mapExpr f (f d))

  -- buffers

  EmptyBuf -> EmptyBuf
  ConcreteBuf a -> ConcreteBuf a
  AbstractBuf a -> AbstractBuf a
  ReadWord a b -> ReadWord (mapExpr f (f a)) (mapExpr f (f b))
  ReadByte a b -> ReadByte (mapExpr f (f a)) (mapExpr f (f b))
  WriteWord a b c -> WriteWord (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c))
  WriteByte a b c -> WriteByte (mapExpr f (f a)) (mapExpr f (f b)) (mapExpr f (f c))

  CopySlice a b c d e
    -> CopySlice
         (mapExpr f (f a))
         (mapExpr f (f b))
         (mapExpr f (f c))
         (mapExpr f (f d))
         (mapExpr f (f e))
  BufLength a -> BufLength (mapExpr f (f a))

unlit :: Expr EWord -> Maybe W256
unlit (Lit x) = Just x
unlit _ = Nothing

unlitByte :: Expr Byte -> Maybe Word8
unlitByte (LitByte x) = Just x
unlitByte _ = Nothing

newtype ByteStringS = ByteStringS ByteString deriving (Eq)

instance Show ByteStringS where
  show (ByteStringS x) = ("0x" ++) . Text.unpack . fromBinary $ x
    where
      fromBinary =
        Text.decodeUtf8 . toStrict . toLazyByteString . byteStringHex

instance JSON.ToJSON ByteStringS where
  toJSON = JSON.String . Text.pack . show

-- | Symbolic words of 256 bits, possibly annotated with additional
--   "insightful" information
--data SymWord = S (Expr EWord) (SWord 256)

--instance Show SymWord where
  --show (S w _) = show w

--var :: String -> SWord 256 -> SymWord
--var name = S (Var name)

---- | Custom instances for SymWord, many of which have direct
---- analogues for concrete words defined in Concrete.hs
--instance EqSymbolic SymWord where
  --(.==) (S _ x) (S _ y) = x .== y

--instance Num SymWord where
  --(S a x) + (S b y) = S (Add a b) (x + y)
  --(S a x) * (S b y) = S (Mul a b) (x * y)
  --abs (S a x) = S (Todo "abs" a) (abs x)
  --signum (S a x) = S (Todo "signum" a) (signum x)
  --fromInteger x = S (Lit (fromInteger x)) (fromInteger x)
  --negate (S a x) = S (Todo "negate" a) (negate x)

--instance Bits SymWord where
  --(S a x) .&. (S b y) = S (And a b) (x .&. y)
  --(S a x) .|. (S b y) = S (Or  a b) (x .|. y)
  --(S a x) `xor` (S b y) = S (Xor a b) (x `xor` y)
  --complement (S a x) = S (Not a) (complement x)
  --shiftL (S a x) i = S (SHL a (Lit $ fromIntegral i)) (shiftL x i)
  --shiftR (S a x) i = S (SHR a (Lit $ fromIntegral i)) (shiftR x i)
  --rotate (S a x) i = S (Todo "rotate " a) (rotate x i) -- unused.
  --bitSize (S _ x) = bitSize x
  --bitSizeMaybe (S _ x) = bitSizeMaybe x
  --isSigned (S _ x) = isSigned x
  --testBit (S _ x) i = testBit x i
  --bit i = w256lit (bit i)
  --popCount (S _ x) = popCount x

---- sQuotRem and sDivMod are identical for SWord 256
---- prove $ \x y -> x `sQuotRem` (y :: SWord 256) .== x `sDivMod` y
---- Q.E.D.
--instance SDivisible SymWord where
  --sQuotRem (S x' x) (S y' y) = let (a, b) = x `sQuotRem` y
                               --in (S (Div x' y') a, S (Mod x' y') b)
  --sDivMod = sQuotRem

-- | Instead of supporting a Mergeable instance directly,
-- we use one which carries the Whiff around:
--iteExpr :: (Expr EWord) -> SBool -> SWord 256 -> SWord 256 -> SymWord
--iteExpr e b x y = S e (ite b x y)

--instance Bounded SymWord where
  --minBound = w256lit minBound
  --maxBound = w256lit maxBound

--instance Eq SymWord where
  --(S _ x) == (S _ y) = x == y

--instance Enum SymWord where
  --toEnum i = w256lit (toEnum i)
  --fromEnum (S _ x) = fromEnum x

newtype Addr = Addr { addressWord160 :: Word160 }
  deriving
    ( Num, Integral, Real, Ord, Enum
    , Eq, Generic, Bits, FiniteBits
    )

--newtype SAddr = SAddr { saddressWord160 :: SWord 160 }
  --deriving (Num)

-- | Capture the correspondence between sized and fixed-sized BVs
-- (This is blatant copypasta of `FromSized` from sbv, which just
-- happens to be defined up to 64 bits)
--type family FromSizzle (t :: Type) :: Type where
   --FromSizzle (WordN 256) = W256
   --FromSizzle (WordN 160) = Addr

-- | Conversion from a sized BV to a fixed-sized bit-vector.
--class FromSizzleBV a where
   -- | Convert a sized bit-vector to the corresponding fixed-sized bit-vector,
   -- for instance 'SWord 16' to 'SWord16'. See also 'toSized'.
   --fromSizzle :: a -> FromSizzle a

   --default fromSizzle :: (Num (FromSizzle a), Integral a) => a -> FromSizzle a
   --fromSizzle = fromIntegral


maybeLitWord :: Expr EWord -> Maybe W256
maybeLitWord (Lit w) = Just w
maybeLitWord _ = Nothing

-- | convert between (WordN 256) and Word256
--type family ToSizzle (t :: Type) :: Type where
    --ToSizzle W256 = (WordN 256)
    --ToSizzle Addr = (WordN 160)

-- | Conversion from a fixed-sized BV to a sized bit-vector.
--class ToSizzleBV a where
   -- | Convert a fixed-sized bit-vector to the corresponding sized bit-vector,
   --toSizzle :: a -> ToSizzle a

   --default toSizzle :: (Num (ToSizzle a), Integral a) => (a -> ToSizzle a)
   --toSizzle = fromIntegral


--instance (ToSizzleBV W256)
--instance (FromSizzleBV (WordN 256))
--instance (ToSizzleBV Addr)
--instance (FromSizzleBV (WordN 160))

-- | Operations over buffers (concrete or symbolic)

-- | A buffer is a list of bytes. For concrete execution, this is simply `ByteString`.
-- In symbolic settings, it is a list of symbolic bitvectors of size 8.
--instance Show Buffer where
  --show (ConcreteBuffer b) = show $ ByteStringS b
  --show (SymbolicBuffer b) = show (length b) ++ " bytes"


--instance Semigroup Buffer where
  --ConcreteBuffer a <> ConcreteBuffer b = ConcreteBuffer (a <> b)
  --ConcreteBuffer a <> SymbolicBuffer b = SymbolicBuffer (litBytes a <> b)
  --SymbolicBuffer a <> ConcreteBuffer b = SymbolicBuffer (a <> litBytes b)
  --SymbolicBuffer a <> SymbolicBuffer b = SymbolicBuffer (a <> b)

--instance Monoid Buffer where
  --mempty = ConcreteBuffer mempty

--instance EqSymbolic Buffer where
  --ConcreteBuffer a .== ConcreteBuffer b = literal (a == b)
  --ConcreteBuffer a .== SymbolicBuffer b = litBytes a .== b
  --SymbolicBuffer a .== ConcreteBuffer b = a .== litBytes b
  --SymbolicBuffer a .== SymbolicBuffer b = a .== b


instance Read W256 where
  readsPrec _ "0x" = [(0, "")]
  readsPrec n s = first W256 <$> readsPrec n s

instance Show W256 where
  showsPrec _ s = ("0x" ++) . showHex s

instance JSON.ToJSON W256 where
  toJSON = JSON.String . Text.pack . show

instance Read Addr where
  readsPrec _ ('0':'x':s) = readHex s
  readsPrec _ s = readHex s

instance Show Addr where
  showsPrec _ addr next =
    let hex = showHex addr next
        str = replicate (40 - length hex) '0' ++ hex
    in "0x" ++ toChecksumAddress str ++ drop 40 str

--instance Show SAddr where
  --show (SAddr a) = case unliteral a of
    --Nothing -> "<symbolic addr>"
    --Just c -> show $ fromSizzle c

-- https://eips.ethereum.org/EIPS/eip-55
toChecksumAddress :: String -> String
toChecksumAddress addr = zipWith transform nibbles addr
  where
    nibbles = unpackNibbles . BS.take 20 $ keccakBytes (Char8.pack addr)
    transform nibble = if nibble >= 8 then toUpper else id

strip0x :: ByteString -> ByteString
strip0x bs = if "0x" `Char8.isPrefixOf` bs then Char8.drop 2 bs else bs

strip0x' :: String -> String
strip0x' s = if "0x" `isPrefixOf` s then drop 2 s else s

instance FromJSON W256 where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")]  -> return x
      _          -> fail $ "invalid hex word (" ++ s ++ ")"

instance FromJSON Addr where
  parseJSON v = do
    s <- Text.unpack <$> parseJSON v
    case reads s of
      [(x, "")] -> return x
      _         -> fail $ "invalid address (" ++ s ++ ")"

#if MIN_VERSION_aeson(1, 0, 0)

instance FromJSONKey W256 where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")]  -> return x
      _          -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

instance FromJSONKey Addr where
  fromJSONKey = FromJSONKeyTextParser $ \s ->
    case reads (Text.unpack s) of
      [(x, "")] -> return x
      _         -> fail $ "invalid word (" ++ Text.unpack s ++ ")"

#endif

instance ParseField W256
instance ParseFields W256
instance ParseRecord W256 where
  parseRecord = fmap getOnly parseRecord

instance ParseField Addr
instance ParseFields Addr
instance ParseRecord Addr where
  parseRecord = fmap getOnly parseRecord

hexByteString :: String -> ByteString -> ByteString
hexByteString msg bs =
  case BS16.decode bs of
    Right x -> x
    _ -> error ("invalid hex bytestring for " ++ msg)

hexText :: Text -> ByteString
hexText t =
  case BS16.decode (Text.encodeUtf8 (Text.drop 2 t)) of
    Right x -> x
    _ -> error ("invalid hex bytestring " ++ show t)

readN :: Integral a => String -> a
readN s = fromIntegral (read s :: Integer)

readNull :: Read a => a -> String -> a
readNull x = fromMaybe x . Text.Read.readMaybe

wordField :: JSON.Object -> Key -> JSON.Parser W256
wordField x f = ((readNull 0) . Text.unpack)
                  <$> (x .: f)

addrField :: JSON.Object -> Key -> JSON.Parser Addr
addrField x f = (read . Text.unpack) <$> (x .: f)

addrFieldMaybe :: JSON.Object -> Key -> JSON.Parser (Maybe Addr)
addrFieldMaybe x f = (Text.Read.readMaybe . Text.unpack) <$> (x .: f)

dataField :: JSON.Object -> Key -> JSON.Parser ByteString
dataField x f = hexText <$> (x .: f)

toWord512 :: W256 -> Word512
toWord512 (W256 x) = fromHiAndLo 0 x

fromWord512 :: Word512 -> W256
fromWord512 x = W256 (loWord x)

num :: (Integral a, Num b) => a -> b
num = fromIntegral

padLeft :: Int -> ByteString -> ByteString
padLeft n xs = BS.replicate (n - BS.length xs) 0 <> xs

padLeftStr :: Int -> String -> String
padLeftStr n xs = replicate (n - length xs) '0' <> xs

padRight :: Int -> ByteString -> ByteString
padRight n xs = xs <> BS.replicate (n - BS.length xs) 0

padRight' :: Int -> String -> String
padRight' n xs = xs <> replicate (n - length xs) '0'

-- | Right padding  / truncating
--truncpad :: Int -> [SWord 8] -> [SWord 8]
--truncpad n xs = if m > n then take n xs
                --else mappend xs (replicate (n - m) 0)
  --where m = length xs

padLeft' :: Int -> [Expr Byte] -> [Expr Byte]
padLeft' n xs = replicate (n - length xs) (LitByte 0) <> xs

word256 :: ByteString -> Word256
word256 xs = case Cereal.runGet m (padLeft 32 xs) of
               Left _ -> error "internal error"
               Right x -> x
  where
    m = do a <- Cereal.getWord64be
           b <- Cereal.getWord64be
           c <- Cereal.getWord64be
           d <- Cereal.getWord64be
           return $ fromHiAndLo (fromHiAndLo a b) (fromHiAndLo c d)

word :: ByteString -> W256
word = W256 . word256

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

fromBE :: (Integral a) => ByteString -> a
fromBE xs = if xs == mempty then 0
  else 256 * fromBE (BS.init xs)
       + (num $ BS.last xs)

asBE :: (Integral a) => a -> ByteString
asBE 0 = mempty
asBE x = asBE (x `div` 256)
  <> BS.pack [num $ x `mod` 256]

word256Bytes :: W256 -> ByteString
word256Bytes x = BS.pack [byteAt x (31 - i) | i <- [0..31]]

word160Bytes :: Addr -> ByteString
word160Bytes x = BS.pack [byteAt (addressWord160 x) (19 - i) | i <- [0..19]]

newtype Nibble = Nibble Word8
  deriving ( Num, Integral, Real, Ord, Enum, Eq, Bounded, Generic)

instance Show Nibble where
  show = (:[]) . intToDigit . num

-- Get first and second Nibble from byte
hi, lo :: Word8 -> Nibble
hi b = Nibble $ b `shiftR` 4
lo b = Nibble $ b .&. 0x0f

toByte :: Nibble -> Nibble -> Word8
toByte  (Nibble high) (Nibble low) = high `shift` 4 .|. low

unpackNibbles :: ByteString -> [Nibble]
unpackNibbles bs = BS.unpack bs >>= unpackByte
  where unpackByte b = [hi b, lo b]

-- Well-defined for even length lists only (plz dependent types)
packNibbles :: [Nibble] -> ByteString
packNibbles [] = mempty
packNibbles (n1:n2:ns) = BS.singleton (toByte n1 n2) <> packNibbles ns
packNibbles _ = error "can't pack odd number of nibbles"

-- Keccak hashing

keccakBytes :: ByteString -> ByteString
keccakBytes =
  (hash :: ByteString -> Digest Keccak_256)
    >>> BA.unpack
    >>> BS.pack

word32 :: [Word8] -> Word32
word32 xs = sum [ fromIntegral x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

keccak :: Expr Buf -> Expr EWord
keccak (ConcreteBuf bs) = Lit $ keccak' bs
keccak buf = Keccak buf

keccak' :: ByteString -> W256
keccak' = keccakBytes >>> BS.take 32 >>> word

abiKeccak :: ByteString -> Word32
abiKeccak =
  keccakBytes
    >>> BS.take 4
    >>> BS.unpack
    >>> word32

-- Utils

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = fmap concat (mapM f xs)

regexMatches :: Text -> Text -> Bool
regexMatches regexSource =
  let
    compOpts =
      Regex.defaultCompOpt { Regex.lastStarGreedy = True }
    execOpts =
      Regex.defaultExecOpt { Regex.captureGroups = False }
    regex = Regex.makeRegexOpts compOpts execOpts (Text.unpack regexSource)
  in
    Regex.matchTest regex . Seq.fromList . Text.unpack

-- | A total variant of (!!)
(!?) :: Foldable f => f a -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
