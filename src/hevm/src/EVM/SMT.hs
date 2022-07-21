{-# Language DataKinds #-}
{-# Language LambdaCase #-}
{-# Language QuasiQuotes #-}

{- |
    Module: EVM.SMT
    Description: Utilities for building and executing SMT queries from Expr instances
-}
module EVM.SMT where

import Prelude hiding (LT, GT)

import Data.String.Here

import EVM.Types
import EVM.Expr hiding (copySlice, writeWord, op1, op2)

prelude :: String
prelude = [i|
    ; hash functions
    (declare-fun keccak ((Array (_ BitVec 256) (_ BitVec 256))) (_ BitVec 256))
    (declare-fun sha256 ((Array (_ BitVec 256) (_ BitVec 256))) (_ BitVec 256))

    ; block context
    (declare-fun blockhash ((_ BitVec 256)) (_ BitVec 256))
    (declare-const origin (_ BitVec 256))
    (declare-const coinbase (_ BitVec 256))
    (declare-const timestamp (_ BitVec 256))
    (declare-const blocknumber (_ BitVec 256))
    (declare-const difficulty (_ BitVec 256))
    (declare-const gaslimit (_ BitVec 256))
    (declare-const chainid (_ BitVec 256))
    (declare-const basefee (_ BitVec 256))

    ; storage
    (declare-const storage (Array (_ BitVec 256) (Array (_ BitVec 256) (_ BitVec 256))))
  |]

declareBufs :: [String] -> String
declareBufs names = unlines $ fmap declare names
  where
    declare n = "(declare-const " <> n <> "(Array (_ BitVec 256) (_ BitVec 8)))"

-- | Walks the expression and returns the names of all referenced buffers
referencedBufs :: Expr a -> [String]
referencedBufs = \case
  AbstractBuf s -> [s]
  Lit _ -> []
  Var _ -> []
  LitByte _ -> []
  IndexWord a b -> go a <> go b
  EqByte a b -> go a <> go b
  JoinBytes
    zero one two three four five six seven
    eight nine ten eleven twelve thirteen fourteen fifteen
    sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree
    twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone
    -> go zero <> go one <> go two <> go three
    <> go four <> go five <> go six <> go seven
    <> go eight <> go nine <> go ten <> go eleven
    <> go twelve <> go thirteen <> go fourteen
    <> go fifteen <> go sixteen <> go seventeen
    <> go eighteen <> go nineteen <> go twenty
    <> go twentyone <> go twentytwo <> go twentythree
    <> go twentyfour <> go twentyfive <> go twentysix
    <> go twentyseven <> go twentyeight <> go twentynine
    <> go thirty <> go thirtyone
  Invalid -> []
  SelfDestruct -> []
  Revert a -> go a
  Return a b -> go a <> go b
  ITE a b c -> go a <> go b <> go c
  Add a b -> go a <> go b
  Sub a b -> go a <> go b
  Mul a b -> go a <> go b
  Div a b -> go a <> go b
  SDiv a b -> go a <> go b
  Mod a b -> go a <> go b
  AddMod a b c -> go a <> go b <> go c
  MulMod a b c -> go a <> go b <> go c
  Exp a b -> go a <> go b
  SEx a b -> go a <> go b
  Min a b -> go a <> go b
  LT a b -> go a <> go b
  GT a b -> go a <> go b
  LEq a b -> go a <> go b
  GEq a b -> go a <> go b
  SLT a b -> go a <> go b
  SGT a b -> go a <> go b
  Eq a b -> go a <> go b
  IsZero a -> go a
  And a b -> go a <> go b
  Or a b -> go a <> go b
  Xor a b -> go a <> go b
  Not a -> go a
  SHL a b -> go a <> go b
  SHR a b -> go a <> go b
  SAR a b -> go a <> go b
  Keccak a -> go a
  SHA256 a -> go a
  Origin -> []
  BlockHash a -> go a
  Coinbase -> []
  Timestamp -> []
  BlockNumber -> []
  Difficulty -> []
  GasLimit -> []
  ChainId -> []
  BaseFee -> []
  EmptyStore -> []
  ConcreteStore _ -> []
  AbstractStore -> []
  SLoad a b c -> go a <> go b <> go c
  SStore a b c e -> go a <> go b <> go c <> go e
  EmptyBuf -> []
  ConcreteBuf _ -> []
  ReadWord a b -> go a <> go b
  ReadByte a b -> go a <> go b
  WriteWord a b c -> go a <> go b <> go c
  WriteByte a b c -> go a <> go b <> go c
  CopySlice a b c e f -> go a <> go b <> go c <> go e <> go f
  a -> error $ "TODO: implement: " <> show a
  where
    go = referencedBufs

-- encodes a word into smt
wordToSMT :: Expr EWord -> String
wordToSMT = \case
  Lit w -> "#x" <> (padLeftStr 64 . strip0x' . show $ w)
  Var s -> s
  JoinBytes
    zero one two three four five six seven
    eight nine ten eleven twelve thirteen fourteen fifteen
    sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree
    twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone
    -> concatBytes
        [ zero, one, two, three, four, five, six, seven
        , eight, nine, ten, eleven, twelve, thirteen, fourteen, fifteen
        , sixteen, seventeen, eighteen, nineteen, twenty, twentyone, twentytwo, twentythree
        , twentyfour, twentyfive, twentysix, twentyseven, twentyeight, twentynine, thirty, thirtyone]

  Add a b -> op2 "bvadd" a b
  Sub a b -> op2 "bv" a b
  Mul a b -> op2 "bvmul" a b
  Div a b -> op2 "bvudiv" a b
  Exp a b -> case b of
               Lit b' -> expandExp a b'
               _ -> error "cannot encode symbolic exponentation into SMT"
  Min a b -> "(ite (<= " <> wordToSMT a <> " " <> wordToSMT b <> ") " <> wordToSMT a <> " " <> wordToSMT b <> ")"

  LT a b -> op2 "bvult" a b
  GT a b -> wordToSMT $ And (Not (LT a b)) (Not (Eq a b))
  LEq a b -> wordToSMT $ Not (LT b a)
  GEq a b -> wordToSMT $ Not (LT a b)
  Eq a b -> op2 "=" a b
  IsZero a -> "(ite (= " <> wordToSMT a <> " 0) " <> wordToSMT (Lit 1) <> " " <> wordToSMT (Lit 0) <> ")"

  And a b -> op2 "bvand" a b
  Or a b -> op2 "bvor" a b
  Xor a b -> wordToSMT $ And (Or a b) (Not (And a b))
  Not a -> op1 "bvnot" a
  SHL a b -> op2 "bvshl" a b
  SHR a b -> op2 "bvlshr" a b -- TODO: is lshr the same as shr?
  EqByte a b -> "(= " <> byteToSMT a  <> " " <> byteToSMT b <> ")"

  Keccak a -> "(keccak " <> bufToSMT a <> ")"
  SHA256 a -> "(sha256 " <> bufToSMT a <> ")"

  Origin -> "origin"
  BlockHash a -> "(blockhash " <> wordToSMT a <> ")"
  Coinbase -> "coinbase"
  Timestamp -> "timestamp"
  BlockNumber -> "blocknumber"
  Difficulty -> "difficulty"
  GasLimit -> "gaslimit"
  ChainId -> "chainid"
  BaseFee -> "basefee"

  a -> error $ "TODO: implement: " <> show a
  where
    op1 op a = "(" <> op <> " " <> wordToSMT a <> ")"
    op2 op a b = "(" <> op <> " " <> wordToSMT a <> " " <> wordToSMT b <> ")"

-- | Encodes a Byte into smt
byteToSMT :: Expr Byte -> String
byteToSMT = \case
  LitByte b -> "#x" <> (strip0x' . show $ (num b :: W256))
  IndexWord w idx -> "((_ extract " <> wordToSMT (add idx (Lit 7)) <> " " <> wordToSMT idx  <> ") " <> wordToSMT w <> ")"
  ReadByte idx src -> "(select " <> bufToSMT src <> " " <> wordToSMT idx <> ")"

-- | Encodes a Buf into smt
bufToSMT :: Expr Buf -> String
bufToSMT = \case
  EmptyBuf -> "((as const (Array (_ BitVec 256) (_ BitVec 8))) 0)"
  ConcreteBuf bs -> error "TODO: concreteBuf"
  AbstractBuf s -> s
  WriteByte idx val prev -> "(store " <> bufToSMT prev <> wordToSMT idx <> " " <> byteToSMT val <> ")"
  WriteWord idx val prev -> writeWord idx val prev
  CopySlice dstIdx srcIdx size src dst -> copySlice dstIdx srcIdx size src dst

-- | Encodes storage into smt
storageToSMT :: Expr Storage -> String
storageToSMT = \case
  EmptyStore -> "((as const (Array (_ BitVec 256) (Array (_ BitVec 256) (_ BitVec 256)))) 0)"
  ConcreteStore s -> error "TODO: concretestore"
  AbstractStore -> "storage"
  SStore addr idx val store -> "(store " <> storageToSMT store <> " " <> wordToSMT addr <> "(store (select " <> wordToSMT addr <> " " <> storageToSMT store <> ") " <> wordToSMT idx <> " " <> wordToSMT val <> "))a"



-- ** Helpers ** ---------------------------------------------------------------------------------


writeWord :: Expr EWord -> Expr EWord -> Expr Buf -> String
writeWord idx val buf = go buf 31
  where
    -- TODO: endianess?
    go :: Expr Buf -> Int -> String
    go b n
      | n == 0 = "(store " <> bufToSMT b  <> " " <> wordToSMT idx <> byteToSMT (IndexWord val (Lit 0)) <> ")"
      | otherwise = "(store (go b (n - 1)) (idx + n) (IndexWord val n))"

-- | Stores a region of src into dst
copySlice :: Expr EWord -> Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf -> String
copySlice dstOffset srcOffset size@(Lit _) src dst
  | size == (Lit 0) = "(store " <> bufToSMT dst <> " " <> wordToSMT dstOffset <> "(select " <> wordToSMT srcOffset <> " " <> bufToSMT src <> "))"
  | otherwise = "(store " <> copySlice dstOffset srcOffset (sub size (Lit 1)) src dst <> "(+ " <> wordToSMT dstOffset <> " " <> wordToSMT size <> ") (select (+ " <> wordToSMT srcOffset <> " " <> wordToSMT size <> ") " <> bufToSMT src <> ")"
copySlice _ _ _ _ _ = error "TODO: implement copySlice with a symbolically sized region"

-- | Unrolls an exponentiation into a series of multiplications
expandExp :: Expr EWord -> W256 -> String
expandExp base expnt
  | expnt == 1 = wordToSMT base
  | otherwise = "(* " <> wordToSMT base <> " " <> expandExp base (expnt - 1) <> ")"

-- | Concatenates a list of bytes into a larger bitvector
concatBytes :: [Expr Byte] -> String
concatBytes [b] = byteToSMT b
concatBytes (hd : tl) = "(concat " <> byteToSMT hd <> " " <> concatBytes tl <> ")"
concatBytes [] = error "cannot concat an empty list of bytes" -- TODO: use nonempty here?
