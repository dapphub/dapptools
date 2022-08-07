{-# Language DataKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language LambdaCase #-}
{-# Language QuasiQuotes #-}

{- |
    Module: EVM.SMT
    Description: Utilities for building and executing SMT queries from Expr instances
-}
module EVM.SMT where

import Prelude hiding (LT, GT)

import GHC.Natural
import Control.Monad
import GHC.IO.Handle (Handle, hGetLine, hPutStr, hFlush, hSetBuffering, BufferMode(..))
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent (forkIO, killThread)
import Data.List (singleton)
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)

import Data.String.Here
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (createProcess, cleanupProcess, proc, ProcessHandle, std_in, std_out, std_err, StdStream(..))

import EVM.Types
import EVM.Expr hiding (copySlice, writeWord, op1, op2, op3, drop)


-- ** Encoding ** ----------------------------------------------------------------------------------


newtype SMT2 = SMT2 [Text]
  deriving (Eq, Show, Semigroup, Monoid)

formatSMT2 :: SMT2 -> Text
formatSMT2 (SMT2 ls) = T.unlines ls

assertWord :: Expr EWord -> SMT2
assertWord e = prelude
            <> (declareBufs $ referencedBufs e)
            <> SMT2 [""]
            <> (declareVars $ referencedVars e)
            <> SMT2 [""]
            <> (declareFrameContext $ referencedFrameContext e)
            <> SMT2 [""]
            <> (SMT2 . singleton $ "(assert (= " <> exprToSMT (Lit 1) <> " " <> exprToSMT e <> ")")

assertWords :: [Expr EWord] -> SMT2
assertWords es = prelude
            <> (declareBufs . nubOrd $ foldl (<>) [] (fmap (referencedBufs) es))
            <> SMT2 [""]
            <> (declareVars . nubOrd $ foldl (<>) [] (fmap (referencedVars) es))
            <> SMT2 [""]
            <> (declareFrameContext . nubOrd $ foldl (<>) [] (fmap (referencedFrameContext) es))
            <> SMT2 [""]
            <> (SMT2 $ fmap (\e -> "(assert (= " <> exprToSMT (Lit 1) <> " " <> exprToSMT e <> "))") es)

assertProps :: [Prop] -> SMT2
assertProps ps = prelude
            <> (declareBufs . nubOrd $ foldl (<>) [] (fmap (referencedBufs') ps))
            <> SMT2 [""]
            <> (declareVars . nubOrd $ foldl (<>) [] (fmap (referencedVars') ps))
            <> SMT2 [""]
            <> (declareFrameContext . nubOrd $ foldl (<>) [] (fmap (referencedFrameContext') ps))
            <> SMT2 [""]
            <> (SMT2 $ fmap (\p -> "(assert " <> propToSMT p <> ")") ps)

prelude :: SMT2
prelude = SMT2 . fmap (T.drop 2) . T.lines $ [i|
  ; types
  (define-sort Byte () (_ BitVec 8))
  (define-sort Word () (_ BitVec 256))
  (define-sort Buf () (Array Word Byte))
  (define-sort Storage () (Array Word (Array Word Word)))

  ; hash functions
  (declare-fun keccak (Buf) Word)
  (declare-fun sha256 (Buf) Word)

  ; word indexing
  (define-fun indexWord0 ((w Word)) Byte ((_ extract 7 0) w))
  (define-fun indexWord1 ((w Word)) Byte ((_ extract 15 8) w))
  (define-fun indexWord2 ((w Word)) Byte ((_ extract 23 16) w))
  (define-fun indexWord3 ((w Word)) Byte ((_ extract 31 24) w))
  (define-fun indexWord4 ((w Word)) Byte ((_ extract 39 32) w))
  (define-fun indexWord5 ((w Word)) Byte ((_ extract 47 40) w))
  (define-fun indexWord6 ((w Word)) Byte ((_ extract 55 48) w))
  (define-fun indexWord7 ((w Word)) Byte ((_ extract 63 56) w))
  (define-fun indexWord8 ((w Word)) Byte ((_ extract 71 64) w))
  (define-fun indexWord9 ((w Word)) Byte ((_ extract 79 72) w))
  (define-fun indexWord10 ((w Word)) Byte ((_ extract 87 80) w))
  (define-fun indexWord11 ((w Word)) Byte ((_ extract 95 88) w))
  (define-fun indexWord12 ((w Word)) Byte ((_ extract 103 96) w))
  (define-fun indexWord13 ((w Word)) Byte ((_ extract 111 104) w))
  (define-fun indexWord14 ((w Word)) Byte ((_ extract 119 112) w))
  (define-fun indexWord15 ((w Word)) Byte ((_ extract 127 120) w))
  (define-fun indexWord16 ((w Word)) Byte ((_ extract 135 128) w))
  (define-fun indexWord17 ((w Word)) Byte ((_ extract 143 136) w))
  (define-fun indexWord18 ((w Word)) Byte ((_ extract 151 144) w))
  (define-fun indexWord19 ((w Word)) Byte ((_ extract 159 152) w))
  (define-fun indexWord20 ((w Word)) Byte ((_ extract 167 160) w))
  (define-fun indexWord21 ((w Word)) Byte ((_ extract 175 168) w))
  (define-fun indexWord22 ((w Word)) Byte ((_ extract 183 176) w))
  (define-fun indexWord23 ((w Word)) Byte ((_ extract 191 184) w))
  (define-fun indexWord24 ((w Word)) Byte ((_ extract 199 192) w))
  (define-fun indexWord25 ((w Word)) Byte ((_ extract 207 200) w))
  (define-fun indexWord26 ((w Word)) Byte ((_ extract 215 208) w))
  (define-fun indexWord27 ((w Word)) Byte ((_ extract 223 216) w))
  (define-fun indexWord28 ((w Word)) Byte ((_ extract 231 224) w))
  (define-fun indexWord29 ((w Word)) Byte ((_ extract 239 232) w))
  (define-fun indexWord30 ((w Word)) Byte ((_ extract 247 240) w))
  (define-fun indexWord31 ((w Word)) Byte ((_ extract 255 248) w))

  ; buffers
  (declare-fun bufLength (Buf) Word)
  (define-const emptyBuf (Buf) ((as const Buf) #b00000000))

  (define-fun readWord ((idx Word) (buf Buf)) Word
    (concat (select buf idx)
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000001))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000002))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000003))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000004))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000005))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000006))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000007))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000008))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000009))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000a))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000b))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000c))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000d))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000e))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000f))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000010))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000011))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000012))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000013))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000014))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000015))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000016))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000017))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000018))
    (concat (select buf (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000019))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001a))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001b))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001c))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001d))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001e))
    (concat (select buf (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001f))
    ))))))))))))))))))))))))))))))))
  )

  (define-fun writeWord ((idx Word) (val Word) (buf Buf)) Buf
      (store (store (store (store (store (store (store (store (store (store (store (store (store (store (store (store (store
      (store (store (store (store (store (store (store (store (store (store (store (store (store (store (store (store (store buf
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001f) (indexWord31 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001e) (indexWord30 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001d) (indexWord29 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001c) (indexWord28 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001b) (indexWord27 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000001a) (indexWord26 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000019) (indexWord25 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000018) (indexWord24 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000017) (indexWord23 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000016) (indexWord22 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000015) (indexWord21 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000014) (indexWord20 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000013) (indexWord19 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000012) (indexWord18 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000011) (indexWord17 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000010) (indexWord16 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000f) (indexWord15 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000e) (indexWord14 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000d) (indexWord13 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000c) (indexWord12 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000b) (indexWord11 val))
      (bvadd idx #x000000000000000000000000000000000000000000000000000000000000000a) (indexWord10 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000009) (indexWord9 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000008) (indexWord8 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000007) (indexWord7 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000006) (indexWord6 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000005) (indexWord5 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000005) (indexWord5 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000004) (indexWord4 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000003) (indexWord3 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000002) (indexWord2 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000001) (indexWord1 val))
      (bvadd idx #x0000000000000000000000000000000000000000000000000000000000000001) (indexWord1 val))
      idx (indexWord0 val))
  )

  ; block context
  (declare-fun blockhash (Word) Word)
  (declare-const origin Word)
  (declare-const coinbase Word)
  (declare-const timestamp Word)
  (declare-const blocknumber Word)
  (declare-const difficulty Word)
  (declare-const gaslimit Word)
  (declare-const chainid Word)
  (declare-const basefee Word)

  ; storage
  (declare-const abstractStore Storage)
  (define-const emptyStore Storage ((as const Storage) ((as const (Array (_ BitVec 256) (_ BitVec 256))) #x0000000000000000000000000000000000000000000000000000000000000000)))

  (define-fun sstore ((addr Word) (key Word) (val Word) (storage Storage)) Storage (
      store storage addr (store (select storage addr) key val)))

  (define-fun sload ((addr Word) (key Word) (storage Storage)) Word (
      select (select storage addr) key))
  |]

declareBufs :: [Text] -> SMT2
declareBufs names = SMT2 $ ["; buffers"] <> fmap declare names
  where
    declare n = "(declare-const " <> n <> " (Array (_ BitVec 256) (_ BitVec 8)))"

referencedBufs :: Expr a -> [Text]
referencedBufs expr = nubOrd (foldExpr go [] expr)
  where
    go :: Expr a -> [Text]
    go = \case
      AbstractBuf s -> [s]
      _ -> []

referencedBufs' :: Prop -> [Text]
referencedBufs' = \case
  (PEq a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PLT a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PGT a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PLEq a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PGEq a b) -> nubOrd $ referencedBufs a <> referencedBufs b

referencedVars' :: Prop -> [Text]
referencedVars' = \case
  (PEq a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PLT a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PGT a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PLEq a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PGEq a b) -> nubOrd $ referencedVars a <> referencedVars b

referencedFrameContext' :: Prop -> [Text]
referencedFrameContext' = \case
  (PEq a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PLT a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PGT a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PLEq a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PGEq a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b

declareVars :: [Text] -> SMT2
declareVars names = SMT2 $ ["; variables"] <> fmap declare names
  where
    declare n = "(declare-const " <> n <> " (_ BitVec 256))"

referencedVars :: Expr a -> [Text]
referencedVars expr = nubOrd (foldExpr go [] expr)
  where
    go :: Expr a -> [Text]
    go = \case
      Var s -> [s]
      _ -> []

declareFrameContext :: [Text] -> SMT2
declareFrameContext names = SMT2 $ ["; frame context"] <> fmap declare names
  where
    declare n = "(declare-const " <> n <> " (_ BitVec 256))"

referencedFrameContext :: Expr a -> [Text]
referencedFrameContext expr = nubOrd (foldExpr go [] expr)
  where
    go :: Expr a -> [Text]
    go = \case
      CallValue a -> [T.append "callvalue_" (T.pack . show $ a)]
      Caller a -> [T.append "caller_" (T.pack . show $ a)]
      Address a -> [T.append "address_" (T.pack . show $ a)]
      Balance {} -> error "TODO: BALANCE"
      SelfBalance {} -> error "TODO: SELFBALANCE"
      Gas {} -> error "TODO: GAS"
      _ -> []

exprToSMT :: Expr a -> Text
exprToSMT = \case
  Lit w -> "#x" <> (T.pack . padLeftStr 64 . strip0x' . show $ w)
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
  Sub a b -> op2 "bvsub" a b
  Mul a b -> op2 "bvmul" a b
  Div a b -> op2 "bvudiv" a b
  Exp a b -> case b of
               Lit b' -> expandExp a b'
               _ -> error "cannot encode symbolic exponentation into SMT"
  Min a b -> "(ite (<= " <> exprToSMT a <> " " <> exprToSMT b <> ") " <> exprToSMT a <> " " <> exprToSMT b <> ")"

  LT a b -> "(ite " <> op2 "bvult" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  SLT a b -> "(ite " <> op2 "bvslt" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  GT a b -> "(ite " <> op2 "bvugt" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  LEq a b -> exprToSMT $ Not (LT b a)
  GEq a b -> exprToSMT $ Not (LT a b)
  Eq a b -> "(ite " <> op2 "=" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  IsZero a -> "(ite (= " <> exprToSMT a <> " " <> exprToSMT (Lit 0) <> ") " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"

  And a b -> op2 "bvand" a b
  Or a b -> op2 "bvor" a b
  Xor a b -> exprToSMT $ And (Or a b) (Not (And a b))
  Not a -> op1 "bvnot" a
  SHL a b -> op2 "bvshl" a b
  SHR a b -> op2 "bvlshr" b a -- TODO: is lshr the same as shr?
  EqByte a b -> "(= " <> exprToSMT a  <> " " <> exprToSMT b <> ")"

  Keccak a -> "(keccak " <> exprToSMT a <> ")"
  SHA256 a -> "(sha256 " <> exprToSMT a <> ")"

  CallValue a -> T.append "callvalue_" (T.pack . show $ a)
  Caller a -> T.append "caller_" (T.pack . show $ a)
  Address a -> T.append "address_" (T.pack . show $ a)

  Origin -> "origin"
  BlockHash a -> "(blockhash " <> exprToSMT a <> ")"
  Coinbase -> "coinbase"
  Timestamp -> "timestamp"
  BlockNumber -> "blocknumber"
  Difficulty -> "difficulty"
  GasLimit -> "gaslimit"
  ChainId -> "chainid"
  BaseFee -> "basefee"

  -- TODO: make me binary...
  LitByte b -> "#x" <> (T.pack . strip0x' . show $ (num b :: W256))
  IndexWord w idx -> case idx of
    Lit n -> if n >= 0 && n < 32
             then "(indexWord" <> (T.pack . show $ n) <> " " <> exprToSMT w <> ")"
             else error $ "indexWord: unsupported index: " <> show n
    n -> error $ "indexWord: unsupported index: " <> show n
  ReadByte idx src -> op2 "select" src idx

  EmptyBuf -> "emptyBuf"
  ConcreteBuf bs -> error "TODO: concreteBuf"
  AbstractBuf s -> s
  WriteByte idx val prev -> op3 "store" prev idx val
  WriteWord idx val prev -> op3 "writeWord" idx val prev
  ReadWord idx prev -> op2 "readWord" idx prev
  BufLength b -> op1 "bufLength" b
  CopySlice dstIdx srcIdx size src dst -> copySlice dstIdx srcIdx size src dst

  EmptyStore -> "emptyStore"
  ConcreteStore s -> error "TODO: concretestore"
  AbstractStore -> "abstractStore"
  SStore addr idx val store -> op4 "sstore" addr idx val store
  SLoad addr idx store -> op3 "sload" addr idx store

  a -> error $ "TODO: implement: " <> show a
  where
    a `sp` b = a <> " " <> b
    op1 op a = "(" <> op `sp` exprToSMT a <> ")"
    op2 op a b = "(" <> op `sp` exprToSMT a `sp` exprToSMT b <> ")"
    op3 op a b c = "(" <> op `sp` exprToSMT a `sp` exprToSMT b `sp` exprToSMT c <> ")"
    op4 op a b c d = "(" <> op `sp` exprToSMT a `sp` exprToSMT b `sp` exprToSMT c `sp` exprToSMT d <> ")"

propToSMT :: Prop -> Text
propToSMT = \case
    PEq a b -> "(= " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PLT a b -> "(< " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PGT a b -> "(> " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PLEq a b -> "(<= " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PGEq a b -> "(>= " <> exprToSMT a <> " " <> exprToSMT b <> ")"


-- ** Execution ** -------------------------------------------------------------------------------


-- | Supported solvers
data Solver
  = Z3
  | CVC5
  | Bitwuzla
  | Custom Text

instance Show Solver where
  show Z3 = "z3"
  show CVC5 = "cvc5"
  show Bitwuzla = "bitwuzla"
  show (Custom s) = T.unpack s


-- | A running solver instance
data SolverInstance = SolverInstance
  { _type :: Solver
  , _stdin :: Handle
  , _stdout :: Handle
  , _stderr :: Handle
  , _process :: ProcessHandle
  }

-- | A channel representing a group of solvers
newtype SolverGroup = SolverGroup (Chan Task)

-- | A script to be executed, a list of models to be extracted in the case of a sat result, and a channel where the result should be written
data Task = Task
  { script :: SMT2
  , models :: [Text]
  , resultChan :: Chan CheckSatResult
  }

-- | The result of a call to (check-sat)
data CheckSatResult
  = Sat [Text]
  | Unsat
  | Unknown
  | Error Text
  deriving (Show)

isSat :: CheckSatResult -> Bool
isSat (Sat _) = True
isSat _ = False

isErr :: CheckSatResult -> Bool
isErr (Error _) = True
isErr _ = False

checkSat' :: SolverGroup -> (SMT2, [Text]) -> IO (CheckSatResult)
checkSat' (SolverGroup taskQueue) (script, models) = do
  -- prepare tasks
  res <- newChan
  let task = Task script models res

  -- send tasks to solver group
  writeChan taskQueue task

  -- collect results
  readChan res

checkSat :: SolverGroup -> [(SMT2, [Text])] -> IO [(SMT2, CheckSatResult)]
checkSat (SolverGroup taskQueue) scripts = do
  -- prepare tasks
  tasks <- forM scripts $ \(s, ms) -> do
    res <- newChan
    pure $ Task s ms res

  -- send tasks to solver group
  forM_ tasks (writeChan taskQueue)

  -- collect results
  forM tasks $ \(Task s _ r) -> do
    res <- readChan r
    pure (s, res)


withSolvers :: Solver -> Natural -> (SolverGroup -> IO a) -> IO a
withSolvers solver count cont = do
  -- spawn solvers
  instances <- mapM (const $ spawnSolver solver) [1..count]

  -- spawn orchestration thread
  taskQueue <- newChan
  availableInstances <- newChan
  forM_ instances (writeChan availableInstances)
  orchestrateId <- forkIO $ orchestrate taskQueue availableInstances

  -- run continuation with task queue
  res <- cont (SolverGroup taskQueue)

  -- cleanup and return results
  mapM_ stopSolver instances
  killThread orchestrateId
  pure res
  where
    orchestrate queue avail = do
      task <- readChan queue
      inst <- readChan avail
      _ <- forkIO $ runTask task inst avail
      orchestrate queue avail

    runTask (Task s@(SMT2 cmds) ms r) inst availableInstances = do
      -- reset solver and send all lines of provided script
      out <- sendScript inst (SMT2 $ "(reset)" : cmds)
      case out of
        -- if we got an error then return it
        Left e -> writeChan r (Error e)
        -- otherwise call (check-sat), parse the result, and send it down the result channel
        Right () -> do
          sat <- sendLine inst "(check-sat)"
          res <- case sat of
            "sat" -> do
              models <- forM ms $ \m -> do
                getValue inst m
              pure $ Sat models
            "unsat" -> pure Unsat
            "timeout" -> pure Unknown
            "unknown" -> pure Unknown
            _ -> pure . Error $ "Unable to parse solver output: " <> sat
          writeChan r res

      -- put the instance back in the list of available instances
      writeChan availableInstances inst


-- | Arguments used when spawing a solver instance
solverArgs :: Solver -> [Text]
solverArgs = \case
  Bitwuzla -> error "TODO: Bitwuzla args"
  Z3 ->
    [ "-in" ]
  CVC5 ->
    [ "--lang=smt"
    , "--interactive"
    , "--no-interactive-prompt"
    , "--produce-models"
    ]
  Custom _ -> []

-- | Spawns a solver instance, and sets the various global config options that we use for our queries
spawnSolver :: Solver -> IO SolverInstance
spawnSolver solver = do
  let cmd = (proc (show solver) (fmap T.unpack $ solverArgs solver)) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just stdin, Just stdout, Just stderr, process) <- createProcess cmd
  hSetBuffering stdin (BlockBuffering (Just 1000000))
  let solverInstance = SolverInstance solver stdin stdout stderr process
  --_ <- sendCommand solverInstance "(set-option :print-success true)"
  pure solverInstance

-- | Cleanly shutdown a running solver instnace
stopSolver :: SolverInstance -> IO ()
stopSolver (SolverInstance _ stdin stdout stderr process) = cleanupProcess (Just stdin, Just stdout, Just stderr, process)

-- | Sends a list of commands to the solver. Returns the first error, if there was one.
sendScript :: SolverInstance -> SMT2 -> IO (Either Text ())
sendScript solver (SMT2 cmds) = do
  putStrLn "sending script"
  sendLine' solver (T.unlines cmds)
  putStrLn "sent script"
  pure $ Right()

-- | Sends a single command to the solver, returns the first available line from the output buffer
sendCommand :: SolverInstance -> Text -> IO Text
sendCommand inst cmd = do
  -- trim leading whitespace
  let cmd' = T.dropWhile isSpace cmd
  case T.unpack cmd' of
    "" -> pure "success"      -- ignore blank lines
    ';' : _ -> pure "success" -- ignore comments
    _ -> sendLine inst cmd'

-- | Sends a string to the solver and appends a newline, returns the first available line from the output buffer
sendLine :: SolverInstance -> Text -> IO Text
sendLine (SolverInstance _ stdin stdout _ _) cmd = do
  hPutStr stdin (T.unpack $ T.append cmd "\n")
  hFlush stdin
  T.pack <$> hGetLine stdout

-- | Sends a string to the solver and appends a newline, doesn't return stdout
sendLine' :: SolverInstance -> Text -> IO ()
sendLine' (SolverInstance _ stdin _ _ _) cmd = do
  hPutStr stdin (T.unpack $ T.append cmd "\n")
  hFlush stdin

-- | Returns a string representation of the model for the requested variable
getValue :: SolverInstance -> Text -> IO Text
getValue (SolverInstance _ stdin stdout _ _) var = do
  hPutStr stdin (T.unpack $ T.append (T.append "(get-value (" var) "))\n")
  hFlush stdin
  T.pack <$> fmap (unlines . reverse) (readSExpr stdout)

-- | Reads lines from h until we have a balanced sexpr
readSExpr :: Handle -> IO [String]
readSExpr h = go 0 0 []
  where
    go :: Int -> Int -> [String] -> IO [String]
    go 0 0 _ = do
      line <- hGetLine h
      let ls = length $ filter (== '(') line
          rs = length $ filter (== ')') line
      if ls == rs
         then pure [line]
         else go ls rs [line]
    go ls rs prev = do
      line <- hGetLine h
      let ls' = length $ filter (== '(') line
          rs' = length $ filter (== ')') line
      if (ls + ls') == (rs + rs')
         then pure $ line : prev
         else go (ls + ls') (rs + rs') (line : prev)



-- ** Helpers ** ---------------------------------------------------------------------------------


-- | Stores a region of src into dst
copySlice :: Expr EWord -> Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf -> Text
copySlice dstOffset srcOffset size@(Lit _) src dst
  | size == (Lit 0) = "(store " <> exprToSMT dst <> " " <> exprToSMT dstOffset <> "(select " <> exprToSMT src<> " " <> exprToSMT srcOffset <> "))"
  | otherwise = "(store " <> copySlice dstOffset srcOffset (sub size (Lit 1)) src dst <> "(bvadd " <> exprToSMT dstOffset <> " " <> exprToSMT size <> ") (select " <> exprToSMT src <> "(bvadd " <> exprToSMT srcOffset <> " " <> exprToSMT size <> ")))"
copySlice _ _ _ _ _ = error "TODO: implement copySlice with a symbolically sized region"

-- | Unrolls an exponentiation into a series of multiplications
expandExp :: Expr EWord -> W256 -> Text
expandExp base expnt
  | expnt == 1 = exprToSMT base
  | otherwise = "(* " <> exprToSMT base <> " " <> expandExp base (expnt - 1) <> ")"

-- | Concatenates a list of bytes into a larger bitvector
concatBytes :: [Expr Byte] -> Text
concatBytes [b] = exprToSMT b
concatBytes (hd : tl) = "(concat " <> exprToSMT hd <> " " <> concatBytes tl <> ")"
concatBytes [] = error "cannot concat an empty list of bytes" -- TODO: use nonempty here?
