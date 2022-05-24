{-# LANGUAGE DeriveDataTypeable #-}

{-|
Module      : SMT2.Syntax.Untyped
Description : Untyped SMT2 expression AST
-}
module SMT2.Syntax.Untyped where

import GHC.Natural
import Data.Typeable
import Data.Data

data Exp
  = LitStr String
  | LitInt Natural
  | LitBool Bool
  | Var String
  | App String [Exp]
  | And [Exp]
  | Or [Exp]
  | Eq [Exp]
  | Xor [Exp]
  | Impl [Exp]
  | Distinct [Exp]
  | ITE Exp Exp Exp
  | Concat Exp Exp
  | Extract Natural Natural Exp
  | BVNot Exp
  | BVNeg Exp
  | BVAnd Exp Exp
  | BVOr Exp Exp
  | BVAdd Exp Exp
  | BVMul Exp Exp
  | BVUDiv Exp Exp
  | BVURem Exp Exp
  | BVShl Exp Exp
  | BVShr Exp Exp
  | BVULt Exp Exp
  | Neg Exp
  | Sub Exp Exp
  | Add Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Mod Exp Exp
  | Abs Exp
  | LEQ Exp Exp
  | LT Exp Exp
  | GEQ Exp Exp
  | GT Exp Exp
  | Divisible Natural Exp
  | Select Exp Exp
  | Store Exp Exp Exp
  deriving (Show, Eq, Typeable, Data)

