{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

{-|
Module      : SMT2.Type
Description : (Partial) typechecking routines for SMT2 expressions
-}
module SMT2.Type (
  infer
) where

import Prelude hiding (GT, LT)
import Data.Kind
import Data.Typeable hiding (typeRep)
import Type.Reflection (typeRep)
import Control.Monad
import Data.List (find)
import Data.Maybe

import Text.Parsec

import SMT2.Syntax.Typed
import qualified SMT2.Syntax.Untyped as U

data Entry :: Ty -> Type where
  Entry :: String -> STy ty -> Entry ty

type Err a = Either String a

-- | Attempts to construct an expression with the type required by the caller.
-- If this is impossible, an error is thrown instead.
infer :: forall e a . (Typeable a) => U.Exp -> Err (Exp a)
infer = \case
  U.LitInt  i -> check <*> (LitInt <$> Right i)
  U.LitBool b -> check <*> (LitBool <$> Right b)
  U.Var     s -> Var <$> Right s

  -- core
  U.And  args -> check <*> (And  <$> (mapM infer args))
  U.Or   args -> check <*> (Or   <$> (mapM infer args))
  U.Eq   args -> polycheck Eq args
  U.Xor  args -> check <*> (Xor  <$> (mapM infer args))
  U.Impl args -> check <*> (Impl <$> (mapM infer args))
  U.Distinct args -> check <*> (Distinct <$> (mapM infer args))
  U.ITE c l r -> liftM3 ITE (infer c) (infer l) (infer r)

  -- integer
  U.Neg i -> check <*> (Neg <$> (infer i))
  U.Sub i j -> check <*> (liftM2 Sub (infer i) (infer j))
  U.Add i j -> check <*> (liftM2 Add (infer i) (infer j))
  U.Mul i j -> check <*> (liftM2 Mul (infer i) (infer j))
  U.Div i j -> check <*> (liftM2 Div (infer i) (infer j))
  U.Mod i j -> check <*> (liftM2 Mod (infer i) (infer j))
  U.Abs i -> check <*> (fmap Abs (infer i))
  U.LEQ i j -> check <*> (liftM2 LEQ (infer i) (infer j))
  U.LT i j -> check <*> (liftM2 LT (infer i) (infer j))
  U.GEQ i j -> check <*> (liftM2 GEQ (infer i) (infer j))
  U.GT i j -> check <*> (liftM2 GT (infer i) (infer j))
  where
    -- Returns an error if @x@ and @a@ do not have the same type
    check :: forall x . Typeable x => Err (Exp x -> Exp a)
    check = case eqT @a @x of
      Just Refl -> Right id
      Nothing   -> Left ("Type mismatch. Expected " <> show (typeRep @a) <> ", got " <> show (typeRep @x))

    -- Takes a polymorphic AST constructor and specializes it to each of
    -- our types. Those specializations are used in order to guide the
    -- typechecking of the supplied expressions. Returns at first success.
    polycheck :: Typeable x => (forall (y :: Ty). (Typeable y) => [Exp y] -> Exp x) -> [U.Exp] -> Err (Exp a)
    polycheck cons es = fromMaybe (error $ "Internal error: Type.polycheck. Exprs: " <> show es) $
      findSuccess
      [ check <*> (cons @'Integer <$> mapM infer es)
      , check <*> (cons @'Boolean <$> mapM infer es)
      ]

-- | Runs through a list of error-prone computations and returns the first successful one.
findSuccess :: [Err a] -> Maybe (Err a)
findSuccess = find valid
  where
    valid (Left _)   = False
    valid (Right _)  = True
