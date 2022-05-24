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
module SMT2.Type where

import Data.Kind
import Data.Typeable hiding (typeRep)
import Type.Reflection (typeRep)


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
  U.And  args -> check <*> (And <$> (mapM infer args))
  U.Or   args -> check <*> (Or <$> (mapM infer args))
  where
    -- Returns an error if @x@ and @a@ do not have the same type
    check :: forall x . Typeable x => Err (Exp x -> Exp a)
    check = case eqT @a @x of
      Just Refl -> Right id
      Nothing   -> Left ("Type mismatch. Expected " <> show (typeRep @a) <> ", got " <> show (typeRep @x))

