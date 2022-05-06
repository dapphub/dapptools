{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module SMT2.Type where

import Data.Kind
import Data.Typeable

import Data.Parameterized.List

import SMT2.Syntax.Typed
import SMT2.Parse
import qualified SMT2.Syntax.Untyped as U

data Entry :: Ty -> Type where
  Entry :: String -> STy ty -> Entry ty

type Env e = List Entry e
type Err a = Either String a

check :: Env e -> U.Exp -> Either String (Exp a)
check env = \case
  U.And args -> Right . And <$> traverse (infer env) args



-- | Attempts to construct an expression with the type
-- required by the caller. If this is impossible, an
-- error is thrown instead.
infer :: forall e a . (Typeable a) => Env e -> U.Exp -> Err (Exp a)
infer env = \case
  U.LitInt  i -> check <*> (Lit <$> Right i)
  U.LitBool b -> check <*> (Lit <$> Right b)
  U.And  args -> check <*> (And <$> (mapM (infer env) args))
  U.Or   args -> check <*> (Or <$> (mapM (infer env) args))
  where
    -- Returns an error if @x@ and @a@ do not have the same type
    check :: forall x . Typeable x => Err (Exp x -> Exp a)
    check = case eqT @a @x of
      Just Refl -> Right id
      Nothing   -> Left "Type mismatch"



test2 :: Err (Exp Boolean)
test2 = infer Nil p
  where
    Right p = test4
