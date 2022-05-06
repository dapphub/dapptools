{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module SMT2.Ref where

import Data.Proxy

-- | Wrapper type for name references.
--
-- A Ref can be constructed at runtime using the `declare` smart constructor, or at compile time using the `asRef` smart constructor.
data Ref (a :: Ty) where
  Ref :: String -> STy a -> Ref a

deriving instance (Show (Ref t))

-- | Construct a Ref from a statically known string
asRef :: forall nm a . KnownSymbol nm => STy a -> Ref a
asRef = Ref (symbolVal (Proxy @nm))

