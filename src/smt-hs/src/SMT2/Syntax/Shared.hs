{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : SMT2.Syntax.Shared
Description : AST data types shared between the typed and untyped AST definitions

This module only exists to increase code reuse; the types defined here won't
be used directly, but will be instantiated with different a Typing parameter
at different steps in the frontend pipeline. This way we don't have to
update mirrored types, instances and functions in lockstep.
-}

module SMT2.Syntax.Shared (module SMT2.Syntax.Shared) where


data Typing = Typed | Untyped

