{-# Language OverloadedStrings #-}
{-# Language ViewPatterns #-}
{-# Language ScopedTypeVariables #-}
{-# Language LambdaCase #-}
{-# Language QuasiQuotes #-}
{-# Language TypeSynonymInstances #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DataKinds #-}
{-# Language StandaloneDeriving #-}

module Main where

import Data.Text (Text)
import Data.ByteString (ByteString)

import Prelude hiding (fail)

import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS (fromStrict, toStrict)
import qualified Data.ByteString.Base16 as Hex
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import Control.Monad.State.Strict (execState, runState)
import Control.Lens hiding (List, pre, (.>))

import qualified Data.Vector as Vector
import Data.String.Here

import Control.Monad.Fail

import Data.Binary.Put (runPut)
import Data.SBV hiding ((===), forAll, sList)
import Data.SBV.Control
import qualified Data.Map as Map
import Data.Binary.Get (runGetOrFail)

import EVM hiding (Query)
import EVM.SymExec
import EVM.ABI
import EVM.Exec
import qualified EVM.Patricia as Patricia
import EVM.Precompiled
import EVM.RLP
import EVM.Solidity
import EVM.Types
import EVM.Expr
import EVM.ExprSimp

instance MonadFail Query where
    fail = io . fail

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties"
  [ testProperty "toExpr . fromExpr" prop_ExprConversionIsomorphism
  , testProperty "parse . show" prop_ExprParsePrintIsomorphism
  ]

-- Test to and from
-- quickcheck passes this, yay
-- test this with `quickCheck prop_ExprConversionIsomorphism`
prop_ExprConversionIsomorphism :: Expr -> Bool
prop_ExprConversionIsomorphism e = toExpr (fromExpr e) == e


-- Test
-- quickcheck passes this, yay
-- test this with `quickCheck prop_ExprParsePrintIsomorphism`
prop_ExprParsePrintIsomorphism :: Expr -> Bool
prop_ExprParsePrintIsomorphism e =
  case parse pScheme "" (pack $ show e) of
    Left _   -> False
    Right e' -> e == e'
