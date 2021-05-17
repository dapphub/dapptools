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

import Data.Text (pack)

import Prelude hiding (fail)

import Text.Megaparsec hiding (State)

import Test.Tasty
import Test.Tasty.QuickCheck

import EVM.Expr


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
