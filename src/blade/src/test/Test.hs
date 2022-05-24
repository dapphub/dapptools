{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding (GT, LT)
import Control.Monad
import Debug.Trace

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Text.Parsec

import SMT2.Syntax.Typed
import SMT2.Parse

trace' x = trace (show x) x

main :: IO ()
main = defaultMain $ testGroup "blade"
  [ testGroup "roundtrips"
      [ testProperty "valid smt" $ do
          generated <- trace' <$> sized genScript
          let parsed = parse script "" (show generated)
          return $ case parsed of
            Left e -> trace (show e) $ property False
            Right s -> s === generated

        --testProperty "invalid smt" undefined
      ]
  , testGroup "smt-comp benchmarks"
      [
      ]
  -- these should all produce compile time errors if there is a failure
  , testGroup "quasiquoter"
      [ testCase "no newline" $ qtest [smt2|(assert true)|]
      , testCase "leading newline" $ qtest [smt2|
          (assert true)|]
      , testCase "trailing newline" $ qtest [smt2|
          (assert true)
        |]
      ]
  ]


genExpBool :: Int -> Gen (Exp Boolean)
genExpBool 0 = oneof
  [ LitBool <$> arbitrary
  , Var <$> arbitrary
  ]
genExpBool n = oneof
  [ fmap And (listOf subExpBool)
  , fmap Or (listOf subExpBool)
  , fmap Eq (listOf subExpBool)
  , fmap Xor (listOf subExpBool)
  , fmap Impl (listOf subExpBool)
  , fmap Distinct (listOf subExpBool)
  , liftM3 ITE subExpBool subExpBool subExpBool
  , liftM2 LEQ subExpInt subExpInt
  , liftM2 GEQ subExpInt subExpInt
  , liftM2 LT subExpInt subExpInt
  , liftM2 GT subExpInt subExpInt
  ]
  where
    subExpBool = genExpBool (n `div` 2)
    subExpInt = genExpInt (n `div` 2)

genExpInt :: Int -> Gen (Exp 'Integer)
genExpInt 0 = oneof
  [ LitInt <$> arbitrary
  , Var <$> arbitrary
  ]
genExpInt n = oneof
  [ fmap Neg subExpInt
  , liftM2 Sub subExpInt subExpInt
  , liftM2 Add subExpInt subExpInt
  , liftM2 Mul subExpInt subExpInt
  , liftM2 Div subExpInt subExpInt
  , liftM2 Mod subExpInt subExpInt
  , fmap Abs subExpInt
  ]
  where
    subExpInt = genExpInt (n `div` 2)

genCommand :: Int -> Gen Command
genCommand n = oneof
  [ liftM2 Declare arbitrary (pure SInt)
  , liftM2 Declare arbitrary (pure SBool)
  , fmap Assert (genExpBool n)
  ]

genScript :: Int -> Gen Script
genScript n = Script <$> listOf1 (genCommand n)

qtest :: Script -> IO ()
qtest = const (pure ())
