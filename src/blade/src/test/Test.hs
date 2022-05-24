{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding (GT, LT)
import Control.Monad
import Debug.Trace

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Text.Parsec (parse)

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
      , testCase "trailing newlines" $ qtest [smt2|
          (assert true)




        |]
      , testCase "leading newlines" $ qtest [smt2|




          (assert true)



        |]
      ]
  ]


-- generators --------------------------------------------------------------------------------------


genExpBool :: Int -> Gen (Exp Boolean)
genExpBool 0 = oneof
  [ LitBool <$> arbitrary
  , Var <$> name
  ]
genExpBool n = oneof
  [ fmap And (listOf1 subExpBool)
  , fmap Or (listOf1 subExpBool)
  , fmap Eq (listOf1 subExpBool)
  , fmap Xor (listOf1 subExpBool)
  , fmap Impl (listOf1 subExpBool)
  , fmap Distinct (listOf1 subExpBool)
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
  , Var <$> name
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
  [ liftM2 Declare name (pure SInt)
  , liftM2 Declare name (pure SBool)
  , fmap Assert (genExpBool n)
  ]

genScript :: Int -> Gen Script
genScript n = Script <$> listOf1 (genCommand n)

lower :: Gen Char
lower = frequency [ (26, choose ('a', 'z')) ]

upper :: Gen Char
upper = frequency [ (26, choose ('A', 'Z')) ]

digit :: Gen Char
digit = frequency [ (10, choose ('0', '9')) ]

special :: Gen Char
special = elements "~!@$%^&*_-+=<>.?/"

name :: Gen String
name = do
  h <- lower
  ts <- listOf (oneof [lower, upper, digit, special])
  pure (h : ts)

reservedWords :: [String]
reservedWords
  = [ "!", "_" , "as", "DECIMAL", "exists", "forall", "let", "NUMERAL", "par", "STRING"
    , "assert", "check-sat", "declare-sort", "declare-fun", "define-sort"
    , "define-fun", "exit", "get-assertions", "get-assignment", "get-info"
    , "get-option", "get-proof", "get-unsat-core", "get-value", "pop", "push"
    , "set-logic", "set-info", "set-option"
    ]


-- utils -------------------------------------------------------------------------------------------


qtest :: Script -> IO ()
qtest = const (pure ())
