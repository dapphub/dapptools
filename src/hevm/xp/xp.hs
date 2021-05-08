{-# LANGUAGE DeriveGeneric     #-}
{-# Language OverloadedStrings #-}

module Main where

import Options.Generic as Options

import Text.Megaparsec hiding (State)
import Data.Text

import EVM.Expr
import EVM.ExprSimp


data Command
  = ToEC {
    exprString :: String
    }
  | Unify {
    exprA :: String,
    exprB :: String
    }
  | X {
    exprString :: String
    }
  deriving (Generic, Show)

instance ParseRecord Command

main :: IO ()
main = do
  x <- Options.getRecord "eXPr -- expression laboratory"
  case x :: Command of
    ToEC str -> let
        pexpr = parse pScheme "" (pack $ exprString x)
        in case pexpr of
        Left err    -> print (show err)
        Right expr  -> print (fromExpr expr)
    Unify exprA exprB
      -> print (x :: Command)
    X exprString
      -> print (x :: Command)

  -- print(x)
  -- let
  --   pexpr = parse pScheme "" (pack $ exprString x)
  --   in
  --   case pexpr of
  --   Left err    -> print (show err)
  --   Right expr  -> print (fromExpr expr)
