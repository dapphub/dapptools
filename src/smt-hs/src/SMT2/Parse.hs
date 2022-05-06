{-# LANGUAGE FlexibleContexts #-}

{- | Compile time SMT2 parser -}
module SMT2.Parse where

import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Data.Char (toLower)
import Text.Parsec
import Data.Functor
import Numeric (readHex)
import Numeric.Compat (readBin)
import Data.Function

import qualified Data.Text.Read as R

import SMT2.Syntax.Typed
import qualified SMT2.Syntax.Untyped as U

data SExp
  = Const Integer
  | Symbol String
  | Keyword String
  | Reserved String
  | SExp [SExp]

sexpr :: Parsec String st SExp
sexpr = node <|> leaf
  where
    leaf =  Const <$> (try numeral <|> try hexadecimal <|> try binary)
        <|> Symbol <$> try symbol
        <|> Keyword <$> try keyword
        <|> Reserved <$> try reservedWord
    node = SExp <$> (between `on` char) '(' ')' (sexpr `sepBy1` char ' ')

nameChar :: Parsec String st Char
nameChar = oneOf "~!@$%^&*_-+=<>.?/"

reservedWords :: [String]
reservedWords
  = [ "!", "_" , "as", "DECIMAL", "exists", "forall", "let", "NUMERAL", "par", "STRING"
    , "assert", "check-sat", "declare-sort", "declare-fun", "define-sort"
    , "define-fun", "exit", "get-assertions", "get-assignment", "get-info"
    , "get-option", "get-proof", "get-unsat-core", "get-value", "pop", "push"
    , "set-logic", "set-info", "set-option"
    ]

reservedWord :: Parsec String st String
reservedWord = choice (string <$> reservedWords)
  where
    parseWord w = try w <* notFollowedBy anyChar

-- | enclosing a simple symbol in vertical bars does not produce a
-- new symbol, e.g. @abc@ and @|abc|@ are the /same/ symbol
-- this is guaranteed by removing the bars
symbol :: Parsec String st String
symbol =  notFollowedBy (try reservedWord) >> (quotedSymbol <|> simpleSymbol <?> "symbol")
  where
    simpleSymbol = do
      c <- nameChar <|> letter
      cs <- many (alphaNum <|> nameChar)
      return (c:cs)
    quotedSymbol = between (char '|') (char '|') $ many (noneOf "\\|")

keyword :: Parsec String st String
keyword = do char ':'
             many1 (alphaNum <|> nameChar)

attribute :: Parsec String st String
attribute = keyword

bool :: Parsec String st Bool
bool =  string "true" $> True
    <|> string "false" $> False
    <?> "bool value"

stringLiteral :: Parsec String st String
stringLiteral = do char '"'
                   str <- many (nonEscaped <|> escaped)
                   char '"'
                   return str
  where
    nonEscaped = noneOf "\""
    escaped = try (string "\"\"") $> '"'

numeral :: Parsec String st Integer
numeral =  string "0" $> 0
       <|> do c <- oneOf "123456789"
              cs <- many digit
              return $ read (c:cs)

hexadecimal :: Parsec String st Integer
hexadecimal = do
  string "#x"
  s <- fmap toLower <$> many1 hexDigit
  return $ fst (readHex s !! 0)

binary :: Parsec String st Integer
binary = do
  string "#b"
  s <- many1 (char '0' <|> char '1')
  return $ fst (readBin s !! 0)

infoFlag :: Parsec String st InfoFlag
infoFlag =  try (string ":all-statistics" $> AllStatistics)
        <|> try (string ":assertion-stack-levels" $> AssertionStackLevels)
        <|> try (string ":authors" $> Authors)
        <|> try (string ":error-behaviour" $> ErrorBehaviour)
        <|> try (string ":name" $> Name)
        <|> try (string ":reason-unknown" $> ReasonUnknown)
        <|> try (string ":version" $> Version)
        <?> "info flag"


smtoption :: Parsec String st Option
smtoption =  DiagnosticOutputChannel <$> opt "diagnostic-output-channel" stringLiteral
         <|> GlobalDeclarations <$> opt "global-declarations" bool
         <|> InteractiveMode <$> opt "interactive-mode" bool
         <|> PrintSuccess <$> opt "print-success" bool
         <|> ProduceAssertions <$> opt "produce-assertions" bool
         <|> ProduceAssignments <$> opt "produce-assignments" bool
         <|> ProduceModels <$> opt "produce-models" bool
         <|> ProduceProofs <$> opt "produce-proofs" bool
         <|> ProduceUnsatAssumptions <$> opt "produce-unsat-assumptions" bool
         <|> ProduceUnsatCores <$> opt "produce-unsat-cores" bool
         <|> RandomSeed <$> opt "random-seed" numeral
         <|> RegularOutputChannel <$> opt "regular-output-channel" stringLiteral
         <|> ReproducibleResourceLimit <$> opt "reproducible-resource-limit" numeral
         <|> Verbosity <$> opt "verbosity" numeral
         <?> "option"
  where
    opt s arg = try (string s) *> space *> arg

smtexp :: Parsec String st (U.Exp)
smtexp = U.LitInt <$> (try numeral <|> try hexadecimal <|> try binary)
      <|> U.LitStr <$> try stringLiteral
      <|> U.LitBool <$> try bool
      <|> opMany "and" U.And
      <|> opMany "or" U.Or
      <|> opMany "eq" U.Eq
      <|> opMany "xor" U.Xor
      <|> opMany "distinct" U.Distinct
      <|> op3 "ite" U.ITE
      <|> op2 "concat" U.Concat
      <|> try extract
      <|> op1 "bvnot" U.BVNot
      <|> op1 "bvneg" U.BVNeg
      <|> op2 "bvand" U.BVAnd
      <|> op2 "bvor" U.BVOr
      <|> op2 "bvmul" U.BVMul
      <|> op2 "bvudiv" U.BVUDiv
      <|> op2 "bvurem" U.BVURem
      <|> op2 "bvshl" U.BVShl
      <|> op2 "bvshr" U.BVShr
      <|> op2 "bvult" U.BVShr
      <|> op1 "-" U.Neg
      <|> op2 "-" U.Sub
      <|> op2 "+" U.Add
      <|> op2 "*" U.Mul
      <|> op2 "div" U.Div
      <|> op2 "mod" U.Div
      <|> op1 "abs" U.Abs
      <|> op2 "<=" U.LEQ
      <|> op2 "<" U.LT
      <|> op2 ">=" U.GEQ
      <|> op2 ">" U.GT
      <|> try divisible
      <|> op2 "select" U.Select
      <|> op3 "store" U.Store
      <|> try (string "(" *> smtexp <* string ")")
      <?> "smt expression"
      where
        op1 n e = try $ do
          string n
          space
          a <- smtexp
          pure $ e a
        op2 n e = try $ do
          string n
          space
          a <- smtexp
          space
          b <- smtexp
          pure $ e a b
        op3 n e = try $ do
          string n
          space
          a <- smtexp
          space
          b <- smtexp
          space
          c <- smtexp
          pure $ e a b c
        opMany n e = e <$> try (string n *> space *> smtexp `sepBy1` char ' ')

divisible :: Parsec String st (U.Exp)
divisible = do
  string "divisble"
  space
  i <- fromInteger <$> numeral
  space
  e <- smtexp
  pure $ U.Divisible i e

extract :: Parsec String st (U.Exp)
extract = do
  string "extract"
  space
  i <- fromInteger <$> numeral
  space
  j <- fromInteger <$> numeral
  space
  e <- smtexp
  pure $ U.Extract i j e

test :: String -> Either ParseError U.Exp
test = parse smtexp "(source)"

test0 = test "(or (and 1 2 \"hi\") (or 3 false))"
test1 = test "(ite (and 1 2 \"hi\") (or 3 false) (eq 4 3))"
test3 = test "(or 1 2 (and 2 3 4))"
test4 = test "(or true false (and false true))"
