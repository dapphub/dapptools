{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# Language ImplicitParams #-}

module EVM.Expr where

import Prelude hiding  (Word, LT, GT)
import Data.List (intercalate)
import Data.DoubleWord
import Control.Monad
-- import Control.Monad
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug
import Data.Void
import Numeric (showHex)
import Data.Text (Text, pack)
import Test.QuickCheck
import qualified Text.Megaparsec.Char.Lexer as L
-- import qualified Data.Text as T

type Parser = Parsec Void Text

-- | This type can give insight into the provenance of a term
-- which is useful, both for the aesthetic purpose of printing
-- terms in a richer way, but also do optimizations on the AST
-- instead of letting the SMT solver do all the heavy lifting.
data Expr =
    Todo String [Expr]
  | Lambda Expr Expr

  --booleans
  | And  Expr Expr
  | Or   Expr Expr
  | Impl Expr Expr
  | Eq   Expr Expr
  | LT   Expr Expr
  | GT   Expr Expr
  | SLT  Expr Expr
  | SGT  Expr Expr
  | IsZero Expr
  | ITE Expr Expr Expr

  -- bits
  | SHL Expr Expr
  | SHR Expr Expr
  | SAR Expr Expr

  -- integers
  | Add  Expr Expr
  | Sub  Expr Expr
  | Mul  Expr Expr
  | Div  Expr Expr
  | Mod  Expr Expr
  | Exp  Expr Expr
  | Neg  Expr

  | Sex  Expr
  | Sgn  Expr          -- signum
  | Cmp  Expr          -- complement
  -- | Sft  Expr Expr    -- shift left
  -- | Sar  Expr Expr    -- shift right
  | Bit  Expr
  | FromKeccak Expr
  -- | FromBuffer Expr Buffer
  | FromBuff Expr Expr
  | FromStorage Expr Expr
  | Pointer1 String Expr
  | Literal Word256
  | Var String

  -- sniff
  | Oops String
  | Slice Expr Expr Expr          -- offset size buffer
  | Write Expr Expr Expr Expr Expr

  -- writes a word to an existing buffer
  -- used in memory
  | WriteWord
      Expr          -- position to write to
      Expr          -- word that is written
      Expr          -- existing buffer
  | Calldata        -- Unknown Calldata
  | SEmpty          -- Unknown Buffer
  | UStorage        -- Unknown Storage
  | Bottom
  deriving Eq

-- compact representation for easyper folding and simplification
data ExprC
  = EC
  String            -- name
  [ExprC]           -- children
  | ECTodo String [ExprC]
  | ECLiteral Word256
  | ECPointer1 String ExprC
  | ECVar String
  | ECOops String
  deriving (Eq, Show)


fromExpr :: Expr -> ExprC
fromExpr (Todo str as)       = ECTodo str (fromExpr <$> as)
fromExpr (Literal n)         = ECLiteral n
fromExpr (Pointer1 str a)    = ECPointer1 str (fromExpr a)
fromExpr (Var w)             = ECVar w
fromExpr (Oops str)          = ECOops str
fromExpr (Lambda a b)        = EC "Lambda"     (fromExpr <$> [ a, b ])
fromExpr (And  a b)          = EC "And"        (fromExpr <$> [ a, b ])
fromExpr (Or   a b)          = EC "Or"         (fromExpr <$> [ a, b ])
fromExpr (Impl a b)          = EC "Impl"       (fromExpr <$> [ a, b ])
fromExpr (Eq   a b)          = EC "Eq"         (fromExpr <$> [ a, b ])
fromExpr (LT   a b)          = EC "LT"         (fromExpr <$> [ a, b ])
fromExpr (GT   a b)          = EC "GT"         (fromExpr <$> [ a, b ])
fromExpr (SLT  a b)          = EC "SLT"        (fromExpr <$> [ a, b ])
fromExpr (SGT  a b)          = EC "SGT"        (fromExpr <$> [ a, b ])
fromExpr (SHL  a b)          = EC "SHL"        (fromExpr <$> [ a, b ])
fromExpr (SHR  a b)          = EC "SHR"        (fromExpr <$> [ a, b ])
fromExpr (SAR  a b)          = EC "SAR"        (fromExpr <$> [ a, b ])
fromExpr (Add  a b)          = EC "Add"        (fromExpr <$> [ a, b ])
fromExpr (Sub  a b)          = EC "Sub"        (fromExpr <$> [ a, b ])
fromExpr (Mul  a b)          = EC "Mul"        (fromExpr <$> [ a, b ])
fromExpr (Div  a b)          = EC "Div"        (fromExpr <$> [ a, b ])
fromExpr (Mod  a b)          = EC "Mod"        (fromExpr <$> [ a, b ])
fromExpr (Exp  a b)          = EC "Exp"        (fromExpr <$> [ a, b ])
fromExpr (FromBuff a b)      = EC "FromBuff"   (fromExpr <$> [ a, b ])
fromExpr (FromStorage a b)   = EC "FromStorage"(fromExpr <$> [ a, b ])
fromExpr (IsZero a)          = EC "IsZero"     (fromExpr <$> [ a ])
fromExpr (Neg  a)            = EC "Neg"        (fromExpr <$> [ a ])
fromExpr (Sex  a)            = EC "Sex"        (fromExpr <$> [ a ])
fromExpr (Sgn  a)            = EC "Sgn"        (fromExpr <$> [ a ])
fromExpr (Cmp  a)            = EC "Cmp"        (fromExpr <$> [ a ])
fromExpr (Bit  a)            = EC "Bit"        (fromExpr <$> [ a ])
fromExpr (FromKeccak a)      = EC "FromKeccak" (fromExpr <$> [ a ])
fromExpr (ITE c a b)         = EC "ITE"        (fromExpr <$> [ a, b, c ])
fromExpr (Slice a b c)       = EC "Slice"      (fromExpr <$> [ a, b, c ])
fromExpr (WriteWord a b c)   = EC "WriteWord"  (fromExpr <$> [ a, b, c ])
fromExpr (Write a b c d e)   = EC "Write"      (fromExpr <$> [ a, b, c, d, e ])
fromExpr (Calldata)          = EC "Calldata"   []
fromExpr (SEmpty)            = EC "SEmpty"     []
fromExpr (UStorage)          = EC "UStorage"   []
fromExpr (Bottom)            = EC "Bottom"     []


toExpr :: ExprC -> Expr
toExpr (ECTodo str as)                    = (Todo str (toExpr <$> as))
toExpr (ECLiteral n)                      = (Literal n)
toExpr (ECPointer1 str a)                 = (Pointer1 str (toExpr a))
toExpr (ECVar w)                          = (Var w)
toExpr (ECOops str)                       = (Oops str)
toExpr (EC "Lambda"     [ a, b ])         = (Lambda (toExpr a) (toExpr b))
toExpr (EC "And"        [ a, b ])         = (And  (toExpr a) (toExpr b))
toExpr (EC "Or"         [ a, b ])         = (Or   (toExpr a) (toExpr b))
toExpr (EC "Impl"       [ a, b ])         = (Impl (toExpr a) (toExpr b))
toExpr (EC "Eq"         [ a, b ])         = (Eq   (toExpr a) (toExpr b))
toExpr (EC "LT"         [ a, b ])         = (LT   (toExpr a) (toExpr b))
toExpr (EC "GT"         [ a, b ])         = (GT   (toExpr a) (toExpr b))
toExpr (EC "SLT"        [ a, b ])         = (SLT  (toExpr a) (toExpr b))
toExpr (EC "SGT"        [ a, b ])         = (SGT  (toExpr a) (toExpr b))
toExpr (EC "SHL"        [ a, b ])         = (SHL  (toExpr a) (toExpr b))
toExpr (EC "SHR"        [ a, b ])         = (SHR  (toExpr a) (toExpr b))
toExpr (EC "SAR"        [ a, b ])         = (SAR  (toExpr a) (toExpr b))
toExpr (EC "Add"        [ a, b ])         = (Add  (toExpr a) (toExpr b))
toExpr (EC "Sub"        [ a, b ])         = (Sub  (toExpr a) (toExpr b))
toExpr (EC "Mul"        [ a, b ])         = (Mul  (toExpr a) (toExpr b))
toExpr (EC "Div"        [ a, b ])         = (Div  (toExpr a) (toExpr b))
toExpr (EC "Mod"        [ a, b ])         = (Mod  (toExpr a) (toExpr b))
toExpr (EC "Exp"        [ a, b ])         = (Exp  (toExpr a) (toExpr b))
toExpr (EC "FromBuff"   [ a, b ])         = (FromBuff    (toExpr a) (toExpr b))
toExpr (EC "FromStorage"[ a, b ])         = (FromStorage (toExpr a) (toExpr b))
toExpr (EC "IsZero"     [ a ])            = (IsZero (toExpr a))
toExpr (EC "Neg"        [ a ])            = (Neg (toExpr a))
toExpr (EC "Sex"        [ a ])            = (Sex (toExpr a))
toExpr (EC "Sgn"        [ a ])            = (Sgn (toExpr a))
toExpr (EC "Cmp"        [ a ])            = (Cmp (toExpr a))
toExpr (EC "Bit"        [ a ])            = (Bit (toExpr a))
toExpr (EC "FromKeccak" [ a ])            = (FromKeccak (toExpr a))
toExpr (EC "ITE"        [ a, b, c ])      = (ITE (toExpr c) (toExpr a) (toExpr b))
toExpr (EC "Slice"      [ a, b, c ])      = (Slice (toExpr a) (toExpr b) (toExpr c))
toExpr (EC "WriteWord"  [ a, b, c ])      = (WriteWord (toExpr a) (toExpr b) (toExpr c))
toExpr (EC "Write"      [ a, b, c, d, e ])= (Write (toExpr a) (toExpr b) (toExpr c) (toExpr d) (toExpr e))
toExpr (EC "Calldata"   [])               = (Calldata)
toExpr (EC "SEmpty"     [])               = (SEmpty)
toExpr (EC "UStorage"   [])               = (UStorage)
toExpr (EC "Bottom"     [])               = (Bottom)
toExpr _ = error "compact form shouldn't be written by humans"


-- Test to and from
-- quickcheck passes this, yay
-- test this with `quickCheck prop_ExprConversionIsomorphism`
prop_ExprConversionIsomorphism :: Expr -> Bool
prop_ExprConversionIsomorphism e = toExpr (fromExpr e) == e



-- helper
-- to construct mappings
addStorageMap :: Expr -- storage map to add to
              -> Expr -- key
              -> Expr -- value
              -> Expr -- resulting new map
addStorageMap m k v = (Lambda
  (Var "new_name_needed")
  (ITE (Eq (Var "new_name_needed") k) v m))


---------------------------------------------------------- format

instance Show Expr where
  show w =
    let
      sexpr s xs = "(" ++ s ++ " " ++ intercalate " " (show <$> xs) ++ ")"
      wstr s = "\"" ++ s ++ "\""
    in case w of
      Var v           -> v
      Literal x       -> "0x" ++ (showHex x "")
      Lambda x y      -> sexpr "lambda" [x, y]
      Todo s args     -> sexpr ("todo " ++ (wstr s)) args
      Pointer1 name a -> sexpr ("ptr " ++ (wstr name)) [a]
      Write s w1 w2 w3 s2 -> sexpr "write" [s, w1, w2, w3, s2]
      ITE b x y       -> sexpr "if"     [b, x, y]
      Slice a b s     -> sexpr "slice"  [a, b, s]
      WriteWord a b s -> sexpr "writeword" [a, b, s]
      And x y         -> sexpr "and"    [x, y]
      Or x y          -> sexpr "or"     [x, y]
      Eq x y          -> sexpr "=="     [x, y]
      LT x y          -> sexpr "<"      [x, y]
      GT x y          -> sexpr ">"      [x, y]
      SLT x y         -> sexpr "s<"     [x, y]
      SGT x y         -> sexpr "s>"     [x, y]
      SHL x y         -> sexpr "shl"    [x, y]
      SHR x y         -> sexpr "shr"    [x, y]
      SAR x y         -> sexpr "sar"    [x, y]
      Add x y         -> sexpr "+"      [x, y]
      Sub x y         -> sexpr "-"      [x, y]
      Exp x y         -> sexpr "**"     [x, y]
      Mul x y         -> sexpr "*"      [x, y]
      Div x y         -> sexpr "/"      [x, y]
      Mod x y         -> sexpr "%"      [x, y]
      Impl a b        -> sexpr "=>"     [a, b]
      FromStorage l s -> sexpr "sload"  [l, s]
      FromBuff l s    -> sexpr "fbuff"  [l, s]
      IsZero x        -> sexpr "IsZero" [x]
      Bit a           -> sexpr "bit"    [a]
      Sgn a           -> sexpr "sgn"    [a]
      Sex a           -> sexpr "signextend" [a]
      Cmp a           -> sexpr "~"      [a]
      Neg a           -> sexpr "not"    [a]
      FromKeccak buf  -> sexpr "keccak" [buf]
      Oops s          -> sexpr "Oops"   [s]
      Calldata        -> "CALLDATA"
      SEmpty          -> "SEmpty"
      UStorage        -> "UStorage"
      Bottom          -> "Bottom"



-------------------------------------------------------- generate

genExpr :: Int -> Gen Expr
genExpr n
  | n == 0 = oneof $
    [ liftM Oops as
    -- , liftM Literal (arbitrar
    , pure Calldata -- TODO remove
    , pure SEmpty
    , pure Bottom
    , pure UStorage
    ]
  | n>0 = oneof $
    [ liftM5 Write a a a a a
    , liftM3 WriteWord a a a
    , liftM3 Slice a a a
    , liftM3 ITE a a a
    , liftM2 Todo as (pure [])
    , liftM2 And a a
    , liftM2 Or a a
    , liftM2 Impl a a
    , liftM2 Eq a a
    , liftM2 LT a a
    , liftM2 GT a a
    , liftM2 SLT a a
    , liftM2 SGT a a
    , liftM2 SHL a a
    , liftM2 SHR a a
    , liftM2 SAR a a
    , liftM2 Add a a
    , liftM2 Sub a a
    , liftM2 Mul a a
    , liftM2 Div a a
    , liftM2 Mod a a
    , liftM2 Exp a a
    , liftM2 FromBuff a a
    , liftM2 FromStorage a a
    , liftM2 Pointer1 as a
    , liftM Var as
    , liftM IsZero a
    , liftM Neg a
    , liftM Sex a
    , liftM Sgn a
    , liftM Cmp a
    , liftM Bit a
    , liftM FromKeccak a
    , liftM Oops as ]
  | otherwise = error "should never be the case"
    where a = genExpr (n `div` 2)
          as :: Gen String
          as = listOf1 $ elements ['a'..'z']

instance Arbitrary Expr where
  arbitrary = sized genExpr

----------------------------------------------------------- parse

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

pPtr :: Parser Expr
pPtr = do
  t <- Pointer1 <$ string "ptr"
  space
  name <- stringLiteral
  space
  a <- pScheme
  return $ t name a

pLiteral :: Parser Expr
pLiteral = do
  _ <- string "0x"
  lit <- L.hexadecimal
  return $ Literal lit

pVar :: Parser Expr
pVar = do
  name <- ((:) <$> letterChar <*> many alphaNumChar <?> "variable")
  return $ Var name

pTreToken :: Parser Expr
pTreToken = do
  t <- choice
    [ ITE       <$ string "if"
    , Slice     <$ string "slice"
    , WriteWord <$ string "writeword"
    ]
  c <- pScheme
  x <- pScheme
  y <- pScheme
  return $ t c x y

pWrite :: Parser Expr
pWrite = do
  _ <- string "write"
  a <- pScheme
  b <- pScheme
  c <- pScheme
  d <- pScheme
  e <- pScheme
  return $ Write a b c d e

pBinToken :: Parser Expr
pBinToken = do
  t <- choice
    [ Impl <$ string "=>"
    , And  <$ string "and"
    , Or   <$ string "or"
    , Eq   <$ string "=="
    , LT   <$ string "<"
    , GT   <$ string ">"
    , SLT  <$ string "s<"
    , SGT  <$ string "s>"
    , SHL  <$ string "shl"
    , SHR  <$ string "shr"
    , SAR  <$ string "sar"
    , Add  <$ string "+"
    , Sub  <$ string "-"
    , Exp  <$ string "**"
    , Mul  <$ string "*"
    , Div  <$ string "/"
    , Mod  <$ string "%"
    , FromBuff <$ string "fbuff"
    , FromStorage <$ string "sload"
    ]
  a <- pScheme
  b <- pScheme
  return $ t a b

pUnToken :: Parser Expr
pUnToken = do
  t <- choice
    [ Sgn        <$ string "sgn"
    , Bit        <$ string "bit"
    , Sgn        <$ string "sgn"
    , IsZero     <$ string "IsZero"
    , Sex        <$ string "signextend"
    , Cmp        <$ string "~"
    , FromKeccak <$ string "keccak"
    , Neg        <$ string "not"]
  a <- pScheme
  return $ t a

pZToken :: Parser Expr
pZToken = choice
  [ Calldata <$ string "CALLDATA"
  , SEmpty   <$ string "SEmpty"
  , UStorage <$ string "UStorage"
  , Bottom   <$ string "Bottom"]


pOops :: Parser Expr
pOops = do
  _ <- string "Oops"
  space
  name <- stringLiteral
  return $ Oops name

pTodo :: Parser Expr
pTodo = do
  _ <- string "todo"
  space
  name <- stringLiteral
  space
  children <- many pScheme
  space
  return $ Todo name children

pKey :: Parser Expr
pKey = choice
  [ pTodo
  , pOops
  , pUnToken
  , pBinToken
  , pTreToken
  , pWrite
  , pPtr
  ]

pSpecial :: Parser Expr
pSpecial = choice
  [ pLiteral
  , pZToken
  , pVar
  ]

pS :: Parser Expr
pS = do
  _ <- char '('
  space
  e <- pKey
  space
  _ <- char ')'
  return e

pScheme :: Parser Expr
pScheme = do
  space
  expr <- (choice
    [ pS
    , pSpecial
    ])
  space
  return (expr)




-- Test
-- quickcheck passes this, yay
-- test this with `quickCheck prop_ExprParsePrintIsomorphism`
prop_ExprParsePrintIsomorphism :: Expr -> Bool
prop_ExprParsePrintIsomorphism e =
  case parse pScheme "" (pack $ show e) of
    Left _   -> False
    Right e' -> e == e'
