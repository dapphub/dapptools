{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# Language ImplicitParams #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- TODO test nested mappings
-- loops

-- rename FromKeccak to keccak
-- Isn't return returning an Buffer * Storage instead of a function?
-- change this!
--
-- reduce everything to a logical formula:
-- e.g. ite c a b ==> (or (imp c a) (imp (not c) b))

module EVM.Expr where

import Prelude hiding  (Word, LT, GT)
import Data.List (intercalate)
import Data.DoubleWord
import Control.Monad
import GHC.Generics
import Data.Aeson
-- import Control.Monad
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
-- import Text.Megaparsec.Debug
import Data.Void
import Numeric (showHex)
import Data.Text (Text, unpack, pack, splitOn)
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
  | Lambda
    Expr
    Expr

  --booleans
  | And  Expr Expr
  | Or   Expr Expr
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
  -- | Sft  Expr Expr    -- shift left
  -- | Sar  Expr Expr    -- shift right
  | Bit  Expr
  | FromKeccak Expr
  | Pointer1 String Expr
  | Literal Word256
  | Var String ECType
  | Sig String Word256

  -- sniff
  | Slice
    Expr            -- from
    Expr            -- size
    Expr            -- buffer
  | Write
    Expr
    Expr
    Expr
    Expr
    Expr

  -- writes a word to an existing buffer
  -- used in memory
  | ReadWord
      Expr          -- position to read from
      Expr          -- existing buffer
      -- Expr          -- length of the buffer
  | WriteWord
      Expr          -- position to write to
      Expr          -- word that is written
      Expr          -- existing buffer
  | WriteStorage
      Expr          -- key
      Expr          -- value
      Expr          -- previous storage
  | ReadStorage
      Expr          -- key
      Expr          -- storage
  -- | Calldata        -- Unknown Calldata
  | SEmpty          -- Unknown Buffer
  | UStorage        -- Unknown Storage
  | Bottom
  | Return
    Expr            -- return buffer
    Expr            -- storage deltas
  deriving Eq

data ECType
  = ECTUnknown
  | ECTBool
  | ECTWord Int
  | ECTFunction ECType ECType
  | ECTAnd ECType ECType
  | ECTBuffer
  | ECTStorage
  | ECTError
  deriving (Eq, Ord, Generic, ToJSON)

instance Show ECType where
  show ECTUnknown   = "Unknown"
  show ECTBool      = "Bool"
  show (ECTWord i)  = "W" ++ (show i)
  show (ECTAnd a b) = "(" ++ (show a) ++ " * " ++ (show b)  ++ ")"
  show (ECTFunction a b) = (show a) ++ " -> " ++ (show b)
  show ECTBuffer    = "Buffer"
  show ECTStorage   = "Storage"
  show ECTError     = "Error"

-- compact representation for easyper folding and simplification
data ExprC
  = EC {
  name :: String,              -- name
  ttype :: ECType,              -- Type
  children :: [ ExprC ]           -- children
       }
  | ECTodo String [ExprC]
  | ECLiteral Word256
  | ECPointer1 String ExprC
  | ECVar String ECType
  | ECSig String Word256
  deriving (Eq, Generic, ToJSON)

instance ToJSON Word256 where
  toJSON p = Number (fromIntegral p)

indent :: String -> String
indent x = intercalate "\n" $ map ((++) "  ") $ map unpack $ splitOn (pack "\n") (pack x)

-- instance Show ExprC where
  -- show e = show $ encode e

instance Show ExprC where
  show (ECTodo str cs)       = "todo \"" ++ str ++  "\"\n[" ++ (intercalate "\n," (indent <$> show <$> cs)) ++ "\n]"
  show (ECLiteral x)         = "0x" ++ (showHex x "")
  show (ECPointer1 str expr) = "ptr:W256 \"" ++ str ++ "\" [\n" ++ (show expr) ++ "\n]"
  show (ECVar str t)         = "var:" ++ (show t) ++ " " ++ str
  show (ECSig str sig)       = "sig:W256 \"" ++ str ++ "\" 0x" ++ (showHex sig "")
  show (EC str t cs)         = str ++ ":" ++ (show t) ++ "\n[" ++ (intercalate "\n," (indent <$> show <$> cs)) ++ "\n]"


fromExpr :: Expr -> ExprC
fromExpr (Todo str as)       = ECTodo str (fromExpr <$> as)
fromExpr (Literal n)         = ECLiteral n
fromExpr (Pointer1 str a)    = ECPointer1 str (fromExpr a)
fromExpr (Var w t)           = ECVar w t
fromExpr (Sig w s)           = ECSig w s
fromExpr (Lambda a b)        = EC "Lambda"      ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (And  a b)          = EC "And"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Or   a b)          = EC "Or"          ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Eq   a b)          = EC "Eq"          ECTBool (fromExpr <$> [ a, b ])
fromExpr (LT   a b)          = EC "LT"          ECTBool (fromExpr <$> [ a, b ])
fromExpr (GT   a b)          = EC "GT"          ECTBool (fromExpr <$> [ a, b ])
fromExpr (SLT  a b)          = EC "SLT"         ECTBool (fromExpr <$> [ a, b ])
fromExpr (SGT  a b)          = EC "SGT"         ECTBool (fromExpr <$> [ a, b ])
fromExpr (SHL  a b)          = EC "SHL"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (SHR  a b)          = EC "SHR"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (SAR  a b)          = EC "SAR"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Add  a b)          = EC "Add"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Sub  a b)          = EC "Sub"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Mul  a b)          = EC "Mul"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Div  a b)          = EC "Div"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Mod  a b)          = EC "Mod"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Exp  a b)          = EC "Exp"         ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (Return a b)        = EC "Return"      ECTUnknown (fromExpr <$> [ a, b ])
fromExpr (IsZero a)          = EC "IsZero"      ECTBool    (fromExpr <$> [ a ])
fromExpr (Neg  a)            = EC "Neg"         ECTUnknown (fromExpr <$> [ a ])
fromExpr (Sex  a)            = EC "Sex"         ECTUnknown (fromExpr <$> [ a ])
fromExpr (Sgn  a)            = EC "Sgn"         ECTUnknown (fromExpr <$> [ a ])
fromExpr (Bit  a)            = EC "Bit"         ECTUnknown (fromExpr <$> [ a ])
fromExpr (FromKeccak a)      = EC "FromKeccak"  (ECTWord 256) (fromExpr <$> [ a ])
fromExpr (ITE c a b)         = EC "ITE"         ECTUnknown (fromExpr <$> [ c, a, b ])
fromExpr (Slice a b c)       = EC "Slice"       ECTBuffer  (fromExpr <$> [ a, b, c ])
fromExpr (ReadWord a b)      = EC "ReadWord"    (ECTWord 256) (fromExpr <$> [ a, b ])
fromExpr (WriteWord a b c)   = EC "WriteWord"   ECTBuffer  (fromExpr <$> [ a, b, c ])
fromExpr (ReadStorage a b)   = EC "ReadStorage" (ECTWord 256) (fromExpr <$> [ a, b ])
fromExpr (WriteStorage a b c)= EC "WriteStorage"ECTStorage (fromExpr <$> [ a, b, c ])
fromExpr (Write a b c d e)   = EC "Write"       ECTBuffer  (fromExpr <$> [ a, b, c, d, e ])
-- fromExpr (Calldata)          = EC "Calldata"    ECTBuffer  []
fromExpr (SEmpty)            = EC "SEmpty"      ECTBuffer  []
fromExpr (UStorage)          = EC "UStorage"    ECTStorage []
fromExpr (Bottom)            = EC "Bottom"      ECTUnknown []


toExpr :: ExprC -> Expr
toExpr (ECTodo str as)                        = (Todo str (toExpr <$> as))
toExpr (ECLiteral n)                          = (Literal n)
toExpr (ECPointer1 str a)                     = (Pointer1 str (toExpr a))
toExpr (ECVar w t)                            = (Var w t)
toExpr (ECSig w s)                            = (Sig w s)
toExpr (EC "Lambda"       _ [ a, b ])         = (Lambda (toExpr a) (toExpr b))
toExpr (EC "And"          _ [ a, b ])         = (And  (toExpr a) (toExpr b))
toExpr (EC "Or"           _ [ a, b ])         = (Or   (toExpr a) (toExpr b))
toExpr (EC "Eq"           _ [ a, b ])         = (Eq   (toExpr a) (toExpr b))
toExpr (EC "LT"           _ [ a, b ])         = (LT   (toExpr a) (toExpr b))
toExpr (EC "GT"           _ [ a, b ])         = (GT   (toExpr a) (toExpr b))
toExpr (EC "SLT"          _ [ a, b ])         = (SLT  (toExpr a) (toExpr b))
toExpr (EC "SGT"          _ [ a, b ])         = (SGT  (toExpr a) (toExpr b))
toExpr (EC "SHL"          _ [ a, b ])         = (SHL  (toExpr a) (toExpr b))
toExpr (EC "SHR"          _ [ a, b ])         = (SHR  (toExpr a) (toExpr b))
toExpr (EC "SAR"          _ [ a, b ])         = (SAR  (toExpr a) (toExpr b))
toExpr (EC "Add"          _ [ a, b ])         = (Add  (toExpr a) (toExpr b))
toExpr (EC "Sub"          _ [ a, b ])         = (Sub  (toExpr a) (toExpr b))
toExpr (EC "Mul"          _ [ a, b ])         = (Mul  (toExpr a) (toExpr b))
toExpr (EC "Div"          _ [ a, b ])         = (Div  (toExpr a) (toExpr b))
toExpr (EC "Mod"          _ [ a, b ])         = (Mod  (toExpr a) (toExpr b))
toExpr (EC "Exp"          _ [ a, b ])         = (Exp  (toExpr a) (toExpr b))
toExpr (EC "ReadStorage"  _ [ a, b ])         = (ReadStorage (toExpr a) (toExpr b))
toExpr (EC "ReadWord"     _ [ a, b ])         = (ReadWord (toExpr a) (toExpr b))
toExpr (EC "Return"       _ [ a, b ])         = (Return (toExpr a) (toExpr b))
toExpr (EC "IsZero"       _ [ a ])            = (IsZero (toExpr a))
toExpr (EC "Neg"          _ [ a ])            = (Neg (toExpr a))
toExpr (EC "Sex"          _ [ a ])            = (Sex (toExpr a))
toExpr (EC "Sgn"          _ [ a ])            = (Sgn (toExpr a))
toExpr (EC "Bit"          _ [ a ])            = (Bit (toExpr a))
toExpr (EC "FromKeccak"   _ [ a ])            = (FromKeccak (toExpr a))
toExpr (EC "ITE"          _ [ c, a, b ])      = (ITE (toExpr c) (toExpr a) (toExpr b))
toExpr (EC "Slice"        _ [ a, b, c ])      = (Slice (toExpr a) (toExpr b) (toExpr c))
toExpr (EC "WriteWord"    _ [ a, b, c ])      = (WriteWord (toExpr a) (toExpr b) (toExpr c))
toExpr (EC "WriteStorage" _ [ a, b, c ])      = (WriteStorage (toExpr a) (toExpr b) (toExpr c))
toExpr (EC "Write"        _ [ a, b, c, d, e ])= (Write (toExpr a) (toExpr b) (toExpr c) (toExpr d) (toExpr e))
-- toExpr (EC "Calldata"     _ [])               = (Calldata)
toExpr (EC "SEmpty"       _ [])               = (SEmpty)
toExpr (EC "UStorage"     _ [])               = (UStorage)
toExpr (EC "Bottom"       _ [])               = (Bottom)
-- error handling
toExpr (EC unknown _ children)
  = error $ "toExpr: unknown '" ++ unknown ++ "' token with children: " ++ (show children)
toExpr _ = error "compact form shouldn't be written by humans"

exprType :: ExprC -> ECType
exprType (ECTodo _ _)     = ECTUnknown
exprType (ECLiteral _)    = ECTWord 256
exprType (ECPointer1 _ _) = ECTWord 256
exprType (ECVar _ t)      = t
exprType (ECSig _ _)      = ECTWord 32
exprType (EC _ tt _)      = tt





-- helper
-- to construct mappings
addStorageMap :: Expr -- storage map to add to
              -> Expr -- key
              -> Expr -- value
              -> Expr -- resulting new map
addStorageMap m k v = (WriteStorage k v m)
-- (Lambda
--   (Var "new_name_needed")
--   (ITE (Eq (Var "new_name_needed") k) v m))


---------------------------------------------------------- format

instance Show Expr where
  show w =
    let
      sexpr s xs = "(" ++ s ++ "\n" ++ intercalate "\n" (show <$> xs) ++ ")"
      wstr s = "\"" ++ s ++ "\""
    in case w of
      Literal x       -> "0x" ++ (showHex x "")
      Lambda x y      -> sexpr "lambda" [x, y]
      Todo s args     -> sexpr ("todo " ++ (wstr s)) args
      Pointer1 name a -> sexpr ("ptr " ++ (wstr name)) [a]
      Write s w1 w2 w3 s2 -> sexpr "write" [s, w1, w2, w3, s2]
      ITE b x y       -> sexpr "if"     [b, x, y]
      Slice a b s     -> sexpr "slice"  [a, b, s]
      WriteWord a b s -> sexpr "writeword" [a, b, s]
      WriteStorage a b s -> sexpr "store" [a, b, s]
      ReadWord a b    -> sexpr "readword" [a, b]
      ReadStorage a b -> sexpr "read"   [a, b]
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
      Return b s      -> sexpr "return" [b, s]
      IsZero x        -> sexpr "IsZero" [x]
      Var v _         -> sexpr "var"    [v]
      Sig v s         -> sexpr "sig"    [v, showHex s ""]
      Bit a           -> sexpr "bit"    [a]
      Sgn a           -> sexpr "sgn"    [a]
      Sex a           -> sexpr "signextend" [a]
      Neg a           -> sexpr "not"    [a]
      FromKeccak buf  -> sexpr "keccak" [buf]
      -- Calldata        -> "CALLDATA"
      SEmpty          -> "SEmpty"
      UStorage        -> "UStorage"
      Bottom          -> "Bottom"



-------------------------------------------------------- generate

genExpr :: Int -> Gen Expr
genExpr n
  | n == 0 = oneof $
    [
    -- , liftM Literal (arbitrar
      -- pure Calldata -- TODO remove
      pure SEmpty
    , pure Bottom
    , pure UStorage
    ]
  | n>0 = oneof $
    [ liftM5 Write a a a a a
    , liftM3 WriteWord a a a
    , liftM3 WriteStorage a a a
    , liftM3 Slice a a a
    , liftM3 ITE a a a
    , liftM2 Todo as (pure [])
    , liftM2 ReadWord a a
    , liftM2 ReadStorage a a
    , liftM2 And a a
    , liftM2 Or a a
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
    , liftM2 Lambda a a
    , liftM2 Return a a
    , liftM2 Pointer1 as a
    -- , liftM2 Sig as arbitrary
    -- , liftM Var as
    , liftM IsZero a
    , liftM Neg a
    , liftM Sex a
    , liftM Sgn a
    , liftM Bit a
    , liftM FromKeccak a
    ]
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


pSig :: Parser Expr
pSig = do
  _ <- string "sig"
  space
  name <- stringLiteral
  space
  _ <- string "0x"
  lit <- L.hexadecimal
  return $ Sig name lit

pVar :: Parser Expr
pVar = do
  _ <- string "var"
  space
  name <- stringLiteral
  return $ Var name (ECTUnknown)

pTreToken :: Parser Expr
pTreToken = do
  t <- choice
    [ ITE       <$ string "if"
    , Slice     <$ string "slice"
    , WriteWord <$ string "writeword"
    , WriteStorage <$ string "store"
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
    [ And  <$ string "and"
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
    , Return      <$ string "return"
    , ReadWord    <$ string "readword"
    , ReadStorage <$ string "read"
    , Lambda      <$ string "lambda"
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
    , FromKeccak <$ string "keccak"
    , Neg        <$ string "not"]
  a <- pScheme
  return $ t a

pZToken :: Parser Expr
pZToken = choice
  [ 
  -- Calldata <$ string "CALLDATA"
    SEmpty   <$ string "SEmpty"
  , UStorage <$ string "UStorage"
  , Bottom   <$ string "Bottom"]


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
  , pUnToken
  , pBinToken
  , pTreToken
  , pWrite
  , pPtr
  , pVar
  , pSig
  ]

pSpecial :: Parser Expr
pSpecial = choice
  [ pLiteral
  , pZToken
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

