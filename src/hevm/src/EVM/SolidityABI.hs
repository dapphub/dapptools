module EVM.SolidityABI ( parseSolidityAbi , Fragment (..) ) where

import EVM.ABI

import Control.Monad (when)
import Data.Char (isDigit, isAlphaNum, isAlpha)
import Text.ParserCombinators.ReadP

import qualified Data.Vector as V

data Fragment = FunctionFragment
  { _name      :: String
  , _types     :: [AbiType]
  , _returns   :: [AbiType]
  , _modifiers :: [String]
  }
  deriving Show

parseSolidityAbi :: String -> Either String Fragment 
parseSolidityAbi abi =
  case readP_to_S parseFragment abi of
    [(val,"")] -> Right val
    r -> Left (show r)

isName1 :: Char -> Bool
isName1 c = isAlpha c || c == '_'

isName :: Char -> Bool
isName c = isAlphaNum c || c == '_'

identifier :: ReadP String
identifier = do
  c <- satisfy isName1
  s <- munch isName
  return (c:s)

space :: ReadP ()
space = skipMany1 (char ' ')

excluding :: (Eq a) => ReadP a -> [a] -> ReadP a
excluding p x = do
  name <- p
  when (elem name x) pfail
  return name

parseFragment :: ReadP Fragment
parseFragment = do
  optional (skipSpaces *> string "function" *> space)
  name  <- (skipSpaces *> identifier)
  types <- (skipSpaces *> parseTypes)
  -- n.b. modifier arguments are not supported
  modifiers <- option [] $
    space *> sepBy1 (identifier `excluding` ["returns"]) (many1 (char ' '))
  -- allow us to parse f(inputs)(outputs) with or without the "returns"
  returns <- option [] $
    optional (space *> string "returns") *> skipSpaces *> parseTypes
  skipSpaces
  optional (char ';')
  skipSpaces
  eof
  return (FunctionFragment name types returns modifiers)

parseTypes :: ReadP [AbiType]
parseTypes = between
  (char '(' *> skipSpaces)
  (skipSpaces <* char ')')
  (sepBy parseType (skipSpaces <* char ',' *> skipSpaces))

parseType :: ReadP AbiType
parseType = do
  t <- parseBasicType
  s <- many (between (char '[') (char ']') (munch isDigit))
  optional (space *> location)
  optional (space *> argName)
  return (foldl makeArray t s)
  where
    location = (string "memory" +++ string "calldata")
    argName = identifier `excluding` ["memory", "calldata"]

    makeArray :: AbiType -> String -> AbiType
    makeArray t "" = AbiArrayDynamicType t
    makeArray t s  = AbiArrayType (read s) t

    parseBasicType :: ReadP AbiType
    parseBasicType = choice
      [ AbiBoolType          <$  string "bool"
      , AbiAddressType       <$  string "address"
      , AbiStringType        <$  string "string"
      , AbiBytesDynamicType  <$  string "bytes"
      , AbiBytesType         <$> (string "bytes" *> (read <$> munch1 isDigit))
      , (<++)
        (AbiUIntType         <$> (string "uint"  *> (read <$> munch1 isDigit)))
        (AbiUIntType 256     <$  string "uint")
      , (<++)
        (AbiIntType          <$> (string "int"   *> (read <$> munch1 isDigit)))
        (AbiIntType 256      <$  string "int")
      , AbiTupleType         <$> V.fromList <$> (optional identifier *> parseTypes)
      ]
