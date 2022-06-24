module EVM.SolidityABI ( parseSolidityAbi ) where

import EVM.ABI (AbiType(..), Event(..), Anonymity(..), Indexed(..), SolError(..))
import EVM.Solidity (Method(..), Mutability(..))

import Control.Monad (when)
import Data.Char (isDigit, isAlphaNum, isAlpha)
import Data.Text (Text, pack)
import Text.ParserCombinators.ReadP

import qualified Data.Vector as V

parseSolidityAbi :: String -> Either String (Maybe Method, Maybe Event, Maybe SolError)
parseSolidityAbi abi =
  case readP_to_S parseFunction abi of
    [(val,"")] -> Right (Just val, Nothing, Nothing)
    [] -> case readP_to_S parseEvent abi of
      [(val,"")] -> Right (Nothing, Just val, Nothing)
      [] -> case readP_to_S parseError abi of
        [(val,"")] -> Right (Nothing, Nothing, Just val)
        r -> Left (show r)
      r -> Left (show r)
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

parseFunction :: ReadP Method
parseFunction = do
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
  let mutability =
        case (flip elem modifiers) <$> ["view", "pure", "payable"] of
          [False, False, False] -> NonPayable
          [True,  False, False] -> View
          [False, True,  False] -> Pure
          [False, False, True ] -> Payable
          _ -> error "overspecified mutability"
  return $ Method
    (fst <$> returns)
    (fst <$> types)
    (pack name)
    (pack $ name <> (show (AbiTupleType (V.fromList (snd <$> fst <$> types)))))
    mutability

parseError :: ReadP SolError
parseError = do
  (skipSpaces *> string "error" *> space)
  name  <- (skipSpaces *> identifier)
  types <- (skipSpaces *> parseTypes)
  skipSpaces
  optional (char ';')
  skipSpaces
  eof
  return $ SolError (pack name) (snd <$> fst <$> types)

data Location
  = MemoryLocation
  | CalldataLocation
  | IndexedLocation
  | NoLocation

parseEvent :: ReadP Event
parseEvent = do
  skipSpaces *> string "event" *> space
  name  <- (skipSpaces *> identifier)
  types <- (skipSpaces *> parseTypes)
  anon  <- option "" (space *> string "anonymous")
  skipSpaces
  optional (char ';')
  skipSpaces
  eof
  let
    anonymous = \case
      "anonymous" -> Anonymous
      "" -> NotAnonymous
    indexed = \case
      ((argname, typ), IndexedLocation) -> (argname, typ, Indexed)
      ((argname, typ), _)               -> (argname, typ, NotIndexed)
  return $ EVM.ABI.Event (pack name) (anonymous anon) (indexed <$> types)

parseTypes :: ReadP [((Text, AbiType), Location)]
parseTypes = between
  (char '(' *> skipSpaces)
  (skipSpaces <* char ')')
  (sepBy parseType (skipSpaces <* char ',' *> skipSpaces))

parseType :: ReadP ((Text, AbiType), Location)
parseType = do
  t <- parseBasicType
  s <- many (between (char '[') (char ']') (munch isDigit))
  loc  <- option "" (space *> location)
  name <- option "" (space *> argName)
  return ((pack name, foldl makeArray t s), locate loc)
  where
    location = (string "memory" +++ string "calldata" +++ string "indexed")
    locate = \case
      "memory"   -> MemoryLocation
      "calldata" -> CalldataLocation
      "indexed"  -> IndexedLocation
      ""         -> NoLocation
      _          -> error "unknown location"

    argName = identifier `excluding` ["memory", "calldata", "indexed"]

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
      , AbiNamedTupleType    <$> fst <$> V.unzip <$> V.fromList <$> (optional identifier *> parseTypes)
      ]
