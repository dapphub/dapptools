{-# Language DeriveAnyClass #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module EVM.Solidity
  ( solidity
  , JumpType (..)
  , SolcContract (..)
  , SourceCache (..)
  , SrcMap (..)
  , CodeType (..)
  , Method (..)
  , methodName
  , methodSignature
  , methodInputs
  , methodOutput
  , parseMethodInput
  , abiMap
  , eventMap
  , contractName
  , constructorInputs
  , creationCode
  , makeSrcMaps
  , readSolc
  , runtimeCode
  , snippetCache
  , runtimeCodehash
  , creationCodehash
  , runtimeSrcmap
  , creationSrcmap
  , contractAst
  , sourceFiles
  , sourceLines
  , sourceAsts
  , stripBytecodeMetadata
  , lineSubrange
  , astIdMap
  , astSrcMap
) where

import EVM.ABI
import EVM.Keccak
import EVM.Types

import Codec.CBOR.Term      (decodeTerm)
import Codec.CBOR.Read      (deserialiseFromBytes)
import Control.Applicative
import Control.Lens         hiding (Indexed)
import Data.Aeson           (Value (..))
import Data.Aeson.Lens
import Data.Binary.Get      (runGet, getWord16be)
import Data.ByteString      (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Char            (isDigit)
import Data.Either          (isRight)
import Data.Foldable
import Data.Map.Strict      (Map)
import Data.Maybe
import Data.Semigroup
import Data.Sequence        (Seq)
import Data.Text            (Text, pack, intercalate)
import Data.Text.Encoding   (encodeUtf8)
import Data.Text.IO         (readFile, writeFile)
import Data.Vector          (Vector)
import Data.Word
import GHC.Generics         (Generic)
import Prelude hiding       (readFile, writeFile)
import System.IO hiding     (readFile, writeFile)
import System.IO.Temp
import System.Process
import Text.Read            (readMaybe)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.HashMap.Strict    as HMap
import qualified Data.Map.Strict        as Map
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector

data SolcContract = SolcContract
  { _runtimeCodehash  :: W256
  , _creationCodehash :: W256
  , _runtimeCode      :: ByteString
  , _creationCode     :: ByteString
  , _contractName     :: Text
  , _constructorInputs :: [(Text, AbiType)]
  , _abiMap           :: Map Word32 Method
  , _eventMap         :: Map W256 Event
  , _runtimeSrcmap    :: Seq SrcMap
  , _creationSrcmap   :: Seq SrcMap
  , _contractAst      :: Value
  } deriving (Show, Eq, Generic)

data Method = Method
  { _methodOutput :: Maybe (Text, AbiType)
  , _methodInputs :: [(Text, AbiType)]
  , _methodName :: Text
  , _methodSignature :: Text
  } deriving (Show, Eq, Ord, Generic)

data SourceCache = SourceCache
  { _snippetCache :: Map (Int, Int) ByteString
  , _sourceFiles  :: Map Int (Text, ByteString)
  , _sourceLines  :: Map Int (Vector ByteString)
  , _sourceAsts   :: Map Text Value
  } deriving (Show, Eq, Generic)

instance Semigroup SourceCache where
  _ <> _ = error "lol"

instance Monoid SourceCache where
  mempty = SourceCache mempty mempty mempty mempty

data JumpType = JumpInto | JumpFrom | JumpRegular
  deriving (Show, Eq, Ord, Generic)

data SrcMap = SM {
  srcMapOffset :: {-# UNPACK #-} Int,
  srcMapLength :: {-# UNPACK #-} Int,
  srcMapFile   :: {-# UNPACK #-} Int,
  srcMapJump   :: JumpType,
  srcMapModifierDepth :: {-# UNPACK #-} Int
} deriving (Show, Eq, Ord, Generic)

data SrcMapParseState
  = F1 [Char] Int
  | F2 Int [Char] Int
  | F3 Int Int [Char] Int
  | F4 Int Int Int (Maybe JumpType)
  | F5 Int Int Int JumpType [Char]
  | Fe
  deriving Show

data CodeType = Creation | Runtime
  deriving (Show, Eq, Ord)

makeLenses ''SolcContract
makeLenses ''SourceCache
makeLenses ''Method

-- Obscure but efficient parser for the Solidity sourcemap format.
makeSrcMaps :: Text -> Maybe (Seq SrcMap)
makeSrcMaps = (\case (_, Fe, _) -> Nothing; x -> Just (done x))
             . Text.foldl' (\x y -> go y x) (mempty, F1 [] 1, SM 0 0 0 JumpRegular 0)
  where
    done (xs, s, p) = let (xs', _, _) = go ';' (xs, s, p) in xs'
    readR = read . reverse

    go :: Char -> (Seq SrcMap, SrcMapParseState, SrcMap) -> (Seq SrcMap, SrcMapParseState, SrcMap)
    go ':' (xs, F1 [] _, p@(SM a _ _ _ _))     = (xs, F2 a [] 1, p)
    go ':' (xs, F1 ds k, p)                    = (xs, F2 (k * (readR ds)) [] 1, p)
    go '-' (xs, F1 [] _, p)                    = (xs, F1 [] (-1), p)
    go d   (xs, F1 ds k, p) | isDigit d        = (xs, F1 (d : ds) k, p)
    go ';' (xs, F1 [] k, p)                    = (xs |> p, F1 [] k, p)
    go ';' (xs, F1 ds k, SM _ b c d e)         = let p' = SM (k * (readR ds)) b c d e in (xs |> p', F1 [] 1, p')

    go '-' (xs, F2 a [] _, p)                  = (xs, F2 a [] (-1), p)
    go d   (xs, F2 a ds k, p) | isDigit d      = (xs, F2 a (d : ds) k, p)
    go ':' (xs, F2 a [] _, p@(SM _ b _ _ _))   = (xs, F3 a b [] 1, p)
    go ':' (xs, F2 a ds k, p)                  = (xs, F3 a (k * (readR ds)) [] 1, p)
    go ';' (xs, F2 a [] _, SM _ b c d e)       = let p' = SM a b c d e in (xs |> p', F1 [] 1, p')
    go ';' (xs, F2 a ds k, SM _ _ c d e)       = let p' = SM a (k * (readR ds)) c d e in
                                                 (xs |> p', F1 [] 1, p')

    go d   (xs, F3 a b ds k, p) | isDigit d    = (xs, F3 a b (d : ds) k, p)
    go '-' (xs, F3 a b [] _, p)                = (xs, F3 a b [] (-1), p)
    go ':' (xs, F3 a b [] _, p@(SM _ _ c _ _)) = (xs, F4 a b c Nothing, p)
    go ':' (xs, F3 a b ds k, p)                = (xs, F4 a b (k * (readR ds)) Nothing, p)
    go ';' (xs, F3 a b [] _, SM _ _ c d e)     = let p' = SM a b c d e in (xs |> p', F1 [] 1, p')
    go ';' (xs, F3 a b ds k, SM _ _ _ d e)     = let p' = SM a b (k * (readR ds)) d e in
                                                 (xs |> p', F1 [] 1, p')

    go 'i' (xs, F4 a b c Nothing, p)           = (xs, F4 a b c (Just JumpInto), p)
    go 'o' (xs, F4 a b c Nothing, p)           = (xs, F4 a b c (Just JumpFrom), p)
    go '-' (xs, F4 a b c Nothing, p)           = (xs, F4 a b c (Just JumpRegular), p)
    go ':' (xs, F4 a b c (Just d),  p)         = (xs, F5 a b c d [], p)
    go ':' (xs, F4 a b c _, p@(SM _ _ _ d _))  = (xs, F5 a b c d [], p)
    go ';' (xs, F4 a b c _, SM _ _ _ d e)      = let p' = SM a b c d e in
                                                 (xs |> p', F1 [] 1, p')

    go d   (xs, F5 a b c j ds, p) | isDigit d  = (xs, F5 a b c j (d : ds), p)
    go ';' (xs, F5 a b c j [], _)              = let p' = SM a b c j (-1) in -- solc <0.6
                                                 (xs |> p', F1 [] 1, p')
    go ';' (xs, F5 a b c j ds, _)              = let p' = SM a b c j (readR ds) in -- solc >=0.6
                                                 (xs |> p', F1 [] 1, p')

    go c (xs, state, p)                      = (xs, error ("srcmap: y u " ++ show c ++ " in state" ++ show state ++ "?!?"), p)

makeSourceCache :: [Text] -> Map Text Value -> IO SourceCache
makeSourceCache paths asts = do
  xs <- mapM (BS.readFile . Text.unpack) paths
  return $! SourceCache
    { _snippetCache = mempty
    , _sourceFiles =
        Map.fromList (zip [0..] (zip paths xs))
    , _sourceLines =
        Map.fromList (zip [0 .. length paths - 1]
                       (map (Vector.fromList . BS.split 0xa) xs))
    , _sourceAsts =
        asts
    }

lineSubrange ::
  Vector ByteString -> (Int, Int) -> Int -> Maybe (Int, Int)
lineSubrange xs (s1, n1) i =
  let
    ks = Vector.map (\x -> 1 + BS.length x) xs
    s2  = Vector.sum (Vector.take i ks)
    n2  = ks Vector.! i
  in
    if s1 + n1 < s2 || s1 > s2 + n2
    then Nothing
    else Just (s1 - s2, min (s2 + n2 - s1) n1)

readSolc :: FilePath -> IO (Maybe (Map Text SolcContract, SourceCache))
readSolc fp =
  (readJSON <$> readFile fp) >>=
    \case
      Nothing -> return Nothing
      Just (contracts, asts, sources) -> do
        sourceCache <- makeSourceCache sources asts
        return $! Just (contracts, sourceCache)

solidity :: Text -> Text -> IO (Maybe ByteString)
solidity contract src = do
  (json, path) <- solidity' src
  let Just (solc, _, _) = readJSON json
  return (solc ^? ix (path <> ":" <> contract) . creationCode)

force :: String -> Maybe a -> a
force s = fromMaybe (error s)

readJSON :: Text -> Maybe (Map Text SolcContract, Map Text Value, [Text])
readJSON json = do
  contracts <-
    f <$> (json ^? key "contracts" . _Object)
      <*> (fmap (fmap (^. _String)) $ json ^? key "sourceList" . _Array)
  sources <- toList . fmap (view _String) <$> json ^? key "sourceList" . _Array
  return (contracts, Map.fromList (HMap.toList asts), sources)
  where
    asts = fromMaybe (error "JSON lacks abstract syntax trees.") (json ^? key "sources" . _Object)
    f x y = Map.fromList . map (g y) . HMap.toList $ x
    g _ (s, x) =
      let
        theRuntimeCode = toCode (x ^?! key "bin-runtime" . _String)
        theCreationCode = toCode (x ^?! key "bin" . _String)
        abis =
          toList ((x ^?! key "abi" . _String) ^?! _Array)
      in (s, SolcContract {
        _runtimeCode      = theRuntimeCode,
        _creationCode     = theCreationCode,
        _runtimeCodehash  = keccak (stripBytecodeMetadata theRuntimeCode),
        _creationCodehash = keccak (stripBytecodeMetadata theCreationCode),
        _runtimeSrcmap    = force "internal error: srcmap-runtime" (makeSrcMaps (x ^?! key "srcmap-runtime" . _String)),
        _creationSrcmap   = force "internal error: srcmap" (makeSrcMaps (x ^?! key "srcmap" . _String)),
        _contractName = s,
        _contractAst =
          fromMaybe
            (error "JSON lacks abstract syntax trees.")
            (preview (ix (Text.split (== ':') s !! 0) . key "AST") asts),

        _constructorInputs =
          let
            isConstructor y =
              "constructor" == y ^?! key "type" . _String
          in
            case filter isConstructor abis of
              [abi] -> map parseMethodInput (toList (abi ^?! key "inputs" . _Array))
              [] -> [] -- default constructor has zero inputs
              _  -> error "strange: contract has multiple constructors",

        _abiMap       = Map.fromList $
          let
            relevant =
              filter (\y -> "function" == y ^?! key "type" . _String) abis
          in flip map relevant $
            \abi -> (
              abiKeccak (encodeUtf8 (signature abi)),
              Method
                { _methodName = abi ^?! key "name" . _String
                , _methodSignature = signature abi
                , _methodInputs =
                    map parseMethodInput
                      (toList (abi ^?! key "inputs" . _Array))
                , _methodOutput =
                    fmap parseMethodInput
                      (abi ^? key "outputs" . _Array . ix 0)
                }
            ),
        _eventMap     = Map.fromList $
          flip map (filter (\y -> "event" == y ^?! key "type" . _String)
                     . toList $ (x ^?! key "abi" . _String) ^?! _Array) $
            \abi ->
              ( keccak (encodeUtf8 (signature abi))
              , Event
                  (abi ^?! key "name" . _String)
                  (case abi ^?! key "anonymous" . _Bool of
                     True -> Anonymous
                     False -> NotAnonymous)
                  (map (\y -> ( force "internal error: type" (parseTypeName' y)
                              , if y ^?! key "indexed" . _Bool
                                then Indexed
                                else NotIndexed ))
                    (toList $ abi ^?! key "inputs" . _Array))
              )
      })

signature :: AsValue s => s -> Text
signature abi =
  case abi ^?! key "type" of
    "fallback" -> "<fallback>"
    _ ->
      fold [
        fromMaybe "<constructor>" (abi ^? key "name" . _String), "(",
        intercalate ","
          (map (\x -> x ^?! key "type" . _String)
            (toList $ abi ^?! key "inputs" . _Array)),
        ")"
      ]

-- Helper function to convert the fields to the desired type
parseTypeName' :: AsValue s => s -> Maybe AbiType
parseTypeName' x =
  parseTypeName
    (fromMaybe mempty $ x ^? key "components" . _Array . to parseComponents)
    (x ^?! key "type" . _String)
  where parseComponents = fmap $ snd . parseMethodInput

-- This actually can also parse a method output! :O
parseMethodInput :: AsValue s => s -> (Text, AbiType)
parseMethodInput x =
  ( x ^?! key "name" . _String
  , force "internal error: method type" (parseTypeName' x)
  )

toCode :: Text -> ByteString
toCode = fst . BS16.decode . encodeUtf8

solidity' :: Text -> IO (Text, Text)
solidity' src = withSystemTempFile "hevm.sol" $ \path handle -> do
  hClose handle
  writeFile path ("pragma solidity ^0.5.2;\n" <> src)
  x <- pack <$>
    readProcess
      "solc"
      ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi,ast", path]
      ""
  return (x, pack path)

-- When doing CREATE and passing constructor arguments, Solidity loads
-- the argument data via the creation bytecode, since there is no "calldata"
-- for CREATE.
--
-- This interferes with our ability to look up the current contract by
-- codehash, so we must somehow strip away this extra suffix. Luckily
-- we can detect the end of the actual bytecode by looking for the
-- "metadata hash". (Not 100% correct, but works in practice.)
--
-- Actually, we strip away the entire BZZR suffix too, because as long
-- as the codehash matches otherwise, we don't care if there is some
-- difference there.
stripBytecodeMetadata :: ByteString -> ByteString
stripBytecodeMetadata bc = if BS.length cl /= 2
                           then bc
                           else if BS.length h >= cl' && (isRight . deserialiseFromBytes decodeTerm $ fromStrict cbor)
                                then bc'
                                else bc
  where l = BS.length bc
        (h, cl) = BS.splitAt (l - 2) bc
        cl' = fromIntegral . runGet getWord16be . fromStrict $ cl
        (bc', cbor) = BS.splitAt (BS.length h - cl') h

-- | Every node in the AST has an ID, and other nodes reference those
-- IDs.  This function recurses through the tree looking for objects
-- with the "id" key and makes a big map from ID to value.
astIdMap :: Foldable f => f Value -> Map Int Value
astIdMap = foldMap f
  where
    f :: Value -> Map Int Value
    f (Array x) = foldMap f x
    f v@(Object x) =
      let t = foldMap f (HMap.elems x)
      in case HMap.lookup "id" x of
        Nothing         -> t
        Just (Number i) -> t <> Map.singleton (round i) v
        Just _          -> t
    f _ = mempty

astSrcMap :: Map Int Value -> (SrcMap -> Maybe Value)
astSrcMap astIds =
  \(SM i n f _ _)  -> Map.lookup (i, n, f) tmp
  where
    tmp :: Map (Int, Int, Int) Value
    tmp =
      ( Map.fromList
      . catMaybes
      . map (\v ->
          case preview (key "src" . _String) v of
            Just src ->
              case map (readMaybe . Text.unpack) (Text.split (== ':') src) of
                [Just i, Just n, Just f] ->
                  Just ((i, n, f), v)
                _ ->
                  error "strange formatting of src field"
            _ ->
              Nothing)
      . Map.elems
      $ astIds
      )
