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
  , abiMap
  , eventMap
  , contractName
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
  , stripConstructorArguments
  , lineSubrange
  , astIdMap
  , astSrcMap
) where

import EVM.ABI
import EVM.Keccak
import EVM.Types

import Control.Applicative
import Control.Lens         hiding (Indexed)
import Data.Aeson           (Value (..))
import Data.Aeson.Lens
import Data.ByteString      (ByteString)
import Data.Char            (isDigit, digitToInt)
import Data.Foldable
import Data.Map.Strict      (Map)
import Data.Maybe
import Data.Monoid
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
  } deriving (Show, Eq, Ord, Generic)

instance Monoid SourceCache where
  mempty = SourceCache mempty mempty mempty
  mappend (SourceCache _ _ _) (SourceCache _ _ _) = error "lol"

data JumpType = JumpInto | JumpFrom | JumpRegular
  deriving (Show, Eq, Ord, Generic)

data SrcMap = SM {
  srcMapOffset :: {-# UNPACK #-} Int,
  srcMapLength :: {-# UNPACK #-} Int,
  srcMapFile   :: {-# UNPACK #-} Int,
  srcMapJump   :: JumpType
} deriving (Show, Eq, Ord, Generic)

data SrcMapParseState
  = F1 [Int]
  | F2 Int [Int]
  | F3 Int Int [Int] !Int
  | F4 Int Int Int
  | F5 SrcMap
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
             . Text.foldl' (\x y -> go y x) (mempty, F1 [], SM 0 0 0 JumpRegular)
  where
    digits ds = digits' (0 :: Int) (0 :: Int) ds
    digits' !x _ []      = x
    digits' !x !n (d:ds) = digits' (x + d * 10 ^ n) (n + 1) ds

    done (xs, s, p) = let (xs', _, _) = go ';' (xs, s, p) in xs'

    go ':' (xs, F1 [], p@(SM a _ _ _))       = (xs, F2 a [], p)
    go ':' (xs, F1 ds, p)                    = (xs, F2 (digits ds) [], p)
    go d   (xs, F1 ds, p) | isDigit d        = (xs, F1 (digitToInt d : ds), p)
    go ';' (xs, F1 [], p)                    = (xs |> p, F1 [], p)
    go ';' (xs, F1 ds, SM _ b c d)           = let p' = SM (digits ds) b c d in
                                               (xs |> p', F1 [], p')

    go d   (xs, F2 a ds, p) | isDigit d      = (xs, F2 a (digitToInt d : ds), p)
    go ':' (xs, F2 a [], p@(SM _ b _ _))     = (xs, F3 a b [] 1, p)
    go ':' (xs, F2 a ds, p)                  = (xs, F3 a (digits ds) [] 1, p)
    go ';' (xs, F2 a [], SM _ b c d)         = let p' = SM a b c d in (xs |> p', F1 [], p')
    go ';' (xs, F2 a ds, SM _ _ c d)         = let p' = SM a (digits ds) c d in
                                               (xs |> p', F1 [], p')

    go d   (xs, F3 a b ds k, p) | isDigit d  = (xs, F3 a b (digitToInt d : ds) k, p)
    go '-' (xs, F3 a b [] _, p)              = (xs, F3 a b [] (-1), p)
    go ':' (xs, F3 a b [] _, p@(SM _ _ c _)) = (xs, F4 a b c, p)
    go ':' (xs, F3 a b ds k, p)              = (xs, F4 a b (k * digits ds), p)
    go ';' (xs, F3 a b [] _, SM _ _ c d)     = let p' = SM a b c d in (xs |> p', F1 [], p')
    go ';' (xs, F3 a b ds k, SM _ _ _ d)     = let p' = SM a b (k * digits ds) d in
                                               (xs |> p', F1 [], p')

    go 'i' (xs, F4 a b c, p)                 = (xs, F5 (SM a b c JumpInto), p)
    go 'o' (xs, F4 a b c, p)                 = (xs, F5 (SM a b c JumpFrom), p)
    go '-' (xs, F4 a b c, p)                 = (xs, F5 (SM a b c JumpRegular), p)
    go ';' (xs, F5 s, _)                     = (xs |> s, F1 [], s)

    go _ (xs, _, p)                          = (xs, Fe, p)

makeSourceCache :: [Text] -> IO SourceCache
makeSourceCache paths = do
  xs <- mapM (BS.readFile . Text.unpack) paths
  return $! SourceCache {
    _snippetCache = mempty,
    _sourceFiles =
      Map.fromList (zip [0..] (zip paths xs)),
    _sourceLines =
      Map.fromList (zip [0 .. length paths - 1]
                     (map (Vector.fromList . BS.split 0xa) xs))

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
      Just (contracts, sources) -> do
        sourceCache <- makeSourceCache sources
        return $! Just (contracts, sourceCache)

solidity :: Text -> Text -> IO (Maybe ByteString)
solidity contract src = do
  (json, path) <- solidity' src
  let Just (solc, _) = readJSON json
  return (solc ^? ix (path <> ":" <> contract) . creationCode)

readJSON :: Text -> Maybe (Map Text SolcContract, [Text])
readJSON json = do
  contracts <-
    f <$> (json ^? key "contracts" . _Object)
      <*> (fmap (fmap (^. _String)) $ json ^? key "sourceList" . _Array)
  sources <- toList . fmap (view _String) <$> json ^? key "sourceList" . _Array
  return (contracts, sources)
  where
    asts = fromMaybe (error "JSON lacks abstract syntax trees.") (json ^? key "sources")
    f x y = Map.fromList . map (g y) . HMap.toList $ x
    g _ (s, x) =
      let
        theRuntimeCode = toCode (x ^?! key "bin-runtime" . _String)
        theCreationCode = toCode (x ^?! key "bin" . _String)
      in (s, SolcContract {
        _runtimeCode      = theRuntimeCode,
        _creationCode     = theCreationCode,
        _runtimeCodehash  = keccak theRuntimeCode,
        _creationCodehash = keccak theCreationCode,
        _runtimeSrcmap    = fromJust (makeSrcMaps (x ^?! key "srcmap-runtime" . _String)),
        _creationSrcmap   = fromJust (makeSrcMaps (x ^?! key "srcmap" . _String)),
        _contractName = s,
        _contractAst =
          fromMaybe
            (error "JSON lacks abstract syntax trees.")
            (preview (key (Text.split (== ':') s !! 0) . key "AST") asts),
        _abiMap       = Map.fromList $
          let
            abis =
              toList ((x ^?! key "abi" . _String) ^?! _Array)
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
                  (map (\y -> ( fromJust (parseTypeName (y ^?! key "type" . _String))
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

-- This actually can also parse a method output! :O
parseMethodInput :: (Show s, AsValue s) => s -> (Text, AbiType)
parseMethodInput x =
  ( x ^?! key "name" . _String
  , fromJust (parseTypeName (x ^?! key "type" . _String))
  )

toCode :: Text -> ByteString
toCode = fst . BS16.decode . encodeUtf8

solidity' :: Text -> IO (Text, Text)
solidity' src = withSystemTempFile "hevm.sol" $ \path handle -> do
  hClose handle
  writeFile path ("pragma solidity ^0.4.8;\n" <> src)
  x <- pack <$>
    readProcess
      "solc"
      ["--combined-json=bin-runtime,bin,srcmap,srcmap-runtime,abi", path]
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
stripConstructorArguments :: ByteString -> ByteString
stripConstructorArguments bs =
  let (a, b) = BS.breakSubstring bzzrPrefix (BS.reverse bs)
  in BS.reverse b <> BS.take (32 + 2) (BS.reverse a)

bzzrPrefix :: ByteString
bzzrPrefix =
  -- a1 65 "bzzr0" 0x58 0x20
  BS.reverse $ BS.pack [0xa1, 0x65, 98, 122, 122, 114, 48, 0x58, 0x20]

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
  \(SM i n f _)  -> Map.lookup (i, n, f) tmp
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
