{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}

module EVM.Dapp where

import EVM (Trace, traceCode, traceOpIx, ContractCode(..), Contract)
import EVM.ABI (Event, AbiType)
import EVM.Debug (srcMapCodePos)
import EVM.Solidity
import EVM.Types (W256, abiKeccak, keccak, Buffer(..), Addr)
import EVM.Concrete

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Aeson (Value)
import Data.Bifunctor (first)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map, toList, elems)
import Data.List (sort)
import Data.Maybe (isJust, fromJust)
import Data.Word (Word32)

import Control.Arrow ((>>>))
import Control.Lens

import Data.List (find)
import qualified Data.Map        as Map
import qualified Data.Sequence   as Seq
import qualified Text.Regex.TDFA as Regex

data DappInfo = DappInfo
  { _dappRoot       :: FilePath
  , _dappSolcByName :: Map Text SolcContract
  , _dappSolcByHash :: Map W256 (CodeType, SolcContract)
  , _dappSolcByCode :: [(Code, SolcContract)] -- for contracts with `immutable` vars.
  , _dappSources    :: SourceCache
  , _dappUnitTests  :: [(Text, [(Test, [AbiType])])]
  , _dappAbiMap     :: Map Word32 Method
  , _dappEventMap   :: Map W256 Event
  , _dappAstIdMap   :: Map Int Value
  , _dappAstSrcMap  :: SrcMap -> Maybe Value
  }

-- | bytecode modulo immutables, to identify contracts
data Code =
  Code {
    raw :: ByteString,
    immutableLocations :: [Reference]
  }
  deriving Show

data DappContext = DappContext
  { _contextInfo :: DappInfo
  , _contextEnv  :: Map Addr Contract
  }

data Test = ConcreteTest Text | SymbolicTest Text | ExploreTest Text

makeLenses ''DappInfo
makeLenses ''DappContext

instance Show Test where
  show t = unpack $ extractSig t

dappInfo
  :: FilePath -> Map Text SolcContract -> SourceCache -> DappInfo
dappInfo root solcByName sources =
  let
    solcs = Map.elems solcByName
    astIds = astIdMap $ snd <$> toList (view sourceAsts sources)
    immutables = filter ((/=) mempty . _immutableReferences) solcs

  in DappInfo
    { _dappRoot = root
    , _dappUnitTests = findAllUnitTests solcs
    , _dappSources = sources
    , _dappSolcByName = solcByName
    , _dappSolcByHash =
        let
          f g k = Map.fromList [(view g x, (k, x)) | x <- solcs]
        in
          mappend
           (f runtimeCodehash  Runtime)
           (f creationCodehash Creation)
      -- contracts with immutable locations can't be id by hash
    , _dappSolcByCode =
      [(Code (_runtimeCode x) (concat $ elems $ _immutableReferences x), x) | x <- immutables]
      -- Sum up the ABI maps from all the contracts.
    , _dappAbiMap   = mconcat (map (view abiMap) solcs)
    , _dappEventMap = mconcat (map (view eventMap) solcs)

    , _dappAstIdMap  = astIds
    , _dappAstSrcMap = astSrcMap astIds
    }

emptyDapp :: DappInfo
emptyDapp = dappInfo "" mempty (SourceCache mempty mempty mempty)

-- Dapp unit tests are detected by searching within abi methods
-- that begin with "test" or "prove", that are in a contract with
-- the "IS_TEST()" abi marker, for a given regular expression.
--
-- The regex is matched on the full test method name, including path
-- and contract, i.e. "path/to/file.sol:TestContract.test_name()".
--
-- Tests beginning with "test" are interpreted as concrete tests, whereas
-- tests beginning with "prove" are interpreted as symbolic tests.

unitTestMarkerAbi :: Word32
unitTestMarkerAbi = abiKeccak (encodeUtf8 "IS_TEST()")

findAllUnitTests :: [SolcContract] -> [(Text, [(Test, [AbiType])])]
findAllUnitTests = findUnitTests ".*:.*\\.(test|prove|invariant).*"

mkTest :: Text -> Maybe Test
mkTest sig
  | "test" `isPrefixOf` sig = Just (ConcreteTest sig)
  | "prove" `isPrefixOf` sig = Just (SymbolicTest sig)
  | "invariant" `isPrefixOf` sig = Just (ExploreTest sig)
  | otherwise = Nothing

regexMatches :: Text -> Text -> Bool
regexMatches regexSource =
  let
    compOpts =
      Regex.defaultCompOpt { Regex.lastStarGreedy = True }
    execOpts =
      Regex.defaultExecOpt { Regex.captureGroups = False }
    regex = Regex.makeRegexOpts compOpts execOpts (unpack regexSource)
  in
    Regex.matchTest regex . Seq.fromList . unpack

findUnitTests :: Text -> ([SolcContract] -> [(Text, [(Test, [AbiType])])])
findUnitTests match =
  concatMap $ \c ->
    case preview (abiMap . ix unitTestMarkerAbi) c of
      Nothing -> []
      Just _  ->
        let testNames = unitTestMethodsFiltered (regexMatches match) c
        in [(view contractName c, testNames) | not (null testNames)]

unitTestMethodsFiltered :: (Text -> Bool) -> (SolcContract -> [(Test, [AbiType])])
unitTestMethodsFiltered matcher c =
  let
    testName method = (view contractName c) <> "." <> (extractSig (fst method))
  in
    filter (matcher . testName) (unitTestMethods c)

unitTestMethods :: SolcContract -> [(Test, [AbiType])]
unitTestMethods =
  view abiMap
  >>> Map.elems
  >>> map (\f -> (mkTest $ view methodSignature f, snd <$> view methodInputs f))
  >>> filter (isJust . fst)
  >>> fmap (first fromJust)

extractSig :: Test -> Text
extractSig (ConcreteTest sig) = sig
extractSig (SymbolicTest sig) = sig
extractSig (ExploreTest  sig) = sig

traceSrcMap :: DappInfo -> Trace -> Maybe SrcMap
traceSrcMap dapp trace =
  let
    h = view traceCode trace
    i = view traceOpIx trace
  in srcMap dapp h i

srcMap :: DappInfo -> ContractCode -> Int -> Maybe SrcMap
srcMap dapp code opIndex = do
  sol <- lookupCode code dapp
  case code of
    (InitCode _) ->
      preview (creationSrcmap . ix opIndex) sol
    (RuntimeCode _) ->
      preview (runtimeSrcmap . ix opIndex) sol

lookupCode :: ContractCode -> DappInfo -> Maybe SolcContract
lookupCode (InitCode (SymbolicBuffer _)) _ = Nothing -- TODO: srcmaps for symbolic bytecode
lookupCode (RuntimeCode (SymbolicBuffer _)) _ = Nothing -- TODO: srcmaps for symbolic bytecode
lookupCode (InitCode (ConcreteBuffer c)) a =
  snd <$> preview (dappSolcByHash . ix (keccak (stripBytecodeMetadata c))) a
lookupCode (RuntimeCode (ConcreteBuffer c)) a =
  case snd <$> preview (dappSolcByHash . ix (keccak (stripBytecodeMetadata c))) a of
    Just x -> return x
    Nothing -> snd <$> find (compareCode c . fst) (view dappSolcByCode a)

compareCode :: ByteString -> Code -> Bool
compareCode raw (Code template locs) =
  let holes' = sort [(start, len) | (Reference start len) <- locs]
      insert at' len' bs = writeMemory (BS.replicate len' 0) (fromIntegral len') 0 (fromIntegral at') bs
      refined = foldr (\(start, len) acc -> insert start len acc) raw holes'
  in BS.length raw == BS.length template && template == refined

showTraceLocation :: DappInfo -> Trace -> Either Text Text
showTraceLocation dapp trace =
  case traceSrcMap dapp trace of
    Nothing -> Left "<no source map>"
    Just sm ->
      case srcMapCodePos (view dappSources dapp) sm of
        Nothing -> Left "<source not found>"
        Just (fileName, lineIx) ->
          Right (fileName <> ":" <> pack (show lineIx))
