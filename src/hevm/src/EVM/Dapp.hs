{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}

module EVM.Dapp where

import EVM (Trace, traceContract, traceOpIx, ContractCode(..), Contract(..), codehash, contractcode)
import EVM.ABI (Event, AbiType, SolError)
import EVM.Debug (srcMapCodePos)
import EVM.Solidity
import EVM.Types (W256, abiKeccak, keccak', Addr, regexMatches, unlit, unlitByte)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Aeson (Value)
import Data.Bifunctor (first)
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map, toList, elems)
import Data.List (sort)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Word (Word32)
import EVM.Concrete

import Control.Arrow ((>>>))
import Control.Lens

import Data.List (find)
import qualified Data.Map        as Map

data DappInfo = DappInfo
  { _dappRoot       :: FilePath
  , _dappSolcByName :: Map Text SolcContract
  , _dappSolcByHash :: Map W256 (CodeType, SolcContract)
  , _dappSolcByCode :: [(Code, SolcContract)] -- for contracts with `immutable` vars.
  , _dappSources    :: SourceCache
  , _dappUnitTests  :: [(Text, [(Test, [AbiType])])]
  , _dappAbiMap     :: Map Word32 Method
  , _dappEventMap   :: Map W256 Event
  , _dappErrorMap   :: Map W256 SolError
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

data Test = ConcreteTest Text | SymbolicTest Text | InvariantTest Text

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
    , _dappErrorMap = mconcat (map (view errorMap) solcs)

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
  | "invariant" `isPrefixOf` sig = Just (InvariantTest sig)
  | otherwise = Nothing

findUnitTests :: Text -> ([SolcContract] -> [(Text, [(Test, [AbiType])])])
findUnitTests match =
  concatMap $ \c ->
    case preview (abiMap . ix unitTestMarkerAbi) c of
      Nothing -> []
      Just _  ->
        let testNames = unitTestMethodsFiltered (regexMatches match) c
        in [(view contractName c, testNames) | not (BS.null (view runtimeCode c)) && not (null testNames)]

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
extractSig (InvariantTest sig) = sig

traceSrcMap :: DappInfo -> Trace -> Maybe SrcMap
traceSrcMap dapp trace =
  let
    h = view traceContract trace
    i = view traceOpIx trace
  in srcMap dapp h i

srcMap :: DappInfo -> Contract -> Int -> Maybe SrcMap
srcMap dapp contr opIndex = do
  sol <- findSrc contr dapp
  case view contractcode contr of
    (InitCode _ _) ->
      preview (creationSrcmap . ix opIndex) sol
    (RuntimeCode _) ->
      preview (runtimeSrcmap . ix opIndex) sol

findSrc :: Contract -> DappInfo -> Maybe SolcContract
findSrc c dapp = do
  hash <- unlit (view codehash c)
  case preview (dappSolcByHash . ix hash) dapp of
    Just (_, v) -> Just v
    Nothing -> lookupCode (view contractcode c) dapp


lookupCode :: ContractCode -> DappInfo -> Maybe SolcContract
lookupCode (InitCode c _) a =
  snd <$> preview (dappSolcByHash . ix (keccak' (stripBytecodeMetadata c))) a
lookupCode (RuntimeCode c) a = let
    code = BS.pack $ mapMaybe unlitByte c
  in case snd <$> preview (dappSolcByHash . ix (keccak' (stripBytecodeMetadata code))) a of
    Just x -> return x
    Nothing -> snd <$> find (compareCode code . fst) (view dappSolcByCode a)

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
