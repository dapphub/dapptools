{-# Language TemplateHaskell #-}

module EVM.Dapp where

import EVM (Trace, traceCodehash, traceOpIx)
import EVM.ABI (Event, AbiType)
import EVM.Debug (srcMapCodePos)
import EVM.Keccak (abiKeccak)
import EVM.Solidity (SolcContract, CodeType (..), SourceCache, SrcMap)
import EVM.Solidity (contractName, methodInputs)
import EVM.Solidity (runtimeCodehash, creationCodehash, abiMap)
import EVM.Solidity (runtimeSrcmap, creationSrcmap, eventMap)
import EVM.Solidity (methodSignature, contractAst, astIdMap, astSrcMap)
import EVM.Types (W256)

import Data.Aeson (Value)
import Data.Text (Text, isPrefixOf, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Word (Word32)

import Control.Arrow ((>>>))
import Control.Lens

import qualified Data.Map as Map

data DappInfo = DappInfo
  { _dappRoot       :: FilePath
  , _dappSolcByName :: Map Text SolcContract
  , _dappSolcByHash :: Map W256 (CodeType, SolcContract)
  , _dappSources    :: SourceCache
  , _dappUnitTests  :: [(Text, [(Text, [AbiType])])]
  , _dappEventMap   :: Map W256 Event
  , _dappAstIdMap   :: Map Int Value
  , _dappAstSrcMap  :: (SrcMap -> Maybe Value)
  }

makeLenses ''DappInfo

dappInfo
  :: FilePath -> Map Text SolcContract -> SourceCache -> DappInfo
dappInfo root solcByName sources =
  let
    solcs = Map.elems solcByName
    astIds = astIdMap (map (view contractAst) solcs)

  in DappInfo
    { _dappRoot = root
    , _dappUnitTests = findUnitTests ("test" `isPrefixOf`) solcs
    , _dappSources = sources
    , _dappSolcByName = solcByName
    , _dappSolcByHash =
        let
          f g k = Map.fromList [(view g x, (k, x)) | x <- solcs]
        in
          mappend
           (f runtimeCodehash  Runtime)
           (f creationCodehash Creation)

    , _dappEventMap =
        -- Sum up the event ABI maps from all the contracts.
        mconcat (map (view eventMap) solcs)

    , _dappAstIdMap  = astIds
    , _dappAstSrcMap = astSrcMap astIds
    }

unitTestMarkerAbi :: Word32
unitTestMarkerAbi = abiKeccak (encodeUtf8 "IS_TEST()")

findUnitTests :: (Text -> Bool) -> ([SolcContract] -> [(Text, [(Text, [AbiType])])])
findUnitTests matcher =
  concatMap $ \c ->
    case preview (abiMap . ix unitTestMarkerAbi) c of
      Nothing -> []
      Just _  ->
        let testNames = (unitTestMethods matcher) c
        in if null testNames
           then []
           else [(view contractName c, testNames)]

unitTestMethods :: (Text -> Bool) -> (SolcContract -> [(Text, [AbiType])])
unitTestMethods matcher =
  view abiMap
    >>> Map.elems
    >>> map (\f -> (view methodSignature f, fmap snd $ view methodInputs f))
    >>> filter (matcher . fst)

traceSrcMap :: DappInfo -> Trace -> Maybe SrcMap
traceSrcMap dapp trace =
  let
    h = view traceCodehash trace
    i = view traceOpIx trace
  in case preview (dappSolcByHash . ix h) dapp of
    Nothing ->
      Nothing
    Just (Creation, solc) ->
      preview (creationSrcmap . ix i) solc
    Just (Runtime, solc) ->
      preview (runtimeSrcmap . ix i) solc

showTraceLocation :: DappInfo -> Trace -> Either Text Text
showTraceLocation dapp trace =
  case traceSrcMap dapp trace of
    Nothing -> Left "<no source map>"
    Just sm ->
      case srcMapCodePos (view dappSources dapp) sm of
        Nothing -> Left "<source not found>"
        Just (fileName, lineIx) ->
          Right (fileName <> ":" <> pack (show lineIx))
