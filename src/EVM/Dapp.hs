{-# Language TemplateHaskell #-}

module EVM.Dapp where

import EVM (Trace, traceCodehash, traceOpIx)
import EVM.Debug (srcMapCodePos)
import EVM.Keccak (abiKeccak)
import EVM.Machine (Machine)
import EVM.Solidity (SolcContract, CodeType (..), SourceCache, SrcMap)
import EVM.Solidity (contractName)
import EVM.Solidity (runtimeCodehash, creationCodehash, abiMap)
import EVM.Solidity (runtimeSrcmap, creationSrcmap)
import EVM.Types (W256)

import Data.Text (Text, isPrefixOf, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Word (Word32)
import Data.List (sort)

import Control.Lens

import qualified Data.Map as Map

data DappInfo = DappInfo
  { _dappRoot       :: FilePath
  , _dappSolcByName :: Map Text SolcContract
  , _dappSolcByHash :: Map W256 (CodeType, SolcContract)
  , _dappSources    :: SourceCache
  , _dappUnitTests  :: [(Text, [Text])]
  } deriving Eq

makeLenses ''DappInfo

dappInfo
  :: FilePath -> Map Text SolcContract -> SourceCache -> DappInfo
dappInfo root solcByName sources =
  let
    solcs = Map.elems solcByName

  in DappInfo
    { _dappRoot = root
    , _dappUnitTests = findUnitTests solcs
    , _dappSources = sources
    , _dappSolcByName = solcByName
    , _dappSolcByHash =
        let
          f g k = Map.fromList [(view g x, (k, x)) | x <- solcs]
        in
          mappend
           (f runtimeCodehash  Runtime)
           (f creationCodehash Creation)
    }

unitTestMarkerAbi :: Word32
unitTestMarkerAbi = abiKeccak (encodeUtf8 "IS_TEST()")

findUnitTests :: [SolcContract] -> [(Text, [Text])]
findUnitTests =
  concatMap $ \c ->
    case preview (abiMap . ix unitTestMarkerAbi) c of
      Nothing -> []
      Just _  ->
        let testNames = unitTestMethods c
        in if null testNames
           then []
           else [(view contractName c, testNames)]

unitTestMethods :: SolcContract -> [Text]
unitTestMethods c =
  sort (filter ("test" `isPrefixOf`) (Map.elems (view abiMap c)))

traceSrcMap :: Machine e => DappInfo -> Trace e -> Maybe SrcMap
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

showTraceLocation :: Machine e => DappInfo -> Trace e -> Either Text Text
showTraceLocation dapp trace =
  case traceSrcMap dapp trace of
    Nothing -> Left "<no source map>"
    Just sm ->
      case srcMapCodePos (view dappSources dapp) sm of
        Nothing -> Left "<source not found>"
        Just (fileName, lineIx) ->
          Right (fileName <> ":" <> pack (show lineIx))
