{-# Language TemplateHaskell #-}

module EVM.Dapp where

import EVM.Solidity (SolcContract, CodeType (..), SourceCache)
import EVM.Solidity (runtimeCodehash, creationCodehash, abiMap)
import EVM.Solidity (contractName)
import EVM.Keccak (abiKeccak)
import EVM.Types (W256)

import Data.Text (Text, isPrefixOf)
import Data.Text.Encoding (encodeUtf8)
import Data.Map (Map)
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
