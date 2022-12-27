module EVM.StorageLayout where

-- Figures out the layout of storage slots for Solidity contracts.

import EVM.Dapp (DappInfo, dappAstSrcMap, dappAstIdMap)
import EVM.Solidity (SolcContract, creationSrcmap, SlotType(..))
import EVM.ABI (AbiType (..), parseTypeName)

import Data.Aeson (Value (..))
import Data.Aeson.Lens

import Control.Lens

import Data.Text (Text, unpack, pack, words)

import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.List.NonEmpty as NonEmpty

import qualified Data.Sequence as Seq

import Prelude hiding (words)

-- A contract has all the slots of its inherited contracts.
--
-- The slot order is determined by the inheritance linearization order,
-- so we first have to calculate that.
--
-- This information is available in the abstract syntax tree.

findContractDefinition :: DappInfo -> SolcContract -> Maybe Value
findContractDefinition dapp solc =
  -- The first source mapping in the contract's creation code
  -- corresponds to the source field of the contract definition.
  case Seq.viewl (view creationSrcmap solc) of
    firstSrcMap Seq.:< _ ->
      (view dappAstSrcMap dapp) firstSrcMap
    _ ->
      Nothing

storageLayout :: DappInfo -> SolcContract -> [Text]
storageLayout dapp solc =
  let
    root :: Value
    root =
      fromMaybe Null
        (findContractDefinition dapp solc)
  in
    case preview ( key "attributes"
                 . key "linearizedBaseContracts"
                 . _Array
                 ) root of
      Nothing ->
        []
      Just ((reverse . toList) -> linearizedBaseContracts) ->
        flip concatMap linearizedBaseContracts
          (\case
             Number i -> fromMaybe (error "malformed AST JSON") $
               storageVariablesForContract =<<
                 preview (dappAstIdMap . ix (floor i)) dapp
             _ ->
               error "malformed AST JSON")

storageVariablesForContract :: Value -> Maybe [Text]
storageVariablesForContract node = do
  name <- preview (ix "attributes" . key "name" . _String) node
  vars <-
    fmap
      (filter isStorageVariableDeclaration . toList)
      (preview (ix "children" . _Array) node)

  pure . flip map vars $
    \x ->
      case preview (key "attributes" . key "name" . _String) x of
        Just variableName ->
          mconcat
            [ variableName
            , " (", name, ")"
            , "\n", "  Type: "
            , pack $ show (slotTypeForDeclaration x)
            ]
        Nothing ->
          error "malformed variable declaration"

nodeIs :: Text -> Value -> Bool
nodeIs t x = isSourceNode && hasRightName
  where
    isSourceNode =
      isJust (preview (key "src") x)
    hasRightName =
      Just t == preview (key "name" . _String) x

isStorageVariableDeclaration :: Value -> Bool
isStorageVariableDeclaration x =
  nodeIs "VariableDeclaration" x
    && preview (key "attributes" . key "constant" . _Bool) x /= Just True

slotTypeForDeclaration :: Value -> SlotType
slotTypeForDeclaration node =
  case toList <$> preview (key "children" . _Array) node of
    Just (x:_) ->
      grokDeclarationType x
    _ ->
      error "malformed AST"

grokDeclarationType :: Value -> SlotType
grokDeclarationType x =
  case preview (key "name" . _String) x of
    Just "Mapping" ->
      case preview (key "children" . _Array) x of
        Just (toList -> xs) ->
          grokMappingType xs
        _ ->
          error "malformed AST"
    Just _ ->
      StorageValue (grokValueType x)
    _ ->
      error ("malformed AST " ++ show x)

grokMappingType :: [Value] -> SlotType
grokMappingType [s, t] =
  case (grokDeclarationType s, grokDeclarationType t) of
    (StorageValue s', StorageMapping t' x) ->
      StorageMapping (NonEmpty.cons s' t') x
    (StorageValue s', StorageValue t') ->
      StorageMapping (pure s') t'
    (StorageMapping _ _, _) ->
      error "unexpected mapping as mapping key"
grokMappingType _ =
  error "unexpected AST child count for mapping"

grokValueType :: Value -> AbiType
grokValueType x =
  case ( preview (key "name" . _String) x
       , preview (key "children" . _Array) x
       , preview (key "attributes" . key "type" . _String) x
       ) of
    (Just "ElementaryTypeName", _, Just typeName) ->
      fromMaybe (error ("ungrokked value type: " ++ show typeName))
        (parseTypeName mempty (head (words typeName)))
    (Just "UserDefinedTypeName", _, _) ->
      AbiAddressType
    (Just "ArrayTypeName", fmap toList -> Just [t], _)->
      AbiArrayDynamicType (grokValueType t)
    (Just "ArrayTypeName", fmap toList -> Just [t, n], _)->
      case ( preview (key "name" . _String) n
           , preview (key "attributes" . key "value" . _String) n
           ) of
        (Just "Literal", Just ((read . unpack) -> i)) ->
          AbiArrayType i (grokValueType t)
        _ ->
          error "malformed AST"
    _ ->
      error ("unknown value type " ++ show x)
