{-# Language DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module EVM.Format where

import Prelude hiding (Word)

import EVM (VM, cheatCode, traceForest, traceData, Error (..))
import EVM (Trace, TraceData (..), Log (..), Query (..), FrameContext (..))
import EVM.Dapp (DappInfo, dappSolcByHash, showTraceLocation, dappEventMap)
import EVM.Concrete (Word (..), wordValue)
import EVM.Symbolic (maybeLitWord, Buffer(..), len)
import EVM.Types (W256 (..), num)
import EVM.ABI (AbiValue (..), Event (..), AbiType (..))
import EVM.ABI (Indexed (NotIndexed), getAbiSeq, getAbi)
import EVM.ABI (parseTypeName)
import EVM.Solidity (SolcContract, contractName, abiMap)
import EVM.Solidity (methodOutput, methodSignature)

import Control.Arrow ((>>>))
import Control.Lens (view, preview, ix, _2, to, _Just)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.DoubleWord (signedWord)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text (dropEnd, splitOn)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Tree.View (showTree)
import Data.Vector (Vector, fromList)

import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Text as Text

data Signedness = Signed | Unsigned
  deriving (Show)

showDec :: Signedness -> W256 -> Text
showDec signed (W256 w) =
  let
    i = case signed of
          Signed   -> num (signedWord w)
          Unsigned -> num w
  in
    if i == num cheatCode
    then "<hevm cheat address>"
    else if (i :: Integer) == 2 ^ (256 :: Integer) - 1
    then "MAX_UINT256"
    else Text.pack (show (i :: Integer))

showWordExact :: Word -> Text
showWordExact (C _ (W256 w)) = humanizeInteger w

humanizeInteger :: (Num a, Integral a, Show a) => a -> Text
humanizeInteger =
  Text.intercalate ","
  . reverse
  . map Text.reverse
  . Text.chunksOf 3
  . Text.reverse
  . Text.pack
  . show

-- TODO: make polymorphic
showAbiValues :: Vector AbiValue -> Text
showAbiValues vs =
  "(" <> intercalate ", " (toList (fmap showAbiValue vs)) <> ")"

showAbiArray :: Vector AbiValue -> Text
showAbiArray vs =
  "[" <> intercalate ", " (toList (fmap showAbiValue vs)) <> "]"

showAbiValue :: AbiValue -> Text
showAbiValue (AbiUInt _ w) =
  pack $ show w
showAbiValue (AbiInt _ w) =
  pack $ show w
showAbiValue (AbiBool b) =
  pack $ show b
showAbiValue (AbiAddress w160) =
  pack $ "0x" ++ (showHex w160 "")
showAbiValue (AbiBytes _ bs) =
  formatBytes bs
showAbiValue (AbiBytesDynamic bs) =
  formatBinary bs
showAbiValue (AbiString bs) =
  formatQString bs
showAbiValue (AbiArray _ _ xs) =
  showAbiArray xs
showAbiValue (AbiArrayDynamic _ xs) =
  showAbiArray xs
showAbiValue (AbiTuple v) =
  showAbiValues v

isPrintable :: ByteString -> Bool
isPrintable =
  decodeUtf8' >>>
    either (const False)
      (Text.all (not . Char.isControl))

formatBytes :: ByteString -> Text
formatBytes b =
  let (s, _) = BS.spanEnd (== 0) b
  in
    if isPrintable s
    then formatQString s
    else formatBinary b

formatSBytes :: Buffer -> Text
formatSBytes (SymbolicBuffer b) = "<" <> pack (show (length b)) <> " symbolic bytes>"
formatSBytes (ConcreteBuffer b) = formatBytes b

formatQString :: ByteString -> Text
formatQString = pack . show

formatString :: ByteString -> Text
formatString bs = decodeUtf8 (fst (BS.spanEnd (== 0) bs))

formatSString :: Buffer -> Text
formatSString (SymbolicBuffer bs) = "<" <> pack (show (length bs)) <> " symbolic bytes (string)>"
formatSString (ConcreteBuffer bs) = formatString bs

formatBinary :: ByteString -> Text
formatBinary =
  (<>) "0x" . decodeUtf8 . toStrict . toLazyByteString . byteStringHex

formatSBinary :: Buffer -> Text
formatSBinary (SymbolicBuffer bs) = "<" <> pack (show (length bs)) <> " symbolic bytes>"
formatSBinary (ConcreteBuffer bs) = formatBinary bs

showTraceTree :: DappInfo -> VM -> Text
showTraceTree dapp =
  traceForest
    >>> fmap (fmap (unpack . showTrace dapp))
    >>> concatMap showTree
    >>> pack

showTrace :: DappInfo -> Trace -> Text
showTrace dapp trace =
  let
    pos =
      case showTraceLocation dapp trace of
        Left x -> " \x1b[90m" <> x <> "\x1b[0m"
        Right x -> " \x1b[90m(" <> x <> ")\x1b[0m"
  in case view traceData trace of
    EventTrace (Log _ bytes topics) ->
      case topics of
        [] ->
          mconcat
            [ "\x1b[36m"
            , "log0("
            , formatSBinary bytes
            , ")"
            , "\x1b[0m"
            ] <> pos
        (topic:_) ->
          let unknownTopic =                    -- todo: catch ds-note
                   mconcat
                     [ "\x1b[36m"
                     , "log" <> (pack (show (length topics))) <> "("
                     , formatSBinary bytes <> ", "
                     , intercalate ", " (map (pack . show) topics) <> ")"
                     , "\x1b[0m"
                     ] <> pos

          in case maybeLitWord topic of
            Just top -> case Map.lookup (wordValue top) (view dappEventMap dapp) of
                 Just (Event name _ types) ->
                   mconcat
                     [ "\x1b[36m"
                     , name
                     , showValues [t | (t, NotIndexed) <- types] bytes
                     -- todo: show indexed
                     , "\x1b[0m"
                     ] <> pos
                 Nothing -> unknownTopic
            Nothing -> unknownTopic

    QueryTrace q ->
      case q of
        PleaseFetchContract addr _ ->
          "fetch contract " <> pack (show addr) <> pos
        PleaseFetchSlot addr slot _ ->
          "fetch storage slot " <> pack (show slot) <> " from " <> pack (show addr) <> pos
        PleaseAskSMT _ _ _ ->
          "ask smt" <> pos
    ErrorTrace e ->
      case e of
        Revert out ->
          "\x1b[91merror\x1b[0m " <> "Revert " <> showError out <> pos
        _ ->
          "\x1b[91merror\x1b[0m " <> pack (show e) <> pos

    ReturnTrace out (CallContext _ _ hash (Just abi) _ _ _) ->
      case getAbiMethodOutput dapp hash abi of
        Nothing ->
          "← " <> formatSBinary out
        Just (_, t) ->
          "← " <> pack (show t) <> " " <> showValue t out
    ReturnTrace out (CallContext {}) ->
      "← " <> formatSBinary out
    ReturnTrace out (CreationContext {}) ->
      "← " <> pack (show (len out)) <> " bytes of code"

    EntryTrace t ->
      t
    FrameTrace (CreationContext hash _ _ ) ->
      "create " <> maybeContractName (preview (dappSolcByHash . ix hash . _2) dapp) <> pos
    FrameTrace (CallContext _ _ hash abi calldata _ _) ->
      case preview (dappSolcByHash . ix hash . _2) dapp of
        Nothing ->
          "call [unknown]" <> pos
        Just solc ->
          "call "
            <> "\x1b[1m"
            <> view (contractName . to contractNamePart) solc
            <> "::"
            <> maybe "[fallback function]"
                 (fromMaybe "[unknown method]" . maybeAbiName solc)
                 abi
            <> maybe ("(" <> formatSBinary calldata <> ")")
                 -- todo: if unknown method, then just show raw calldata
                 (\x -> showCall (catMaybes x) calldata)
                 (abi >>= fmap getAbiTypes . maybeAbiName solc)
            <> "\x1b[0m"
            <> pos

getAbiMethodOutput
  :: DappInfo -> W256 -> Word -> Maybe (Text, AbiType)
getAbiMethodOutput dapp hash abi =
  -- Some typical ugly lens code. :'(
  preview
    ( dappSolcByHash . ix hash . _2 . abiMap
    . ix (fromIntegral abi) . methodOutput . _Just
    )
    dapp

getAbiTypes :: Text -> [Maybe AbiType]
getAbiTypes abi = map (parseTypeName mempty) types
  where
    types =
      filter (/= "") $
        splitOn "," (dropEnd 1 (last (splitOn "(" abi)))

showCall :: [AbiType] -> Buffer -> Text
showCall ts (SymbolicBuffer bs) = showValues ts $ SymbolicBuffer (drop 4 bs)
showCall ts (ConcreteBuffer bs) = showValues ts $ ConcreteBuffer (BS.drop 4 bs)

showError :: ByteString -> Text
showError bs = case BS.take 4 bs of
  -- Method ID for Error(string)
  "\b\195y\160" -> showCall [AbiStringType] (ConcreteBuffer bs)
  _             -> formatBinary bs

showValues :: [AbiType] -> Buffer -> Text
showValues ts (SymbolicBuffer sbs) = "symbolic: " <> (pack . show $ AbiTupleType (fromList ts))
showValues ts (ConcreteBuffer bs) =
  case runGetOrFail (getAbiSeq (length ts) ts) (fromStrict bs) of
    Right (_, _, xs) -> showAbiValues xs
    Left (_, _, _)   -> formatBinary bs

showValue :: AbiType -> Buffer -> Text
showValue t (SymbolicBuffer _) = "symbolic: " <> (pack $ show t)
showValue t (ConcreteBuffer bs) =
  case runGetOrFail (getAbi t) (fromStrict bs) of
    Right (_, _, x) -> showAbiValue x
    Left (_, _, _)  -> formatBinary bs

maybeContractName :: Maybe SolcContract -> Text
maybeContractName =
  maybe "<unknown contract>" (view (contractName . to contractNamePart))

maybeAbiName :: SolcContract -> Word -> Maybe Text
maybeAbiName solc abi = preview (abiMap . ix (fromIntegral abi) . methodSignature) solc

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0
