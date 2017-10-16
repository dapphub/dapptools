module EVM.Format where

import Prelude hiding (Word)

import EVM (VM, cheatCode, traceForest, traceData)
import EVM (Trace, TraceData (..), Log (..), Query (..), FrameContext (..))
import EVM.Dapp (DappInfo, dappSolcByHash, showTraceLocation, dappEventMap)
import EVM.Concrete (Concrete, Word (..), Blob (..))
import EVM.Types (W256 (..), num)
import EVM.ABI (AbiValue (..), Event (..), AbiType (..))
import EVM.ABI (Indexed (Indexed, NotIndexed), getAbiSeq)
import EVM.ABI (parseTypeName)
import EVM.Solidity (SolcContract, contractName, abiMap)
import EVM.Machine (forceConcreteBlob, forceConcreteWord)

import Control.Arrow ((>>>))
import Control.Lens (view, preview, ix, _2, to)
import Data.Binary.Get (runGetOrFail)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.DoubleWord (signedWord)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text (dropEnd, splitOn)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Tree.View (showTree)
import Data.Vector (Vector)

import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text

data Signedness = Signed | Unsigned

showDec :: Signedness -> W256 -> Text
showDec signed (W256 w) =
  let
    i = case signed of
          Signed   -> num (signedWord w)
          Unsigned -> num w

  in
    if i == num cheatCode
    then "<hevm cheat address>"
    else
      if abs i > 1000000000000
      then
        "~" <> pack (Scientific.formatScientific
                       Scientific.Generic
                       (Just 8)
                       (fromIntegral i))
      else
        showDecExact i

showDecExact :: Integer -> Text
showDecExact = humanizeInteger

showWordExact :: Word Concrete -> Text
showWordExact (C _ (W256 w)) = humanizeInteger w

humanizeInteger :: (Num a, Integral a, Show a) => a -> Text
humanizeInteger =
  ( Text.intercalate ","
  . reverse
  . map Text.reverse
  . Text.chunksOf 3
  . Text.reverse
  . Text.pack
  . show
  )

-- TODO: make polymorphic
showAbiValues :: Vector AbiValue -> Text
showAbiValues vs =
  "(" <> intercalate ", " (toList (fmap showAbiValue vs)) <> ")"

showAbiValue :: AbiValue -> Text
showAbiValue (AbiUInt _ w)        = humanizeInteger w
showAbiValue (AbiInt _ w)         = humanizeInteger w
showAbiValue (AbiAddress w160)    = pack $ "0x" ++ (showHex w160 "")
showAbiValue (AbiBool b)          = pack $ show b
showAbiValue (AbiBytes _ bs)      = formatBytes bs
showAbiValue (AbiBytesDynamic bs) = formatBinary bs
showAbiValue (AbiString bs)       = formatQString bs
-- TODO: arrays
showAbiValue value = pack $ show value

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

formatQString :: ByteString -> Text
formatQString = pack . show

formatString :: ByteString -> Text
formatString bs = decodeUtf8 (fst (BS.spanEnd (== 0) bs))

formatBinary :: ByteString -> Text
formatBinary =
  (<>) "0x" . decodeUtf8 . toStrict . toLazyByteString . byteStringHex

showTraceTree :: DappInfo -> VM Concrete -> Text
showTraceTree dapp =
  traceForest
    >>> fmap (fmap (unpack . showTrace dapp))
    >>> concatMap showTree
    >>> pack

showTrace :: DappInfo -> Trace Concrete -> Text
showTrace dapp trace =
  let
    pos =
      case showTraceLocation dapp trace of
        Left x -> " \x1b[90m" <> x <> "\x1b[0m"
        Right x -> " \x1b[90m(" <> x <> ")\x1b[0m"
  in case view traceData trace of
    EventTrace (Log _ bytes topics) ->
      case topics of
        (t:_) ->
          formatLog
            (getEvent t (view dappEventMap dapp))
            (forceConcreteBlob bytes) <> pos
        _ ->
          "log" <> pos
    QueryTrace q ->
      case q of
        PleaseFetchContract addr _ ->
          "fetch contract " <> pack (show addr) <> pos
        PleaseFetchSlot addr slot _ ->
          "fetch storage slot " <> pack (show slot) <> " from " <> pack (show addr) <> pos
    ErrorTrace e ->
      "\x1b[91merror\x1b[0m " <> pack (show e) <> pos
    ReturnTrace _ (CreationContext {}) ->
      error "internal error: shouldn't show returns for creates"
    ReturnTrace output _ ->
      "← " <> formatBinary (forceConcreteBlob output)
    EntryTrace t ->
      t
    FrameTrace (CreationContext hash) ->
      "create " <> maybeContractName (preview (dappSolcByHash . ix hash . _2) dapp) <> pos
    FrameTrace (CallContext _ _ hash abi calldata _) ->
      case preview (dappSolcByHash . ix hash . _2) dapp of
        Nothing ->
          "call [unknown]" <> pos
        Just solc ->
          "call "
            <> "\x1b[1m"
            <> view (contractName . to contractNamePart) solc
            <> "::"
            <> maybe ("[fallback function] calldata: " <> formatBinary (forceConcreteBlob calldata))
                 (\x -> maybe "[unknown method]" id (maybeAbiName solc x))
                 abi
            <> maybe "[fallback function]"
                 -- todo: if unknown method, then just show raw calldata
                 (\x -> showCall (catMaybes (getAbiTypes (maybe "" id (maybeAbiName solc x)))) (forceConcreteBlob calldata))
                 abi
            <> "\x1b[0m"
            <> pos

getAbiTypes :: Text -> [Maybe AbiType]
getAbiTypes abi = map parseTypeName types
    where types = splitOn "," (dropEnd 1 (last (splitOn "(" abi)))

showCall :: [AbiType] -> ByteString -> Text
showCall types calldata =
    case runGetOrFail (getAbiSeq (length types) types)
                      (fromStrict (BS.drop 4 calldata)) of
                 Right (_, _, abivals) -> showAbiValues abivals
                 Left (_,_,_)          -> error "lol"

maybeContractName :: Maybe SolcContract -> Text
maybeContractName =
  maybe "<unknown contract>" (view (contractName . to contractNamePart))

maybeAbiName :: SolcContract -> Word Concrete -> Maybe Text
maybeAbiName solc abi = preview (abiMap . ix (fromIntegral abi)) solc

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0

-- TODO: this should take Log
formatLog :: Maybe Event -> ByteString -> Text
formatLog event args =
  let types = getEventUnindexedTypes event
      name  = getEventName event
  in
  case runGetOrFail (getAbiSeq (length types) types)
                      (fromStrict args) of
                    Right (_, _, abivals) ->
                      mconcat
                        [ "\x1b[36m"
                        , name
                        , showAbiValues abivals
                        , "\x1b[0m"
                        ]
                    Left (_,_,_) ->
                      error "lol"

getEvent :: Word Concrete -> Map W256 Event -> Maybe Event
getEvent w events = Map.lookup (forceConcreteWord w) events

getEventName :: Maybe Event -> Text
getEventName (Just (Event name _ _)) = name
getEventName Nothing = "<unknown-event>"

getEventUnindexedTypes :: Maybe Event -> [AbiType]
getEventUnindexedTypes Nothing = []
getEventUnindexedTypes (Just (Event _ _ xs)) = [x | (x, NotIndexed) <- xs]

getEventIndexedTypes :: Maybe Event -> [AbiType]
getEventIndexedTypes Nothing = []
getEventIndexedTypes (Just (Event _ _ xs)) = [x | (x, Indexed) <- xs]

getEventArgs :: Blob Concrete -> Text
getEventArgs b = formatBlob b

formatBlob :: Blob Concrete -> Text
formatBlob b = decodeUtf8 $ forceConcreteBlob b
