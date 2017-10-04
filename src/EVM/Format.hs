module EVM.Format where

import Prelude hiding (Word)

import EVM (VM, cheatCode, traceForest, traceData)
import EVM (Trace, TraceData (..), Log (..), Query (..), FrameContext (..))
import EVM.Dapp (DappInfo, dappSolcByHash, showTraceLocation)
import EVM.Concrete (Concrete, Word (..))
import EVM.Types (W256 (..), num)
import EVM.ABI (AbiValue (..))
import EVM.Solidity (SolcContract, contractName, abiMap)

import Control.Arrow ((>>>))
import Control.Lens (view, preview, ix, _2, to)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.DoubleWord (signedWord)
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Tree.View (showTree)
import Data.Vector (Vector)

import Numeric (showHex)

import qualified Data.Char as Char
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
formatBytes b | isPrintable b = formatQString b
              | otherwise     = formatBinary b

formatQString :: ByteString -> Text
formatQString = pack . show

formatString :: ByteString -> Text
formatString = decodeUtf8

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
        Left x -> " \x1b[90mm" <> x <> "\x1b[0m"
        Right x -> " \x1b[90m(" <> x <> ")\x1b[0m"
  in case view traceData trace of
    EventTrace (Log _ _bytes _topics) ->
      "log" <> pos
    QueryTrace q ->
      case q of
        PleaseFetchContract addr _ ->
          "fetch contract " <> pack (show addr) <> pos
        PleaseFetchSlot addr slot _ ->
          "fetch storage slot " <> pack (show slot) <> " from " <> pack (show addr) <> pos
    ErrorTrace e ->
      "error " <> pack (show e) <> pos
    EntryTrace t ->
      t
    FrameTrace (CreationContext hash) ->
      "create " <> maybeContractName (preview (dappSolcByHash . ix hash . _2) dapp) <> pos
    FrameTrace (CallContext _ _ hash abi _) ->
      case preview (dappSolcByHash . ix hash . _2) dapp of
        Nothing ->
          "call [unknown]" <> pos
        Just solc ->
          "call "
            <> view (contractName . to contractNamePart) solc
            <> " "
            <> maybe "[fallback function]"
                 (\x -> maybe "[unknown method]" id (maybeAbiName solc x))
                 abi
            <> pos

maybeContractName :: Maybe SolcContract -> Text
maybeContractName =
  maybe "<unknown contract>" (view (contractName . to contractNamePart))

maybeAbiName :: SolcContract -> Word Concrete -> Maybe Text
maybeAbiName solc abi = preview (abiMap . ix (fromIntegral abi)) solc

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0
