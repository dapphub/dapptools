{-# Language DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
module EVM.Format where

import Prelude hiding (Word)
import Numeric
import qualified EVM
import EVM.Dapp (DappInfo (..), dappSolcByHash, dappAbiMap, dappSolcByName, showTraceLocation, dappEventMap)
import EVM.Concrete ( wordValue )
import EVM (VM, VMResult(..), cheatCode, traceForest, traceData, Error (..), result)
import EVM (Trace, TraceData (..), Log (..), Query (..), FrameContext (..), Storage(..))
import EVM.SymExec
import EVM.Symbolic ( len, litWord)
import EVM.Types (maybeLitWord, Word (..), Whiff(..), SymWord(..), W256 (..), num, Buffer(..), ByteStringS(..))
import EVM.ABI (AbiValue (..), Event (..), AbiType (..))
import EVM.ABI (Indexed (NotIndexed), getAbiSeq, getAbi)
import EVM.ABI (parseTypeName)
import EVM.Solidity (SolcContract(..), contractName, abiMap)
import EVM.Solidity (methodOutput, methodSignature, methodName)

import Control.Arrow ((>>>))
import Control.Lens (view, preview, ix, _2, to, _Just, makeLenses, over, (^?!))
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
import Data.Tree (Tree (Node))
import Data.Tree.View (showTree)
import Data.Vector (Vector, fromList)

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

showWordExplanation :: W256 -> DappInfo -> Text
showWordExplanation w _ | w > 0xffffffff = showDec Unsigned w
showWordExplanation w dapp =
  case Map.lookup (fromIntegral w) (view dappAbiMap dapp) of
    Nothing -> showDec Unsigned w
    Just x  -> "keccak(\"" <> view methodSignature x <> "\")"

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
        Left x -> " \x1b[1m" <> x <> "\x1b[0m"
        Right x -> " \x1b[1m(" <> x <> ")\x1b[0m"
    fullAbiMap = view dappAbiMap dapp
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
        PleaseFetchContract addr _ _ ->
          "fetch contract " <> pack (show addr) <> pos
        PleaseFetchSlot addr slot _ ->
          "fetch storage slot " <> pack (show slot) <> " from " <> pack (show addr) <> pos
        PleaseAskSMT _ _ _ ->
          "ask smt" <> pos
        PleaseMakeUnique _ _ _ ->
          "make unique value" <> pos

    ErrorTrace e ->
      case e of
        Revert out ->
          "\x1b[91merror\x1b[0m " <> "Revert " <> showError out <> pos
        _ ->
          "\x1b[91merror\x1b[0m " <> pack (show e) <> pos

    ReturnTrace out (CallContext _ _ _ _ hash (Just abi) _ _ _) ->
      case getAbiMethodOutput dapp hash abi of
        Nothing ->
          "← " <>
            case Map.lookup (fromIntegral abi) fullAbiMap of
              Just m  ->
                case (view methodOutput m) of
                  Just (_, t) ->
                    pack (show t) <> " " <> showValue t out
                  Nothing ->
                    formatSBinary out
              Nothing ->
                formatSBinary out
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
    FrameTrace (CallContext target context _ _ hash abi calldata _ _) ->
      let calltype = if target == context
                     then "call "
                     else "delegatecall "
      in case preview (dappSolcByHash . ix hash . _2) dapp of
        Nothing ->
          calltype
            <> pack (show target)
            <> pack "::"
            <> case Map.lookup (fromIntegral (fromMaybe 0x00 abi)) fullAbiMap of
                 Just m  ->
                   "\x1b[1m"
                   <> view methodName m
                   <> "\x1b[0m"
                   <> showCall (catMaybes (getAbiTypes (view methodSignature m))) calldata
                 Nothing ->
                   formatSBinary calldata
            <> pos

        Just solc ->
          calltype
            <> "\x1b[1m"
            <> view (contractName . to contractNamePart) solc
            <> "::"
            <> maybe "[fallback function]"
                 (fromMaybe "[unknown method]" . maybeAbiName solc)
                 abi
            <> maybe ("(" <> formatSBinary calldata <> ")")
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
showValues ts (SymbolicBuffer  _) = "symbolic: " <> (pack . show $ AbiTupleType (fromList ts))
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

prettyvmresult :: VMResult -> String
prettyvmresult (EVM.VMFailure (EVM.Revert ""))  = "Revert"
prettyvmresult (EVM.VMFailure (EVM.Revert msg)) = "Revert" ++ (unpack $ showError msg)
prettyvmresult (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) = "Assertion violation"
prettyvmresult (EVM.VMFailure err) = "Failed: " <> show err
prettyvmresult (EVM.VMSuccess (ConcreteBuffer msg)) =
  if BS.null msg
  then "Stop"
  else "Return: " <> show (ByteStringS msg)
prettyvmresult (EVM.VMSuccess (SymbolicBuffer msg)) =
  "Return: " <> show (length msg) <> " symbolic bytes"


currentSolc :: DappInfo -> VM -> Maybe SolcContract
currentSolc dapp vm =
  let
    this = vm ^?! env . contracts . ix (view (state . contract) vm)
    h = view codehash this
  in
    preview (dappSolcByHash . ix h . _2) dapp

-- TODO: display in an 'act' format
--
-- TreeLine describes a singe line of the tree
-- it contains the indentation which is prefixed to it
-- and its content which contains the rest
data TreeLine = TreeLine {
  _indent   :: String,
  _content  :: String
  }
  -- _leafData :: Maybe (VMResult, [(SymWord, SymWord)])

makeLenses ''TreeLine

showTreeIndentSymbol :: Bool      -- ^ isLastChild
                     -> Bool      -- ^ isTreeHead
                     -> String
showTreeIndentSymbol True  True  = "\x2514"
showTreeIndentSymbol False True  = "\x251c"
showTreeIndentSymbol True  False = " "
showTreeIndentSymbol False False = "\x2502"



showStorage :: [(SymWord, SymWord)] -> [String]
showStorage = fmap (\(k, v) -> show k <> " => " <> show v)

showLeaf :: DappInfo -> BranchInfo -> [String]
showLeaf srcInfo (BranchInfo vm _) = let
  self    = view (EVM.state . EVM.contract) vm
  updates = case view (EVM.env . EVM.contracts) vm ^?! ix self . EVM.storage of
    Symbolic v _ -> v
    Concrete x -> [(litWord k,v) | (k, v) <- Map.toList x]
  showResult = [prettyvmresult res | Just res <- [view result vm]]
  in showResult
  ++ showStorage updates
  ++ [""]

formatBranchInfo :: DappInfo -> BranchInfo -> String
formatBranchInfo srcInfo bi =
  let cond = maybe "" show (_branchCondition bi)
  in case _branchCondition bi of
    -- recover number if branch condition is `isZero(<hexNr> == _)`
    -- TODO this is dirty and dangerous, do this only for a certain tree depth where abi discovery takes place
    --      this could be extended by requiering the second information to be Slice 0:4 of CALLDATALOAD
    -- TODO rewrite this as a multi case rose tree unraveling, make sure n case jump in the debugger is implemented
    Just (UnOp "isZero" (InfixBinOp "==" (Val x) _)) ->
      let
        abimap = view abiMap <$> currentSolc srcInfo (_vm bi)
        method = abimap >>= Map.lookup (read x)
      in cond -- maybe cond (show . view methodSignature) method

    _ -> cond


flattenTree :: DappInfo ->
               Int -> -- total number of cases
               Int -> -- case index
               Tree BranchInfo ->
               [TreeLine]
-- leaf case
flattenTree srcInfo totalCases i (Node bi []) = let
  bhead = formatBranchInfo srcInfo bi
  isLastCase       = i + 1 == totalCases
  indenthead       = showTreeIndentSymbol isLastCase True <> " " <> show i
  tail = showLeaf srcInfo bi
  in TreeLine indenthead (bhead)
  : ((TreeLine (showTreeIndentSymbol isLastCase False <> " ")) <$> tail)

-- branch case
flattenTree srcInfo totalCases i (Node bi xs) = let
  bhead            = formatBranchInfo srcInfo bi
  isLastCase       = i + 1 == totalCases
  indenthead       = showTreeIndentSymbol isLastCase True <> " " <> show i <> " "
  indentchild      = showTreeIndentSymbol isLastCase False <> " "
  forest           = flattenForest srcInfo xs
  indentLine       = over indent $ (<>) indentchild
  indentedChildren = map indentLine forest
  in (TreeLine indenthead bhead)
  :  indentedChildren

flattenForest :: DappInfo -> [Tree BranchInfo] -> [TreeLine]
flattenForest srcInfo forest = concat $ zipWith (flattenTree srcInfo (length forest)) [0..] forest

leftpad :: Int -> String -> String
leftpad n = (<>) $ replicate n ' '

showBranchTree :: DappInfo -> Tree BranchInfo -> String
showBranchTree srcInfo (Node _ children) =
  let
    treeLines = flattenForest srcInfo children
    doMax treeLine x = max x $ length $ _indent treeLine
    maxIndent = 2 + foldr doMax 0 treeLines
    showTreeLine bd = let
      colIndent  = _indent bd
      colContent = _content bd
      indentSize = (maxIndent -(length colIndent))
      in colIndent <> leftpad indentSize colContent
  in unlines $ showTreeLine <$> treeLines


data AbiBranching = NoBranching | ConcreteBranching | CompareBranching

abiBranching :: BranchInfo -> AbiBranching
abiBranching _ = NoBranching -- TODO fix

flattenAbi :: Bool ->
              DappInfo ->
              Tree BranchInfo ->
              Tree BranchInfo
flattenAbi False srcInfo t@(Node bi children) = case abiBranching bi of
  NoBranching       -> Node bi $ map (flattenAbi False srcInfo) children
  ConcreteBranching -> t
  CompareBranching  -> t
flattenAbi False srcInfo t = t
