{-# Language DataKinds #-}
{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}
module EVM.Format where

import Prelude hiding (Word)
import qualified EVM
import EVM.Dapp (DappInfo (..), dappSolcByHash, dappAbiMap, showTraceLocation, dappEventMap, dappErrorMap)
import EVM.Dapp (DappContext (..), contextInfo, contextEnv)
import EVM (VM, VMResult(..), cheatCode, traceForest, traceData, Error (..), result)
import EVM (Trace, TraceData (..), Query (..), FrameContext (..))
import EVM.SymExec
import EVM.Types (maybeLitWord, W256 (..), num, word, Expr(..), EType(..))
import EVM.Types (Addr, ByteStringS(..))
import EVM.ABI (AbiValue (..), Event (..), AbiType (..), SolError (..))
import EVM.ABI (Indexed (NotIndexed), getAbiSeq)
import EVM.ABI (parseTypeName, formatString)
import EVM.Solidity (SolcContract(..), contractName, abiMap)
import EVM.Solidity (methodOutput, methodSignature, methodName)

import Control.Arrow ((>>>))
import Control.Lens (view, preview, ix, _2, to, makeLenses, over, each, (^?!))
import Data.Binary.Get (runGetOrFail)
import Data.Bits       (shiftR)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.DoubleWord (signedWord)
import Data.Foldable (toList)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text (dropEnd, splitOn)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Tree (Tree (Node))
import Data.Tree.View (showTree)
import Data.Vector (Vector)
import Data.Word (Word32)
import Data.Char (isSpace)

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

--showWordExact :: W256 -> Text
--showWordExact = humanizeInteger w

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

showAbiValue :: (?context :: DappContext) => AbiValue -> Text
showAbiValue (AbiBytes _ bs) =
  formatBytes bs  -- opportunistically decodes recognisable strings
showAbiValue (AbiAddress addr) = undefined
  {-
  let dappinfo = view contextInfo ?context
      contracts = view contextEnv ?context
      name = case (Map.lookup addr contracts) of
        Nothing -> ""
        Just contract ->
          let hash = view EVM.codehash contract
              solcContract = (preview (dappSolcByHash . ix hash . _2) dappinfo)
          in maybeContractName' solcContract
  in
    name <> "@" <> (pack $ show addr)
  -}
showAbiValue v = pack $ show v

showAbiValues :: (?context :: DappContext) => Vector AbiValue -> Text
showAbiValues vs = parenthesise (textAbiValues vs)

textAbiValues :: (?context :: DappContext) => Vector AbiValue -> [Text]
textAbiValues vs = toList (fmap showAbiValue vs)

textValues :: (?context :: DappContext) => [AbiType] -> Expr Buf -> [Text]
textValues = undefined
--textValues ts (SymbolicBuffer  _) = [pack $ show t | t <- ts]
--textValues ts (ConcreteBuffer bs) =
  --case runGetOrFail (getAbiSeq (length ts) ts) (fromStrict bs) of
    --Right (_, _, xs) -> textAbiValues xs
    --Left (_, _, _)   -> [formatBinary bs]

parenthesise :: [Text] -> Text
parenthesise ts = "(" <> intercalate ", " ts <> ")"

showValues :: (?context :: DappContext) => [AbiType] -> Expr Buf -> Text
showValues ts b = parenthesise $ textValues ts b

showValue :: (?context :: DappContext) => AbiType -> Expr Buf -> Text
showValue t b = head $ textValues [t] b

showCall :: (?context :: DappContext) => [AbiType] -> Expr Buf -> Text
showCall = undefined
--showCall ts (SymbolicBuffer bs) = showValues ts $ SymbolicBuffer (drop 4 bs)
--showCall ts (ConcreteBuffer bs) = showValues ts $ ConcreteBuffer (BS.drop 4 bs)

showError :: (?context :: DappContext) => Expr Buf -> Text
showError bs = undefined
  {-
  let dappinfo = view contextInfo ?context
      bs4 = BS.take 4 bs
  in case Map.lookup (word bs4) (view dappErrorMap dappinfo) of
      Just (SolError errName ts) -> errName <> " " <> showCall ts (ConcreteBuffer bs)
      Nothing -> case bs4 of
                  -- Method ID for Error(string)
                  "\b\195y\160" -> showCall [AbiStringType] (ConcreteBuffer bs)
                  _             -> formatBinary bs
                -}

-- the conditions under which bytes will be decoded and rendered as a string
isPrintable :: ByteString -> Bool
isPrintable =
  decodeUtf8' >>>
    either
      (const False)
      (Text.all (\c-> Char.isPrint c && (not . Char.isControl) c))

formatBytes :: ByteString -> Text
formatBytes b =
  let (s, _) = BS.spanEnd (== 0) b
  in
    if isPrintable s
    then formatBString s
    else formatBinary b

formatSBytes :: Expr Buf -> Text
formatSBytes = undefined
--formatSBytes (SymbolicBuffer b) = "<" <> pack (show (length b)) <> " symbolic bytes>"
--formatSBytes (ConcreteBuffer b) = formatBytes b

-- a string that came from bytes, displayed with special quotes
formatBString :: ByteString -> Text
formatBString b = mconcat [ "«",  Text.dropAround (=='"') (pack $ formatString b), "»" ]

formatSString :: Expr Buf -> Text
formatSString = undefined
--formatSString (SymbolicBuffer bs) = "<" <> pack (show (length bs)) <> " symbolic bytes (string)>"
--formatSString (ConcreteBuffer bs) = pack $ formatString bs

formatBinary :: ByteString -> Text
formatBinary =
  (<>) "0x" . decodeUtf8 . toStrict . toLazyByteString . byteStringHex

formatSBinary :: Expr Buf -> Text
formatSBinary = undefined
--formatSBinary (SymbolicBuffer bs) = "<" <> pack (show (length bs)) <> " symbolic bytes>"
--formatSBinary (ConcreteBuffer bs) = formatBinary bs

showTraceTree :: DappInfo -> VM -> Text
showTraceTree dapp vm =
  let forest = traceForest vm
      traces = fmap (fmap (unpack . showTrace dapp vm)) forest
  in pack $ concatMap showTree traces

unindexed :: [(Text, AbiType, Indexed)] -> [AbiType]
unindexed ts = [t | (_, t, NotIndexed) <- ts]

showTrace :: DappInfo -> VM -> Trace -> Text
showTrace dapp vm trace =
  let ?context = DappContext { _contextInfo = dapp, _contextEnv = vm ^?! EVM.env . EVM.contracts }
  in let
    pos =
      case showTraceLocation dapp trace of
        Left x -> " \x1b[1m" <> x <> "\x1b[0m"
        Right x -> " \x1b[1m(" <> x <> ")\x1b[0m"
    fullAbiMap = view dappAbiMap dapp
  in case view traceData trace of
    EventTrace _ bytes topics ->
      let logn = mconcat
            [ "\x1b[36m"
            , "log" <> (pack (show (length topics)))
            , parenthesise ((map (pack . show) topics) ++ [formatSBinary bytes])
            , "\x1b[0m"
            ] <> pos
          knownTopic name types = mconcat
            [ "\x1b[36m"
            , name
            , showValues (unindexed types) bytes
            -- todo: show indexed
            , "\x1b[0m"
            ] <> pos
          lognote sig usr = mconcat
            [ "\x1b[36m"
            , "LogNote"
            , parenthesise [sig, usr, "..."]
            , "\x1b[0m"
            ] <> pos
      in case topics of
        [] ->
          logn
        (t1:_) ->
          case maybeLitWord t1 of
            Just topic -> undefined
              --case Map.lookup (wordValue topic) (view dappEventMap dapp) of
                --Just (Event name _ types) ->
                  --knownTopic name types
                --Nothing ->
                  --case topics of
                    --[_, t2, _, _] ->
                      ---- check for ds-note logs.. possibly catching false positives
                      ---- event LogNote(
                      ----     bytes4   indexed  sig,
                      ----     address  indexed  usr,
                      ----     bytes32  indexed  arg1,
                      ----     bytes32  indexed  arg2,
                      ----     bytes             data
                      ---- ) anonymous;
                      --let
                        --sig = fromIntegral $ shiftR (wordValue topic) 224 :: Word32
                        --usr = case maybeLitWord t2 of
                          --Just w ->
                            --pack $ show (fromIntegral w :: Addr)
                          --Nothing  ->
                            --"<symbolic>"
                      --in
                        --case Map.lookup sig (view dappAbiMap dapp) of
                          --Just m ->
                           --lognote (view methodSignature m) usr
                          --Nothing ->
                            --logn
                    --_ ->
                      --logn
            Nothing ->
              logn

    QueryTrace q ->
      case q of
        PleaseFetchContract addr _ ->
          "fetch contract " <> pack (show addr) <> pos
        PleaseFetchSlot addr slot _ ->
          "fetch storage slot " <> pack (show slot) <> " from " <> pack (show addr) <> pos
        --PleaseAskSMT {} ->
          --"ask smt" <> pos
        --PleaseMakeUnique {} ->
          --"make unique value" <> pos
        PleaseDoFFI cmd _ ->
          "execute ffi " <> pack (show cmd) <> pos

    ErrorTrace e ->
      case e of
        EVM.Revert out ->
          "\x1b[91merror\x1b[0m " <> "Revert " <> showError out <> pos
        _ ->
          "\x1b[91merror\x1b[0m " <> pack (show e) <> pos

    ReturnTrace out (CallContext _ _ _ _ _ (Just abi) _ _ _) ->
      "← " <>
        case Map.lookup (fromIntegral abi) fullAbiMap of
          Just m  ->
            case unzip (view methodOutput m) of
              ([], []) ->
                formatSBinary out
              (_, ts) ->
                showValues ts out
          Nothing ->
            formatSBinary out
    ReturnTrace out (CallContext {}) ->
      "← " <> formatSBinary out
    ReturnTrace out (CreationContext {}) -> undefined
      --"← " <> pack (show (len out)) <> " bytes of code"

    EntryTrace t ->
      t
    FrameTrace (CreationContext addr hash _ _ ) -> undefined
      -- "create "
      -- <> maybeContractName (preview (dappSolcByHash . ix hash . _2) dapp)
      -- <> "@" <> pack (show addr)
      -- <> pos
    FrameTrace (CallContext target context _ _ hash abi calldata _ _) -> undefined
      {-
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
          -}

getAbiTypes :: Text -> [Maybe AbiType]
getAbiTypes abi = map (parseTypeName mempty) types
  where
    types =
      filter (/= "") $
        splitOn "," (dropEnd 1 (last (splitOn "(" abi)))

maybeContractName :: Maybe SolcContract -> Text
maybeContractName =
  maybe "<unknown contract>" (view (contractName . to contractNamePart))

maybeContractName' :: Maybe SolcContract -> Text
maybeContractName' =
  maybe "" (view (contractName . to contractNamePart))

maybeAbiName :: SolcContract -> W256 -> Maybe Text
maybeAbiName solc abi = preview (abiMap . ix (fromIntegral abi) . methodSignature) solc

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0

prettyvmresult :: (?context :: DappContext) => VMResult -> String
--prettyvmresult (EVM.VMFailure (EVM.Revert ""))  = "Revert"
prettyvmresult (EVM.VMFailure (EVM.Revert msg)) = "Revert" ++ (unpack $ showError msg)
prettyvmresult (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) = "Assertion violation"
prettyvmresult (EVM.VMFailure err) = "Failed: " <> show err
prettyvmresult (EVM.VMSuccess (ConcreteBuf msg)) =
  if BS.null msg
  then "Stop"
  else "Return: " <> show (ByteStringS msg)
prettyvmresult _ = error "TODO: sym prettyVmResult"
--prettyvmresult (EVM.VMSuccess (SymbolicBuffer msg)) =
  --"Return: " <> show (length msg) <> " symbolic bytes"

currentSolc :: DappInfo -> VM -> Maybe SolcContract
currentSolc dapp vm = undefined
  --let
    --this = vm ^?! EVM.env . EVM.contracts . ix (view (EVM.state . EVM.contract) vm)
    --h = view EVM.codehash this
  --in
    --preview (dappSolcByHash . ix h . _2) dapp

-- TODO: display in an 'act' format

-- TreeLine describes a singe line of the tree
-- it contains the indentation which is prefixed to it
-- and its content which contains the rest
data TreeLine = TreeLine {
  _indent   :: String,
  _content  :: String
  }

makeLenses ''TreeLine

-- SHOW TREE

showTreeIndentSymbol :: Bool      -- ^ isLastChild
                     -> Bool      -- ^ isTreeHead
                     -> String
showTreeIndentSymbol True  True  = "\x2514" -- └
showTreeIndentSymbol False True  = "\x251c" -- ├
showTreeIndentSymbol True  False = " "
showTreeIndentSymbol False False = "\x2502" -- │

flattenTree :: Int -> -- total number of cases
               Int -> -- case index
               Tree [String] ->
               [TreeLine]
-- this case should never happen for our use case, here for generality
flattenTree _ _ (Node [] _)  = []

flattenTree totalCases i (Node (x:xs) cs) = let
  isLastCase       = i + 1 == totalCases
  indenthead       = showTreeIndentSymbol isLastCase True <> " " <> show i <> " "
  indentchild      = showTreeIndentSymbol isLastCase False <> " "
  in TreeLine indenthead x
  : ((TreeLine indentchild <$> xs) ++ over (each . indent) ((<>) indentchild) (flattenForest cs))

flattenForest :: [Tree [String]] -> [TreeLine]
flattenForest forest = concat $ zipWith (flattenTree (length forest)) [0..] forest

leftpad :: Int -> String -> String
leftpad n = (<>) $ replicate n ' '

showTree' :: Tree [String] -> String
showTree' (Node s []) = unlines s
showTree' (Node _ children) =
  let
    treeLines = flattenForest children
    maxIndent = 2 + maximum (length . _indent <$> treeLines)
    showTreeLine (TreeLine colIndent colContent) =
      let indentSize = maxIndent - length colIndent
      in colIndent <> leftpad indentSize colContent
  in unlines $ showTreeLine <$> treeLines


-- RENDER TREE

--showStorage :: [(Expr EWord, Expr EWord)] -> [String]
--showStorage = fmap (\(k, v) -> show k <> " => " <> show v)

--showLeafInfo :: DappInfo -> BranchInfo -> [String]
--showLeafInfo srcInfo (BranchInfo vm _) = let
  -- ?context = DappContext { _contextInfo = srcInfo, _contextEnv = vm ^?! EVM.env . EVM.contracts }
  --in let
  --self    = view (EVM.state . EVM.contract) vm
  --updates = case view (EVM.env . EVM.contracts) vm ^?! ix self . EVM.storage of
    --Symbolic v _ -> v
    --Concrete x -> [(litWord k,v) | (k, v) <- Map.toList x]
  --showResult = [prettyvmresult res | Just res <- [view result vm]]
  --in showResult
  -- ++ showStorage updates
  -- ++ [""]

--showBranchInfoWithAbi :: DappInfo -> BranchInfo -> [String]
--showBranchInfoWithAbi _ (BranchInfo _ Nothing) = [""]
--showBranchInfoWithAbi srcInfo (BranchInfo vm (Just y)) =
  --case y of
    --(IsZero (Eq (Literal x) _)) ->
      --let
        --abimap = view abiMap <$> currentSolc srcInfo vm
        --method = abimap >>= Map.lookup (num x)
      --in [maybe (show y) (show . view methodSignature) method]
    --y' -> [show y']

--renderTree :: (a -> [String])
           --- -> (a -> [String])
           --- -> Tree a
           --- -> Tree [String]
--renderTree showBranch showLeaf (Node b []) = Node (showBranch b ++ showLeaf b) []
--renderTree showBranch showLeaf (Node b cs) = Node (showBranch b) (renderTree showBranch showLeaf <$> cs)

indent' :: Int -> String -> String
indent' n = rstrip . unlines . fmap (replicate n ' ' <>) . lines

rstrip :: String -> String
rstrip = reverse . dropWhile (=='\n') . reverse

formatExpr :: Expr a -> String
formatExpr = \case
  ITE c t f -> rstrip . unlines $
    [ "(ITE (" <> formatExpr c <> ")"
    , indent' 2 (formatExpr t)
    , indent' 2 (formatExpr f)
    , ")"]
  a -> show a
