{-# Language DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# Language TemplateHaskell #-}
{-# LANGUAGE ImplicitParams #-}
module EVM.Format where

import System.Console.ANSI hiding (Dull)
import Prelude hiding (Word, GT, LT)
import Numeric
import qualified EVM
import EVM.Dapp (DappInfo (..), dappSolcByHash, dappAbiMap, showTraceLocation, dappEventMap)
import EVM.Concrete ( wordValue )
import EVM (VM, VMResult(..), cheatCode, traceForest, traceData, Error (..), result)
import EVM (Trace, TraceData (..), Log (..), Query (..), FrameContext (..), Storage(..))
import EVM.SymExec
import EVM.Symbolic (len, litWord)
import EVM.Types (maybeLitWord, Word (..), Whiff(..), SymWord(..), W256 (..), num, Buffer(..), ByteStringS(..), Sniff(..))
import EVM.ABI (AbiValue (..), Event (..), AbiType (..))
import EVM.ABI (Indexed (NotIndexed), getAbiSeq, getAbi)
import EVM.ABI (parseTypeName)
import EVM.Solidity (methodName, methodInputs, Method, SolcContract(..), contractName, abiMap, StorageItem(..))
import EVM.Solidity (methodOutput, methodSignature, methodName)

import Control.Monad (join)
import Control.Arrow ((>>>))
import Control.Lens (view, preview, ix, _2, to, _Just, makeLenses, over, each, (^?!))
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

import Debug.Trace

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
formatSBytes (SymbolicBuffer wbuff b) = "<" <> pack ((show (length b)) <> " symbolic bytes> " <> show wbuff)
formatSBytes (ConcreteBuffer _ b) = formatBytes b

formatQString :: ByteString -> Text
formatQString = pack . show

formatString :: ByteString -> Text
formatString bs = decodeUtf8 (fst (BS.spanEnd (== 0) bs))

formatSString :: Buffer -> Text
formatSString (SymbolicBuffer wbuff bs) = "<" <> pack ((show (length bs)) <> " symbolic bytes (string)> " <> show wbuff)
formatSString (ConcreteBuffer _ bs) = formatString bs

formatBinary :: ByteString -> Text
formatBinary =
  (<>) "0x" . decodeUtf8 . toStrict . toLazyByteString . byteStringHex

formatSBinary :: Buffer -> Text
formatSBinary (SymbolicBuffer wbuff bs) = "<" <> pack ((show (length bs)) <> " symbolic bytes> " <> show wbuff)
formatSBinary (ConcreteBuffer _ bs) = formatBinary bs

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
showCall ts (SymbolicBuffer wbuff bs) = showValues ts $ SymbolicBuffer (Oops "showCallS") (drop 4 bs)
showCall ts (ConcreteBuffer wbuff bs) = showValues ts $ ConcreteBuffer (Oops "showCallC") (BS.drop 4 bs)

showError :: ByteString -> Text
showError bs = case BS.take 4 bs of
  -- Method ID for Error(string)
  "\b\195y\160" -> showCall [AbiStringType] (ConcreteBuffer (Oops "showError") bs)
  _             -> formatBinary bs

showValues :: [AbiType] -> Buffer -> Text
showValues ts (SymbolicBuffer _  _) = "symbolic: " <> (pack . show $ AbiTupleType (fromList ts))
showValues ts (ConcreteBuffer _ bs) =
  case runGetOrFail (getAbiSeq (length ts) ts) (fromStrict bs) of
    Right (_, _, xs) -> showAbiValues xs
    Left (_, _, _)   -> formatBinary bs

showValue :: AbiType -> Buffer -> Text
showValue t (SymbolicBuffer _ _) = "symbolic: " <> (pack $ show t)
showValue t (ConcreteBuffer _ bs) =
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
prettyvmresult (EVM.VMSuccess (ConcreteBuffer _ msg)) =
  if BS.null msg
  then "Stop"
  else "Return: " <> show (ByteStringS msg)
prettyvmresult (EVM.VMSuccess (SymbolicBuffer _ msg)) =
  "Return: " <> show (length msg) <> " symbolic bytes"


currentSolc :: DappInfo -> VM -> Maybe SolcContract
currentSolc dapp vm =
  let
    this = vm ^?! EVM.env . EVM.contracts . ix (view (EVM.state . EVM.contract) vm)
    h = view EVM.codehash this
  in
    preview (dappSolcByHash . ix h . _2) dapp

-- COLOR
cColor :: SGR -> String -> String
cColor sgr str =
  let
    c = setSGRCode [sgr]
    r = setSGRCode []
  in c ++ str ++ r

cKeyword :: String -> String
cKeyword = cColor (SetColor Foreground Vivid Magenta)

cVar :: String -> String
cVar = cColor (SetColor Foreground Vivid Yellow)

cParam :: String -> String
cParam = cColor (SetColor Foreground Vivid Green)


-- TODO: display in an 'act' format
-- TreeLine describes a singe line of the tree
-- it contains the indentation which is prefixed to it
-- and its content which contains the rest
data TreeLine = TreeLine {
  _indent   :: String,
  _content  :: String
  }
  -- _leafData :: Maybe (VMResult, [(SymWord, SymWord)])

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

showStorage :: (?srcInfo :: DappInfo,
                ?vm :: VM,
                ?method :: Maybe Method)
                => [(SymWord, SymWord)]
                -> [String]
showStorage = fmap (\(S w x, S v y) -> show (fixW w) <> " => " <> show (fixW v))

showLeafInfo :: (?srcInfo :: DappInfo)
                 => BranchInfo
                 -> [String]
showLeafInfo (BranchInfo vm _ m) = let
  self    = view (EVM.state . EVM.contract) vm
  updates = case view (EVM.env . EVM.contracts) vm ^?! ix self . EVM.storage of
    Symbolic v _ -> v
    Concrete x -> [(litWord k,v) | (k, v) <- Map.toList x]
  showResult = [prettyvmresult res | Just res <- [view result vm]]
  in showResult
  ++ let
    ?vm = vm
    ?method = m
    in showStorage updates
  ++ [""]

showPlaneBranchInfo :: (?srcInfo :: DappInfo)
                    => BranchInfo
                    -> [String]
showPlaneBranchInfo (BranchInfo vm Nothing m)  = [""]
showPlaneBranchInfo (BranchInfo vm (Just w) m) = [show w]

showBranchInfoWithAbi :: (?srcInfo :: DappInfo)
                 => BranchInfo
                 -> [String]
showBranchInfoWithAbi (BranchInfo _ Nothing _) = [""]
showBranchInfoWithAbi (BranchInfo vm (Just w) Nothing) =
  let
    ?vm = vm
    ?method = Nothing
    in [(show (fixW w))]
showBranchInfoWithAbi (BranchInfo vm (Just (IsZero (IsZero (Eq (Literal x) _)))) (Just m)) =
  let
    abimap = view abiMap <$> currentSolc ?srcInfo vm
    method = abimap >>= Map.lookup (num x)
    mname  = (unpack . view methodName) <$> method
    formatInput = \(name, t) -> ((cParam (unpack name)) ++ " " ++ (show t))
    minputs = ((<$>) (pack .formatInput)) <$> (view methodInputs) <$> method
    str = unpack <$> (intercalate (pack ", ")) <$> minputs
    interface = ((<>) (cKeyword "interface ")) <$> mname
    sround = (\str -> "("++str++")") <$> str
    maybeinterface = (fromMaybe "" interface) ++ (fromMaybe "" sround)
  in [maybeinterface]
showBranchInfoWithAbi (BranchInfo vm (Just w) (Just m)) =
  let
    ?vm = vm
    ?method = Just m
    in [(show (fixW w))]

renderTree :: (a -> [String])
           -> (a -> [String])
           -> Tree a
           -> Tree [String]
renderTree showBranch showLeaf (Node b []) = Node (showBranch b ++ showLeaf b) []
renderTree showBranch showLeaf (Node b cs) = Node (showBranch b) (renderTree showBranch showLeaf <$> cs)





simpS :: (?srcInfo :: DappInfo,
          ?vm :: VM,
          ?method :: Maybe Method)
          => Sniff
          -> Sniff
simpS r@(Slice (Literal from) (Literal to) (WriteWord (Literal x) w s))
  | from == x && to == x + 0x20 = WriteWord (Literal from) (simpW w) SEmpty
  | from == x && x + 0x20 < to  = (WriteWord (Literal x) (simpW w) (simpS (Slice (Literal (from + 0x20)) (Literal to) s)))
  | from <= x && x + 0x20 == to = (WriteWord (Literal x) (simpW w) (simpS (Slice (Literal from) (Literal (to - 0x20)) s)))
  | otherwise = Oops (show (x - from))
simpS Calldata = Calldata
simpS x = Oops ("simpS" <> show x)

simpW :: (?srcInfo :: DappInfo,
          ?vm :: VM,
          ?method :: Maybe Method)
          => Whiff
          -> Whiff
simpW (IsZero (IsZero (IsZero a)))  = simpW (IsZero a)
simpW (IsZero (IsZero a@(GT x y)))  = simpW a
simpW (IsZero (IsZero a@(LT x y)))  = simpW a
simpW (IsZero (IsZero a@(Eq x y)))  = simpW a
simpW (IsZero (IsZero a@(SLT x y))) = simpW a
simpW (IsZero (IsZero a@(SGT x y))) = simpW a
simpW (IsZero (IsZero a@(LEQ x y))) = simpW a
simpW (IsZero (IsZero a@(GEQ x y))) = simpW a
simpW (IsZero (Eq x y))             = simpW (NEq x y)
simpW (IsZero (NEq x y))            = simpW (Eq x y)
simpW (IsZero (LT x y))             = simpW (GEQ x y)
simpW (IsZero (GT x y))             = simpW (LEQ x y)
simpW (IsZero (LEQ x y))            = simpW (GT x y)
simpW (IsZero (GEQ x y))            = simpW (LT x y)
simpW (IsZero (Var x a))            = simpW (Eq (Var x a) (Literal 0))
simpW (IsZero x)                    = IsZero $ simpW x
simpW (GT x y)                      = GT (simpW x) (simpW y)
simpW (LT x y)                      = LT (simpW x) (simpW y)
simpW (GEQ x y)                     = GEQ (simpW x) (simpW y)
simpW (LEQ x y)                     = LEQ (simpW x) (simpW y)
simpW (SGT x y)                     = GT (simpW x) (simpW y)
simpW (SLT x y)                     = LT (simpW x) (simpW y)
simpW (Eq x y)                      = Eq (simpW x) (simpW y)
simpW (NEq x y)                     = NEq (simpW x) (simpW y)
simpW (Pointer1 str x)              = Pointer1 str (simpW x)
simpW (FromStorage x)               = FromStorage (simpW x)
-- symbolic storage lookup
simpW (FromKeccak
        s@(WriteWord
          (Literal x)
          (Literal y)
          (WriteWord
            (Literal z)
            word
            SEmpty)))
  | x == 0x20 && z == 0x0 = showStorageSlot storagelist (num y) word
  | otherwise             = FromKeccak $ simpS s
  where
    storagelist = Map.toList $ fromMaybe mempty (fromMaybe Nothing (_storageLayout <$> currentSolc ?srcInfo ?vm))
simpW (FromKeccak s)                = FromKeccak $ simpS s
simpW (And (Literal x) (And (Literal x') y))
  | x == x'                         = simpW (And (Literal x) y)
  | otherwise                       = (And (Literal x) (simpW (And (Literal x') y)))
simpW (And (Literal x) (Var str i))
  | x == 2^i - 1            = (Var str i)
  | (2 ^ (floor (logBase 2.0 ((num x) + 1.0)))) == (num x) + 1  = (Var str (floor (logBase 2.0 ((num x) + 1.0))))
  | otherwise               = (And (Literal x) (Var str i))

simpW (And x y)                     = And (simpW x) (simpW y)
simpW (Add (Literal x) (Literal y)) = (Literal (x+y))
simpW (Add x (Sub (Literal a) y))
  | a == 0x0  = (Sub (simpW x) (simpW y))
  | otherwise = (Add (simpW x) (Sub (Literal a) (simpW y)))
simpW (Add x y) = (Add (simpW x) (simpW y))
simpW (Sub (Literal x) (Literal y)) = (Literal (x-y))
simpW (Sub x y) = (Sub (simpW x) (simpW y))
simpW (Mul x y)                     = Mul (simpW x) (simpW y)
simpW (Div x y)                     = Div (simpW x) (simpW y)
simpW (FromBuff (Literal x) Calldata) =
  let
    input = fromMaybe [] $ view methodInputs <$> ?method
    index = num (((toInteger x) - 4) `div` 32)
  in if length input > index then Var (cParam $ unpack $ fst (input !! index)) 256 else Dull ("sad" ++ (show index))
simpW (FromBuff w s) = FromBuff (simpW w) (simpS s)
simpW x = x

fixW :: (?srcInfo :: DappInfo,
         ?vm :: VM,
         ?method :: Maybe Method)
         => Whiff
         -> Whiff
fixW w = let w' = simpW w
         in if w == w' then w else fixW w'


showStorageSlot :: [(Text, StorageItem)] -> Int -> Whiff -> Whiff
showStorageSlot []      slot w = w
showStorageSlot ((text, StorageItem t o s):xs) slot w
  | s == slot   = Pointer1 (unpack text) w




propagateMethod :: Method -> Tree BranchInfo -> Tree BranchInfo
propagateMethod m (Node (BranchInfo vm whiff _) cs) = (Node (BranchInfo vm whiff (Just m)) (propagateMethod m <$> cs))

propagateData :: (?srcInfo :: DappInfo) => Int -> Tree BranchInfo -> Tree BranchInfo
propagateData _ n@(Node _ [])    = n
propagateData _ (Node bi@(BranchInfo _ Nothing _) cs) = (Node bi (zipWith propagateData [0..] cs))
propagateData 1 n@(Node bi@(BranchInfo vm (Just (IsZero (IsZero (Eq (Literal x) _)))) _) cs) =
  let
    abimap = view abiMap <$> currentSolc ?srcInfo vm
    mmethod = (abimap >>= Map.lookup (num x))
    mmm = mmethod
  in case mmm of
    Nothing -> (Node bi (zipWith propagateData [0..] cs))
    Just method -> propagateMethod method n
propagateData _ (Node bi cs) = (Node bi (zipWith propagateData [0..] cs))
