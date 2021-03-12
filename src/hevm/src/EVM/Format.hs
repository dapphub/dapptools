{-# Language DataKinds #-}
{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}
module EVM.Format where

import System.Console.ANSI hiding (Dull)
import Prelude hiding (Word, GT, LT)
import Numeric
import qualified EVM
import EVM.Dapp (DappInfo (..), dappSolcByHash, dappAbiMap, showTraceLocation, dappEventMap)
import EVM.Dapp (DappContext (..), contextInfo, contextEnv)
import EVM.Concrete ( wordValue )
import EVM (VM, VMResult(..), cheatCode, traceForest, traceData, Error (..), result)
import EVM (Trace, TraceData (..), Log (..), Query (..), FrameContext (..), Storage(..))
import EVM.SymExec
import EVM.Symbolic (len, litWord)
import EVM.Types (maybeLitWord, Word (..), SymWord(..), W256 (..), num, Addr, Buffer(..), ByteStringS(..))
import EVM.Expr
import EVM.ABI (AbiValue (..), Event (..), AbiType (..))
import EVM.ABI (Indexed (NotIndexed), getAbiSeq, getAbi)
import EVM.ABI (parseTypeName, formatString)
import EVM.Solidity (methodName, methodInputs, Method, SolcContract(..), contractName, abiMap, StorageItem(..))
import EVM.Solidity (methodOutput, methodSignature, methodName)

import Control.Monad (join)
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
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)
import Data.Text (dropEnd, splitOn)
import Data.Text.Encoding (decodeUtf8, decodeUtf8')
import Data.Tree (Tree (Node))
import Data.Tree.View (showTree)
import Data.Vector (Vector)
import Data.Word (Word32)

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

showAbiValue :: (?context :: DappContext) => AbiValue -> Text
showAbiValue (AbiBytes _ bs) =
  formatBytes bs  -- opportunistically decodes recognisable strings
showAbiValue (AbiAddress addr) =
  let dappinfo = view contextInfo ?context
      contracts = view (contextEnv . EVM.contracts) ?context
      name = case (Map.lookup addr contracts) of
        Nothing -> ""
        Just contract ->
          let hash = view EVM.codehash contract
              solcContract = (preview (dappSolcByHash . ix hash . _2) dappinfo)
          in maybeContractName' solcContract
  in
    name <> "@" <> (pack $ show addr)
showAbiValue v = pack $ show v

showAbiValues :: (?context :: DappContext) => Vector AbiValue -> Text
showAbiValues vs = parenthesise (textAbiValues vs)

textAbiValues :: (?context :: DappContext) => Vector AbiValue -> [Text]
textAbiValues vs = toList (fmap showAbiValue vs)

textValues :: (?context :: DappContext) => [AbiType] -> Buffer -> [Text]
textValues ts (SymbolicBuffer _ _) = [pack $ show t | t <- ts]
textValues ts (ConcreteBuffer _ bs) =
  case runGetOrFail (getAbiSeq (length ts) ts) (fromStrict bs) of
    Right (_, _, xs) -> textAbiValues xs
    Left (_, _, _)   -> [formatBinary bs]

parenthesise :: [Text] -> Text
parenthesise ts = "(" <> intercalate ", " ts <> ")"

showValues :: (?context :: DappContext) => [AbiType] -> Buffer -> Text
showValues ts b = parenthesise $ textValues ts b

showValue :: (?context :: DappContext) => AbiType -> Buffer -> Text
showValue t b = head $ textValues [t] b

showCall :: (?context :: DappContext) => [AbiType] -> Buffer -> Text
showCall ts (SymbolicBuffer _ bs) = showValues ts $ SymbolicBuffer (Oops "showCall") (drop 4 bs)
showCall ts (ConcreteBuffer _ bs) = showValues ts $ ConcreteBuffer (Oops "showCall") (BS.drop 4 bs)

showError :: (?context :: DappContext) => ByteString -> Text
showError bs = case BS.take 4 bs of
  -- Method ID for Error(string)
  "\b\195y\160" -> showCall [AbiStringType] (ConcreteBuffer (Oops "showError") bs)
  _             -> formatBinary bs


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

formatSBytes :: Buffer -> Text
formatSBytes (SymbolicBuffer wbuff b) = "<" <> pack ((show (length b)) <> " symbolic bytes> " <> show wbuff)
formatSBytes (ConcreteBuffer _ b) = formatBytes b

-- a string that came from bytes, displayed with special quotes
formatBString :: ByteString -> Text
formatBString b = mconcat [ "«",  Text.dropAround (=='"') (pack $ formatString b), "»" ]

formatSString :: Buffer -> Text
formatSString (SymbolicBuffer wbuff bs) = "<" <> pack ((show (length bs)) <> " symbolic bytes (string)> " <> show wbuff)
formatSString (ConcreteBuffer _ bs) = pack $ formatString bs

formatBinary :: ByteString -> Text
formatBinary =
  (<>) "0x" . decodeUtf8 . toStrict . toLazyByteString . byteStringHex

formatSBinary :: Buffer -> Text
formatSBinary (SymbolicBuffer wbuff bs) = "<" <> pack ((show (length bs)) <> " symbolic bytes> " <> show wbuff)
formatSBinary (ConcreteBuffer _ bs) = formatBinary bs

showTraceTree :: DappInfo -> VM -> Text
showTraceTree dapp vm =
  let forest = traceForest vm
      traces = fmap (fmap (unpack . showTrace dapp vm)) forest
  in pack $ concatMap showTree traces

unindexed :: [(AbiType, Indexed)] -> [AbiType]
unindexed ts = [t | (t, NotIndexed) <- ts]

showTrace :: DappInfo -> VM -> Trace -> Text
showTrace dapp vm trace =
  let ?context = DappContext { _contextInfo = dapp, _contextEnv = vm ^?! EVM.env }
  in let
    pos =
      case showTraceLocation dapp trace of
        Left x -> " \x1b[1m" <> x <> "\x1b[0m"
        Right x -> " \x1b[1m(" <> x <> ")\x1b[0m"
    fullAbiMap = view dappAbiMap dapp
  in case view traceData trace of
    EventTrace (Log _ bytes topics) ->
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
            Just topic ->
              case Map.lookup (wordValue topic) (view dappEventMap dapp) of
                Just (Event name _ types) ->
                  knownTopic name types
                Nothing ->
                  case topics of
                    [_, t2, _, _] ->
                      -- check for ds-note logs.. possibly catching false positives
                      -- event LogNote(
                      --     bytes4   indexed  sig,
                      --     address  indexed  usr,
                      --     bytes32  indexed  arg1,
                      --     bytes32  indexed  arg2,
                      --     bytes             data
                      -- ) anonymous;
                      let
                        sig = fromIntegral $ shiftR (wordValue topic) 224 :: Word32
                        usr = case maybeLitWord t2 of
                          Just w ->
                            pack $ show $ (fromIntegral w :: Addr)
                          Nothing  ->
                            "<symbolic>"
                      in
                        case Map.lookup sig (view dappAbiMap dapp) of
                          Just m ->
                           lognote (view methodSignature m) usr
                          Nothing ->
                            logn
                    _ ->
                      logn
            Nothing ->
              logn

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
    ReturnTrace out (CreationContext {}) ->
      "← " <> pack (show (len out)) <> " bytes of code"

    EntryTrace t ->
      t
    FrameTrace (CreationContext addr hash _ _ ) ->
      "create "
      <> maybeContractName (preview (dappSolcByHash . ix hash . _2) dapp)
      <> "@" <> pack (show addr)
      <> pos
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

maybeAbiName :: SolcContract -> Word -> Maybe Text
maybeAbiName solc abi = preview (abiMap . ix (fromIntegral abi) . methodSignature) solc

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0

prettyvmresult :: (?context :: DappContext) => VMResult -> String
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

-- TODO addapt to expr
showLeafInfo :: (?srcInfo :: DappInfo)
                 => BranchInfo
                 -> [String]
showLeafInfo (BranchInfo vm _ m) = let
  ?context = DappContext { _contextInfo = ?srcInfo, _contextEnv = vm ^?! EVM.env }
  in let
  self    = view (EVM.state . EVM.contract) vm
  updates = case view (EVM.env . EVM.contracts) vm ^?! ix self . EVM.storage of
    Symbolic v _ -> v
    Concrete _ -> (Todo "showLeafInfo>Concrete storage to expr" [])
  showResult = [prettyvmresult res | Just res <- [view result vm]]
  in showResult
  ++ let
    ?vm = vm
    ?method = m
    in showStorage []
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
          => Expr
          -> Expr
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
          => Expr
          -> Expr
simpW (IsZero (IsZero (IsZero a)))  = simpW (IsZero a)
simpW (IsZero (IsZero a@(GT x y)))  = simpW a
simpW (IsZero (IsZero a@(LT x y)))  = simpW a
simpW (IsZero (IsZero a@(Eq x y)))  = simpW a
simpW (IsZero (IsZero a@(SLT x y))) = simpW a
simpW (IsZero (IsZero a@(SGT x y))) = simpW a
-- simpW (IsZero (IsZero a@(LEQ x y))) = simpW a
-- simpW (IsZero (IsZero a@(GEQ x y))) = simpW a
-- simpW (IsZero (Eq x y))             = simpW (NEq x y)
-- simpW (IsZero (NEq x y))            = simpW (Eq x y)
-- simpW (IsZero (LT x y))             = simpW (GEQ x y)
-- simpW (IsZero (GT x y))             = simpW (LEQ x y)
-- simpW (IsZero (LEQ x y))            = simpW (GT x y)
-- simpW (IsZero (GEQ x y))            = simpW (LT x y)
simpW (IsZero (Var x))              = simpW (Eq (Var x) (Literal 0))
simpW (IsZero x)                    = IsZero $ simpW x
simpW (GT x y)                      = GT (simpW x) (simpW y)
simpW (LT x y)                      = LT (simpW x) (simpW y)
-- simpW (GEQ x y)                     = GEQ (simpW x) (simpW y)
-- simpW (LEQ x y)                     = LEQ (simpW x) (simpW y)
simpW (SGT x y)                     = GT (simpW x) (simpW y)
simpW (SLT x y)                     = LT (simpW x) (simpW y)
simpW (Eq x y)                      = Eq (simpW x) (simpW y)
-- simpW (NEq x y)                     = NEq (simpW x) (simpW y)
simpW (Pointer1 str x)              = Pointer1 str (simpW x)
simpW (FromStorage x y)             = FromStorage (simpW x) (simpS y)
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
-- simpW (And (Literal x) (Var str i))
--   | x == 2^i - 1            = (Var str i)
--   | (2 ^ (floor (logBase 2.0 ((num x) + 1.0)))) == (num x) + 1  = (Var str (floor (logBase 2.0 ((num x) + 1.0))))
--   | otherwise               = (And (Literal x) (Var str i))

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
  in if length input > index then Var (cParam $ unpack $ fst (input !! index)) else Todo ("sad" ++ (show index)) []
simpW (FromBuff w s) = FromBuff (simpW w) (simpS s)
simpW x = x

fixW :: (?srcInfo :: DappInfo,
         ?vm :: VM,
         ?method :: Maybe Method)
         => Expr
         -> Expr
fixW w = let w' = simpW w
         in if w == w' then w else fixW w'


showStorageSlot :: [(Text, StorageItem)] -> Int -> Expr -> Expr
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
