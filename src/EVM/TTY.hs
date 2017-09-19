{-# Language TemplateHaskell #-}

module EVM.TTY where

import Prelude hiding (Word)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import EVM
import EVM.Concrete (Concrete, Blob (..), Word (C))
import EVM.Debug
import EVM.Exec
import EVM.Machine (Machine)
import EVM.Op
import EVM.Solidity
import EVM.Types
import EVM.UnitTest

import Control.Lens
import Control.Monad.State.Strict hiding (state)

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (drawForest)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Tree.Zipper as Zipper
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec
import qualified Graphics.Vty as Vty

import qualified EVM.TTYCenteredList as Centered

data Name
  = AbiPane
  | StackPane
  | BytecodePane
  | TracePane
  | SolidityPane
  | SolidityViewport
  | TestPickerPane
  deriving (Eq, Show, Ord)

type UiWidget = Widget Name

data VmContinuation e
  = Continue (VMResult e -> UiVmState e -> UiVmState e)
  | Stop

data UiVmState e = UiVmState
  { _uiVm             :: VM e
  , _uiVmContinuation :: VmContinuation e
  , _uiVmStackList    :: List Name (Int, Word e)
  , _uiVmBytecodeList :: List Name (Int, Op)
  , _uiVmTraceList    :: List Name String
  , _uiVmSolidityList :: List Name (Int, ByteString)
  , _uiVmSolc         :: Maybe SolcContract
  , _uiVmDapp         :: Maybe DappInfo
  , _uiVmStepCount    :: Int
  , _uiVmFirstState   :: UiVmState e
  , _uiVmMessage      :: Maybe String
  }

data CodeType = Creation | Runtime
  deriving (Show, Eq, Ord)

data DappInfo = DappInfo
  { _dappRoot       :: FilePath
  , _dappSolcByName :: Map Text SolcContract
  , _dappSolcByHash :: Map W256 (CodeType, SolcContract)
  , _dappSources    :: SourceCache
  , _dappUnitTests  :: [(Text, [Text])]
  }

data UiTestPickerState = UiTestPickerState
  { _testPickerList :: List Name (Text, Text)
  , _testPickerDapp :: DappInfo
  }

data UiState e
  = UiVmScreen (UiVmState e)
  | UiTestPickerScreen UiTestPickerState

makeLenses ''DappInfo
makeLenses ''UiVmState
makeLenses ''UiTestPickerState
makePrisms ''UiState

isUnitTestContract :: Text -> DappInfo -> Bool
isUnitTestContract name dapp =
  elem name (map fst (view dappUnitTests dapp))

mkVty :: IO Vty.Vty
mkVty = do
  vty <- Vty.mkVty Vty.defaultConfig
  Vty.setMode (Vty.outputIface vty) Vty.BracketedPaste True
  return vty

runFromVM :: VM Concrete -> IO ()
runFromVM vm = do
  let
    ui0 = UiVmState
           { _uiVm = vm
           , _uiVmContinuation = Stop
           , _uiVmStackList = undefined
           , _uiVmBytecodeList = undefined
           , _uiVmTraceList = undefined
           , _uiVmSolidityList = undefined
           , _uiVmSolc = Nothing
           , _uiVmDapp = Nothing
           , _uiVmStepCount = 0
           , _uiVmFirstState = undefined
           , _uiVmMessage = Just "Executing EVM code"
           }
    ui1 = updateUiVmState ui0 vm & set uiVmFirstState ui1

  _ <- customMain mkVty Nothing app (UiVmScreen ui1)
  return ()

main :: FilePath -> FilePath -> IO ()
main root jsonFilePath = do
  readSolc jsonFilePath >>=
    \case
      Nothing ->
        error "Failed to read Solidity JSON"
      Just (contractMap, sourceCache) -> do
        let
          solcs     = Map.elems contractMap
          unitTests = findUnitTests solcs

          dappInfo = DappInfo
              { _dappRoot       = root
              , _dappUnitTests  = unitTests
              , _dappSolcByName = contractMap
              , _dappSources    = sourceCache
              , _dappSolcByHash =
                  mappend
                    (Map.fromList [(view runtimeCodehash c, (Runtime, c)) | c <- solcs])
                    (Map.fromList [(view creationCodehash c, (Creation, c)) | c <- solcs])
              }

          ui = UiTestPickerScreen $ UiTestPickerState
            { _testPickerList =
                list
                  TestPickerPane
                  (Vec.fromList
                   (concatMap
                    (\(a, xs) -> [(a, x) | x <- xs])
                    unitTests))
                  1
            , _testPickerDapp = dappInfo
            }

        _ <- customMain mkVty Nothing app (ui :: UiState Concrete)
        return ()

app :: App (UiState Concrete) () Name
app = App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = \s e ->

      case (s, e) of
        (_, VtyEvent (Vty.EvKey Vty.KEsc []) )->
          halt s

        (UiVmScreen s', VtyEvent (Vty.EvKey (Vty.KChar 'n') [])) ->
          useContinuation (halt s) stepOneOpcode s'

        (UiVmScreen s', VtyEvent (Vty.EvKey (Vty.KChar 'N') [])) ->
          useContinuation (halt s) stepOneSourcePosition s'

        (UiVmScreen s', VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) ->
          useContinuation (halt s) stepOneSourcePosition_over s'

        (UiVmScreen s', VtyEvent (Vty.EvKey (Vty.KChar 'p') [])) ->
          let n = view uiVmStepCount s' - 1
          in useContinuationRepeatedly n (halt s) stepOneOpcode
               (view uiVmFirstState s')

        (UiTestPickerScreen s', VtyEvent (Vty.EvKey (Vty.KEnter) [])) -> do
          case listSelectedElement (view testPickerList s') of
            Nothing -> error "nothing selected"
            Just (_, x) ->
              continue . UiVmScreen $
                initialUiVmStateForTest (view testPickerDapp s') x

        (UiTestPickerScreen s', VtyEvent e') -> do
          s'' <- handleEventLensed s'
            testPickerList
            handleListEvent
            e'
          continue (UiTestPickerScreen s'')

        _ -> continue s

  , appStartEvent = return
  , appAttrMap = const (attrMap Vty.defAttr myTheme)
  }

useContinuation
  :: EventM n (Next (UiState Concrete))
  -> (UiVmState Concrete -> UiVmState Concrete)
  -> UiVmState Concrete
  -> EventM n (Next (UiState Concrete))
useContinuation onStop f ui =
  let ui' = f ui
  in case view (uiVm . result) ui' of
    Nothing ->
      continue (UiVmScreen ui')
    Just r ->
      case view uiVmContinuation ui' of
        Stop -> onStop
        Continue k ->
          continue . UiVmScreen $ k r ui'

useContinuationRepeatedly
  :: Int
  -> EventM n (Next (UiState Concrete))
  -> (UiVmState Concrete -> UiVmState Concrete)
  -> UiVmState Concrete
  -> EventM n (Next (UiState Concrete))
useContinuationRepeatedly n onStop f ui = go n ui
  where
    go 0 ui' = continue (UiVmScreen ui')
    go i ui' =
      let ui'' = f ui'
      in case view (uiVm . result) ui'' of
        Nothing ->
          go (i - 1) ui''
        Just r ->
          case view uiVmContinuation ui'' of
            Stop -> onStop
            Continue k ->
              go (i - 1) (k r ui'')

initialUiVmStateForTest :: DappInfo -> (Text, Text) -> UiVmState Concrete
initialUiVmStateForTest dapp (theContractName, theTestName) =
  ui1
  where
    ui1 =
      updateUiVmState ui0 vm0
        & set uiVmFirstState ui1
    ui0 =
      UiVmState
        { _uiVm = vm0
        , _uiVmContinuation = Continue k1
        , _uiVmStackList = undefined
        , _uiVmBytecodeList = undefined
        , _uiVmTraceList = undefined
        , _uiVmSolidityList = undefined
        , _uiVmSolc = Just testContract
        , _uiVmDapp = Just dapp
        , _uiVmStepCount = 0
        , _uiVmFirstState = undefined
        , _uiVmMessage = Just "Creating unit test contract"
        }
    Just testContract =
      view (dappSolcByName . at theContractName) dapp
    vm0 =
      initialUnitTestVm testContract (Map.elems (view dappSolcByName dapp))
    k1 r ui =
      case r of
        VMFailure e ->
          error $ "creation error: " ++ show e
        VMSuccess (B targetCode) ->
          let
            vm1 = view uiVm ui
            target = view (state . contract) vm1
            vm2 = vm1 & env . contracts . ix target . balance +~ 0xffffffffffffffffffffffff
            vm3 = flip execState vm2 $ do
              performCreation targetCode
              setupCall target "setUp()"
          in
            updateUiVmState ui vm3
              & set uiVmContinuation (Continue (k2 target))
              & set uiVmMessage (Just "Calling `setUp()' for unit test")
    k2 target r ui =
      case r of
        VMFailure e ->
          error $ "setUp() error: " ++ show e
        VMSuccess _ ->
          let
            vm3 = view uiVm ui
            vm4 = flip execState vm3 $ do
                    setupCall target theTestName
                    assign contextTrace (Zipper.fromForest [])
                    assign logs mempty
          in
            updateUiVmState ui vm4
              & set uiVmContinuation Stop
              & set uiVmMessage (Just "Running unit test")

myTheme :: [(AttrName, Vty.Attr)]
myTheme =
  [ (selectedAttr, Vty.defAttr `Vty.withStyle` Vty.standout)
  , (dimAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
  , (borderAttr, Vty.defAttr `Vty.withStyle` Vty.dim)
  , (wordAttr, fg Vty.yellow)
  , (boldAttr, Vty.defAttr `Vty.withStyle` Vty.bold)
  , (activeAttr, Vty.defAttr `Vty.withStyle` Vty.standout)
  ]

drawUi :: UiState Concrete -> [UiWidget]
drawUi (UiVmScreen s) = drawVm s
drawUi (UiTestPickerScreen s) = drawTestPicker s

drawTestPicker :: UiTestPickerState -> [UiWidget]
drawTestPicker ui =
  [ center . borderWithLabel (txt "Unit tests") .
      hLimit 80 $
        renderList
          (\selected (x, y) ->
             withHighlight selected $
               txt " Debug " <+> txt (contractNamePart x) <+> txt "::" <+> txt y)
          True
          (view testPickerList ui)
  ]

drawVm :: UiVmState Concrete -> [UiWidget]
drawVm ui =
  [ vBox
    [ vLimit 20 $ hBox
      [ drawStackPane ui <+> vBorder
      , drawTracePane ui
      ]
    , hBox $
      [ hLimit 72 $ drawBytecodePane ui
      , drawSolidityPane ui
      ]
    ]
  ]

stepOneOpcode :: UiVmState Concrete -> UiVmState Concrete
stepOneOpcode ui =
  let
    nextVm = execState exec1 (view uiVm ui)
  in
    updateUiVmState ui nextVm
      & over uiVmStepCount (+ 1)

stepOneSourcePosition :: UiVmState Concrete -> UiVmState Concrete
stepOneSourcePosition ui =
  let
    vm              = view uiVm ui
    Just dapp       = view uiVmDapp ui
    initialPosition = currentSrcMap dapp vm
    stillHere s     = currentSrcMap dapp s == initialPosition
    (i, nextVm)     = runState (execWhile stillHere) vm
  in
    updateUiVmState ui nextVm
      & over uiVmStepCount (+ i)

stepOneSourcePosition_over :: UiVmState Concrete -> UiVmState Concrete
stepOneSourcePosition_over ui =
  let
    vm              = view uiVm ui
    Just dapp       = view uiVmDapp ui
    initialPosition = currentSrcMap dapp vm
    initialHeight   = length (view frames vm)

    predicate x =
      case currentSrcMap dapp x of
        Nothing ->
          True
        Just sm ->
          let
            remain = Just sm == initialPosition
            deeper = length (view frames x) > initialHeight
            boring =
              case srcMapCode (view dappSources dapp) sm of
                Just bs ->
                  BS.isPrefixOf "contract " bs
                Nothing ->
                  True
          in
            remain || deeper || boring

    (i, nextVm)     = runState (execWhile predicate) vm
  in
    updateUiVmState ui nextVm
      & over uiVmStepCount (+ i)

currentSrcMap :: Machine e => DappInfo -> VM e -> Maybe SrcMap
currentSrcMap dapp vm =
  let
    this = vm ^?! env . contracts . ix (view (state . codeContract) vm)
    i = (view opIxMap this) SVec.! (view (state . pc) vm)
    h = view codehash this
  in
    case preview (dappSolcByHash . ix h) dapp of
      Nothing ->
        Nothing
      Just (Creation, solc) ->
        preview (creationSrcmap . ix i) solc
      Just (Runtime, solc) ->
        preview (runtimeSrcmap . ix i) solc

currentSolc :: Machine e => DappInfo -> VM e -> Maybe SolcContract
currentSolc dapp vm =
  let
    this = vm ^?! env . contracts . ix (view (state . contract) vm)
    h = view codehash this
  in
    preview (dappSolcByHash . ix h . _2) dapp

updateUiVmState :: UiVmState Concrete -> VM Concrete -> UiVmState Concrete
updateUiVmState ui vm =
  let
    move = case vmOpIx vm of
             Nothing -> id
             Just x -> listMoveTo x
    ui' = ui
      & set uiVm vm
      & set uiVmStackList
          (list StackPane (Vec.fromList $ zip [1..] (view (state . stack) vm)) 2)
      & set uiVmBytecodeList
          (move $ list BytecodePane
             (Vec.imap (,) (view codeOps (fromJust (currentContract vm))))
             1)
  in
    case view uiVmDapp ui of
      Nothing ->
        ui'
          & set uiVmTraceList (list TracePane mempty 1)
          & set uiVmSolidityList (list SolidityPane mempty 1)
      Just dapp ->
        ui'
          & set uiVmTraceList
              (list
                TracePane
                (Vec.fromList
                 . lines
                 . drawForest
                 . fmap (fmap (unpack . showContext dapp))
                 $ contextTraceForest vm)
                1)
          & set uiVmSolidityList
              (list SolidityPane
                 (case currentSrcMap dapp vm of
                    Nothing -> mempty
                    Just x ->
                      view (dappSources
                            . sourceLines
                            . ix (srcMapFile x)
                            . to (Vec.imap (,)))
                        dapp)
                 1)

maybeContractName :: Maybe SolcContract -> Text
maybeContractName =
  maybe "<unknown contract>" (view (contractName . to contractNamePart))

maybeAbiName :: SolcContract -> Word Concrete -> Maybe Text
maybeAbiName solc abi = preview (abiMap . ix (fromIntegral abi)) solc

showContext :: DappInfo -> Either (Log Concrete) (FrameContext Concrete) -> Text
showContext _ (Left (Log _ bytes topics)) =
  "LOG " <> pack (show bytes) <> " " <> pack (show topics)
showContext dapp (Right (CreationContext hash)) =
  "CREATE " <> maybeContractName (preview (dappSolcByHash . ix hash . _2) dapp)
showContext dapp (Right (CallContext _ _ hash abi _)) =
  case preview (dappSolcByHash . ix hash . _2) dapp of
    Nothing ->
      "CALL [unknown]"
    Just solc ->
      "CALL "
        <> view (contractName . to contractNamePart) solc
        <> " "
        <> maybe "[fallback function]"
             (\x -> maybe "[unknown method]" id (maybeAbiName solc x))
             abi

drawStackPane :: UiVmState Concrete -> UiWidget
drawStackPane ui =
  let
    gasText = showWordExact (view (uiVm . state . gas) ui)
    labelText = txt ("Gas available: " <> gasText <> "; stack:")
  in hBorderWithLabel labelText <=>
    renderList
      (\_ (i, x@(C _ w)) ->
         vBox
           [ withHighlight True (str ("#" ++ show i ++ " "))
               <+> str (show x)
           , dim (str ("   " ++ showWordExplanation w (view uiVmDapp ui)))
           ])
      False
      (view uiVmStackList ui)

showDec :: W256 -> String
showDec (W256 w) =
  if w > 1000000000000
  then
    "~" ++ Scientific.formatScientific
       Scientific.Generic
       (Just 8)
       (fromIntegral w)
  else
    showDecExact (W256 w)

showDecExact :: W256 -> String
showDecExact (W256 w) = unpack (humanizeInteger w)

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

showWordExplanation :: W256 -> Maybe DappInfo -> String
showWordExplanation w Nothing = showDec w
showWordExplanation w _ | w > 0xffffffff = showDec w
showWordExplanation w (Just dapp) =
  let
    fullAbiMap =
      mconcat (map (view abiMap) (Map.elems (view dappSolcByName dapp)))
  in
    case Map.lookup (fromIntegral w) fullAbiMap of
      Nothing -> showDec w
      Just x  -> "abi " ++ show (unpack x)

drawBytecodePane :: UiVmState Concrete -> UiWidget
drawBytecodePane ui =
  hBorderWithLabel (case view uiVmMessage ui of { Nothing -> str ""; Just s -> str s }) <=>
    Centered.renderList
      (\active x -> if not active
                    then withDefAttr dimAttr (opWidget x)
                    else withDefAttr boldAttr (opWidget x))
      False
      (view uiVmBytecodeList ui)

dim :: Widget n -> Widget n
dim = withDefAttr dimAttr

withHighlight :: Bool -> Widget n -> Widget n
withHighlight False = withDefAttr dimAttr
withHighlight True  = withDefAttr boldAttr

drawTracePane :: Machine e => UiVmState e -> UiWidget
drawTracePane ui =
  hBorderWithLabel (txt "Trace") <=>
    renderList
      (\_ x -> str x)
      False
      (view uiVmTraceList ui)

drawSolidityPane :: Machine e => UiVmState e -> UiWidget
drawSolidityPane ui | not (isJust (view uiVmDapp ui)) = vBox []
drawSolidityPane ui =
  let
    Just dapp = view uiVmDapp ui
    sm = fromJust $ currentSrcMap dapp (view uiVm ui)
    rows = fromJust $ view (dappSources . sourceLines . at (srcMapFile sm)) dapp
    subrange i = lineSubrange rows (srcMapOffset sm, srcMapLength sm) i
    lineNo =
      (snd . fromJust $
        (srcMapCodePos
         (view dappSources dapp)
         sm)) - 1
  in vBox
    [ hBorderWithLabel $
        txt (maybe "<unknown>" contractNamePart
              (preview (uiVmSolc . _Just . contractName) ui))
          <+> str (" " ++ show lineNo)
    , Centered.renderList
        (\_ (i, line) ->
           let s = case decodeUtf8 line of "" -> " "; y -> y
           in case subrange i of
                Nothing -> withHighlight False (txt s)
                Just (a, b) ->
                  let (x, y, z) = ( Text.take a s
                                  , Text.take b (Text.drop a s)
                                  , Text.drop (a + b) s
                                  )
                  in hBox [ withHighlight False (txt x)
                          , withHighlight True (txt y)
                          , withHighlight False (txt z)
                          ])
        False
        (listMoveTo lineNo
          (view uiVmSolidityList ui))
    ]

contractNamePart :: Text -> Text
contractNamePart x = Text.split (== ':') x !! 1

contractPathPart :: Text -> Text
contractPathPart x = Text.split (== ':') x !! 0

opWidget :: Show a => (a, Op) -> Widget n
opWidget (i, o) = str (show i ++ " ") <+> case o of
  OpStop -> txt "STOP"
  OpAdd -> txt "ADD"
  OpMul -> txt "MUL"
  OpSub -> txt "SUB"
  OpDiv -> txt "DIV"
  OpSdiv -> txt "SDIV"
  OpMod -> txt "MOD"
  OpSmod -> txt "SMOD"
  OpAddmod -> txt "ADDMOD"
  OpMulmod -> txt "MULMOD"
  OpExp -> txt "EXP"
  OpSignextend -> txt "SIGNEXTEND"
  OpLt -> txt "LT"
  OpGt -> txt "GT"
  OpSlt -> txt "SLT"
  OpSgt -> txt "SGT"
  OpEq -> txt "EQ"
  OpIszero -> txt "ISZERO"
  OpAnd -> txt "AND"
  OpOr -> txt "OR"
  OpXor -> txt "XOR"
  OpNot -> txt "NOT"
  OpByte -> txt "BYTE"
  OpSha3 -> txt "SHA3"
  OpAddress -> txt "ADDRESS"
  OpBalance -> txt "BALANCE"
  OpOrigin -> txt "ORIGIN"
  OpCaller -> txt "CALLER"
  OpCallvalue -> txt "CALLVALUE"
  OpCalldataload -> txt "CALLDATALOAD"
  OpCalldatasize -> txt "CALLDATASIZE"
  OpCalldatacopy -> txt "CALLDATACOPY"
  OpCodesize -> txt "CODESIZE"
  OpCodecopy -> txt "CODECOPY"
  OpGasprice -> txt "GASPRICE"
  OpExtcodesize -> txt "EXTCODESIZE"
  OpExtcodecopy -> txt "EXTCODECOPY"
  OpBlockhash -> txt "BLOCKHASH"
  OpCoinbase -> txt "COINBASE"
  OpTimestamp -> txt "TIMESTAMP"
  OpNumber -> txt "NUMBER"
  OpDifficulty -> txt "DIFFICULTY"
  OpGaslimit -> txt "GASLIMIT"
  OpPop -> txt "POP"
  OpMload -> txt "MLOAD"
  OpMstore -> txt "MSTORE"
  OpMstore8 -> txt "MSTORE8"
  OpSload -> txt "SLOAD"
  OpSstore -> txt "SSTORE"
  OpJump -> txt "JUMP"
  OpJumpi -> txt "JUMPI"
  OpPc -> txt "PC"
  OpMsize -> txt "MSIZE"
  OpGas -> txt "GAS"
  OpJumpdest -> txt "JUMPDEST"
  OpCreate -> txt "CREATE"
  OpCall -> txt "CALL"
  OpCallcode -> txt "CALLCODE"
  OpReturn -> txt "RETURN"
  OpDelegatecall -> txt "DELEGATECALL"
  OpSelfdestruct -> txt "SELFDESTRUCT"
  OpDup x -> txt "DUP" <+> str (show x)
  OpSwap x -> txt "SWAP" <+> str (show x)
  OpLog x -> txt "LOG" <+> str (show x)
  OpPush x -> txt "PUSH " <+> withDefAttr wordAttr (str (show x))
  OpRevert -> txt "REVERT"
  OpUnknown x -> txt "UNKNOWN " <+> str (show x)

selectedAttr :: AttrName; selectedAttr = "selected"
dimAttr :: AttrName; dimAttr = "dim"
wordAttr :: AttrName; wordAttr = "word"
boldAttr :: AttrName; boldAttr = "bold"
activeAttr :: AttrName; activeAttr = "active"
