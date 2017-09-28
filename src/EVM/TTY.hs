{-# Language TemplateHaskell #-}

module EVM.TTY where

import Prelude hiding (Word)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import EVM
import EVM.Concrete (Concrete, Word (C))
import EVM.Debug
import EVM.Machine (Machine)
import EVM.Op
import EVM.Solidity
import EVM.Types
import EVM.UnitTest (UnitTestOptions (..))
import EVM.UnitTest (initializeUnitTest, runUnitTest)
import EVM.UnitTest (initialUnitTestVm, findUnitTests)

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import qualified EVM.Fetch as Fetch

import Control.Lens
import Control.Monad.State.Strict hiding (state)

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tree (drawForest)

import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
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

data UiVmState e = UiVmState
  { _uiVm             :: VM e
  , _uiVmNextStep     :: Stepper Concrete ()
  , _uiVmStackList    :: List Name (Int, Word e)
  , _uiVmBytecodeList :: List Name (Int, Op)
  , _uiVmTraceList    :: List Name String
  , _uiVmSolidityList :: List Name (Int, ByteString)
  , _uiVmSolc         :: Maybe SolcContract
  , _uiVmDapp         :: Maybe DappInfo
  , _uiVmStepCount    :: Int
  , _uiVmFirstState   :: UiVmState e
  , _uiVmMessage      :: Maybe String
  , _uiVmNotes        :: [String]
  }

data CodeType = Creation | Runtime
  deriving (Show, Eq, Ord)

data DappInfo = DappInfo
  { _dappRoot       :: FilePath
  , _dappSolcByName :: Map Text SolcContract
  , _dappSolcByHash :: Map W256 (CodeType, SolcContract)
  , _dappSources    :: SourceCache
  , _dappUnitTests  :: [(Text, [Text])]
  } deriving Eq

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

data StepMode
  = StepOne        -- ^ Finish after one opcode step
  | StepMany !Int  -- ^ Run a specific number of steps
  | StepNone       -- ^ Finish before the next opcode
  deriving Show

-- | Each step command in the terminal should finish immediately
-- with one of these outcomes.
data StepOutcome e a
  = Returned a                         -- ^ Program finished
  | Stepped  (Stepper Concrete a)      -- ^ Took one step; more steps to go
  | Blocked  (IO (Stepper Concrete a)) -- ^ Came across blocking request

-- | This turns a @Stepper@ into a state action usable
-- from within the TTY loop, yielding a @StepOutcome@ depending on the @StepMode@.
interpret
  :: StepMode
  -> Fetch.Fetcher
  -> Stepper Concrete a
  -> State (UiVmState Concrete) (StepOutcome Concrete a)
interpret mode fetcher =

  -- Like the similar interpreters in @EVM.UnitTest@ and @EVM.VMTest@,
  -- this one is implemented as an "operational monad interpreter".

  eval . Operational.view
  where
    eval
      :: Operational.ProgramView (Stepper.Action Concrete) a
      -> State (UiVmState Concrete) (StepOutcome Concrete a)

    eval (Operational.Return x) =
      pure (Returned x)

    eval (action Operational.:>>= k) =
      case action of

        -- Stepper wants to keep executing?
        Stepper.Exec -> do
          case mode of
            StepNone ->
              -- We come here when we've continued while stepping,
              -- either from a query or from a return;
              -- we should pause here and wait for the user.
              pure (Stepped (Operational.singleton action >>= k))

            StepOne -> do
              -- Run an instruction
              modify stepOneOpcode

              use (uiVm . result) >>= \case
                Nothing ->
                  -- If instructions remain, then pause & await user.
                  pure (Stepped (Stepper.exec >>= k))
                Just r ->
                  -- If returning, proceed directly the continuation,
                  -- but stopping before the next instruction.
                  interpret StepNone fetcher (k r)

            StepMany 0 -> do
              -- Finish the continuation until the next instruction;
              -- then, pause & await user.
              interpret StepNone fetcher (Stepper.exec >>= k)

            StepMany i -> do
              -- Run one instruction.
              interpret StepOne fetcher (Stepper.exec >>= k) >>=
                \case
                  Stepped stepper ->
                    interpret (StepMany (i - 1)) fetcher stepper

                  -- This shouldn't happen, because re-stepping needs
                  -- to avoid blocking and halting.
                  r -> pure r

              -- use (uiVm . result) >>= \case
              --   Nothing ->
              --     -- If instructions remain, keep going.
              --     interpret (StepMany (i - 1)) fetcher (Stepper.exec >>= k)
              --   Just r  ->
              --     -- If returning, proceed with the continuation.
              --     interpret (StepMany (i - 1)) fetcher (k r)

        -- Stepper wants to make a query and wait for the results?
        Stepper.Wait q -> do
          -- Tell the TTY to run an I/O action to produce the next stepper.
          pure . Blocked $ do
            -- First run the fetcher, getting a VM state transition back.
            m <- fetcher q
            -- Join that transition with the stepper script's continuation.
            pure (Stepper.evm m >> k ())

        -- Stepper wants to modify the VM.
        Stepper.EVM m -> do
          vm0 <- use uiVm
          let (r, vm1) = runState m vm0
          modify (flip updateUiVmState vm1)
          interpret mode fetcher (k r)

        -- Stepper wants to emit a message.
        Stepper.Note s -> do
          assign uiVmMessage (Just (unpack s))
          modifying uiVmNotes (unpack s :)
          interpret mode fetcher (k ())

        -- Stepper wants to exit because of a failure.
        Stepper.Fail _ ->
          error "how to show errors in TTY?"

isUnitTestContract :: Text -> DappInfo -> Bool
isUnitTestContract name dapp =
  elem name (map fst (view dappUnitTests dapp))

mkVty :: IO Vty.Vty
mkVty = do
  vty <- Vty.mkVty Vty.defaultConfig
  Vty.setMode (Vty.outputIface vty) Vty.BracketedPaste True
  return vty

runFromVM :: VM Concrete -> IO (VM Concrete)
runFromVM vm = do
  let
    ui0 = UiVmState
           { _uiVm = vm
           , _uiVmNextStep = void Stepper.execFully
           , _uiVmStackList = undefined
           , _uiVmBytecodeList = undefined
           , _uiVmTraceList = undefined
           , _uiVmSolidityList = undefined
           , _uiVmSolc = Nothing
           , _uiVmDapp = Nothing
           , _uiVmStepCount = 0
           , _uiVmFirstState = undefined
           , _uiVmMessage = Just "Executing EVM code"
           , _uiVmNotes = []
           }
    ui1 = updateUiVmState ui0 vm & set uiVmFirstState ui1

    testOpts = UnitTestOptions
      { oracle            = Fetch.zero
      , gasForCreating    = error "irrelevant"
      , gasForInvoking    = error "irrelevant"
      , balanceForCreator = error "irrelevant"
      , balanceForCreated = error "irrelevant"
      }

  ui2 <- customMain mkVty Nothing (app testOpts) (UiVmScreen ui1)
  case ui2 of
    UiVmScreen ui -> return (view uiVm ui)
    _ -> error "internal error: customMain returned prematurely"

main :: UnitTestOptions -> FilePath -> FilePath -> IO ()
main opts root jsonFilePath = do
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

        _ <- customMain mkVty Nothing (app opts) (ui :: UiState Concrete)
        return ()

app :: UnitTestOptions -> App (UiState Concrete) () Name
app opts = App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = \ui e ->

      case (ui, e) of
        (_, VtyEvent (Vty.EvKey Vty.KEsc []) )->
          halt ui

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'n') [])) -> do
          let
            run mode s' = do
              let m = interpret mode (oracle opts) (view uiVmNextStep s')
              case runState (m <* modify renderVm) s' of
                (Blocked blocker, s'') -> do
                  stepper <- liftIO blocker
                  run StepNone $ execState (assign uiVmNextStep stepper) s''
                (Stepped stepper, s'') -> do
                  continue (UiVmScreen (s'' & set uiVmNextStep stepper))
                (Returned (), s'') ->
                  halt (UiVmScreen s'')
           in
            run StepOne s

          --useContinuation halt stepOneOpcode s'

        -- (UiVmScreen s', VtyEvent (Vty.EvKey (Vty.KChar 'N') [])) ->
        --   useContinuation halt stepOneSourcePosition s'

        -- (UiVmScreen s', VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) ->
        --   useContinuation halt stepOneSourcePosition_over s'

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'p') [])) ->
          let
            n  = view uiVmStepCount s - 1
            s0 = view uiVmFirstState s
            s1 = set (uiVm . cache) (view (uiVm . cache) s) s0
            m  = interpret (StepMany n) (oracle opts) (view uiVmNextStep s1)
          in
            if n == -1
            then continue ui
            else case runState (m <* modify renderVm) s1 of
              (Stepped stepper, s2) -> do
                continue (UiVmScreen (s2 & set uiVmNextStep stepper))
              (Blocked _, _) -> do
                error "blocked while rewinding"
              (Returned (), _) ->
                error "exited while rewinding"

          -- in useContinuationRepeatedly n halt stepOneOpcode
          --      (view uiVmFirstState s')

        (UiTestPickerScreen s', VtyEvent (Vty.EvKey (Vty.KEnter) [])) -> do
          case listSelectedElement (view testPickerList s') of
            Nothing -> error "nothing selected"
            Just (_, x) ->
              continue . UiVmScreen $
                initialUiVmStateForTest opts (view testPickerDapp s') x

        (UiTestPickerScreen s', VtyEvent e') -> do
          s'' <- handleEventLensed s'
            testPickerList
            handleListEvent
            e'
          continue (UiTestPickerScreen s'')

        _ ->
          continue ui

  , appStartEvent = return
  , appAttrMap = const (attrMap Vty.defAttr myTheme)
  }

initialUiVmStateForTest
  :: UnitTestOptions
  -> DappInfo
  -> (Text, Text)
  -> UiVmState Concrete
initialUiVmStateForTest opts@(UnitTestOptions {..}) dapp (theContractName, theTestName) =
  ui1
  where
    script = do
      initializeUnitTest opts
      void (runUnitTest opts theTestName)
    ui0 =
      UiVmState
        { _uiVm             = vm0
        , _uiVmNextStep     = script
        , _uiVmStackList    = undefined
        , _uiVmBytecodeList = undefined
        , _uiVmTraceList    = undefined
        , _uiVmSolidityList = undefined
        , _uiVmSolc         = Just testContract
        , _uiVmDapp         = Just dapp
        , _uiVmStepCount    = 0
        , _uiVmFirstState   = undefined
        , _uiVmMessage      = Just "Creating unit test contract"
        , _uiVmNotes        = []
        }
    Just testContract =
      view (dappSolcByName . at theContractName) dapp
    vm0 =
      initialUnitTestVm opts testContract (Map.elems (view dappSolcByName dapp))
    ui1 =
      updateUiVmState ui0 vm0 & set uiVmFirstState ui1

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
  -- EVM debugging needs a lot of space because of the 256-bit words
  -- in both the bytecode and the stack .
  --
  -- If on a very tall display, prefer a vertical layout.
  --
  -- Actually the horizontal layout would be preferrable if the display
  -- is both very tall and very wide, but this is okay for now.
  [ ifTallEnough (20 * 4)
      ( vBox
        [ vLimit 20 $ drawBytecodePane ui
        , vLimit 20 $ drawStackPane ui
        , drawSolidityPane ui
        , vLimit 20 $ drawTracePane ui
        ]
      )
      ( vBox
        [ hBox
          [ vLimit 20 $ drawBytecodePane ui
          , vLimit 20 $ drawStackPane ui
          ]
        , hBox $
          [ drawSolidityPane ui
          , vLimit 20 $ drawTracePane ui
          ]
        ]
      )
  ]

stepOneOpcode :: UiVmState Concrete -> UiVmState Concrete
stepOneOpcode ui =
  let
    nextVm = execState exec1 (view uiVm ui)
  in
    ui & over uiVmStepCount (+ 1)
       & set uiVm nextVm

-- stepOneSourcePosition :: UiVmState Concrete -> UiVmState Concrete
-- stepOneSourcePosition ui =
--   let
--     vm              = view uiVm ui
--     Just dapp       = view uiVmDapp ui
--     initialPosition = currentSrcMap dapp vm
--     stillHere s     = currentSrcMap dapp s == initialPosition
--     (i, nextVm)     = runState (execWhile stillHere) vm
--   in
--     ui & set uiVm nextVm
--        & over uiVmStepCount (+ i)

-- stepOneSourcePosition_over :: UiVmState Concrete -> UiVmState Concrete
-- stepOneSourcePosition_over ui =
--   let
--     vm              = view uiVm ui
--     Just dapp       = view uiVmDapp ui
--     initialPosition = currentSrcMap dapp vm
--     initialHeight   = length (view frames vm)

--     predicate x =
--       case currentSrcMap dapp x of
--         Nothing ->
--           True
--         Just sm ->
--           let
--             remain = Just sm == initialPosition
--             deeper = length (view frames x) > initialHeight
--             boring =
--               case srcMapCode (view dappSources dapp) sm of
--                 Just bs ->
--                   BS.isPrefixOf "contract " bs
--                 Nothing ->
--                   True
--           in
--             remain || deeper || boring

--     (i, nextVm)     = runState (execWhile predicate) vm
--   in
--     ui & over uiVmStepCount (+ i)

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

renderVm :: UiVmState Concrete -> UiVmState Concrete
renderVm ui = updateUiVmState ui (view uiVm ui)

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
  if w == num cheatCode
  then "<hevm cheat address>"
  else
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
  hBorderWithLabel (case view uiVmMessage ui of { Nothing -> str ""; Just s -> str (show (view uiVmStepCount ui) ++ ":" ++ s) }) <=>
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
drawSolidityPane ui@(view uiVmDapp -> Just dapp) =
  case currentSrcMap dapp (view uiVm ui) of
    Nothing -> hBorderWithLabel (txt "<no source map>")
    Just sm ->
      case view (dappSources . sourceLines . at (srcMapFile sm)) dapp of
        Nothing -> hBorderWithLabel (txt "<source not found>")
        Just rows ->
          let
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
drawSolidityPane _ =
  -- When e.g. debugging raw EVM code without dapp info,
  -- don't show a Solidity pane.
  vBox []

ifTallEnough :: Int -> Widget n -> Widget n -> Widget n
ifTallEnough need w1 w2 =
  Widget Greedy Greedy $ do
    c <- getContext
    if view availHeightL c > need
      then render w1
      else render w2

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
