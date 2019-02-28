{-# Language TemplateHaskell #-}
{-# Language ImplicitParams #-}

module EVM.TTY where

import Prelude hiding (Word)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import EVM
import EVM.ABI (abiTypeSolidity)
import EVM.Concrete (Word (C))
import EVM.Dapp (DappInfo, dappInfo)
import EVM.Dapp (dappUnitTests, dappSolcByName, dappSolcByHash, dappSources)
import EVM.Dapp (dappAstSrcMap)
import EVM.Debug
import EVM.Format (Signedness (..), showDec, showWordExact)
import EVM.Format (showTraceTree)
import EVM.Format (contractNamePart, contractPathPart)
import EVM.Hexdump (prettyHex)
import EVM.Op
import EVM.Solidity
import EVM.Types hiding (padRight)
import EVM.UnitTest (UnitTestOptions (..))
import EVM.UnitTest (initialUnitTestVm)
import EVM.UnitTest (initializeUnitTest, runUnitTest)
import EVM.StorageLayout

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import EVM.Fetch (Fetcher)
import qualified EVM.Fetch as Fetch

import Control.Lens
import Control.Monad.State.Strict hiding (state)

import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (sort)
import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec
import qualified Graphics.Vty as Vty
import qualified System.Console.Haskeline as Readline

import qualified EVM.TTYCenteredList as Centered

data Name
  = AbiPane
  | StackPane
  | BytecodePane
  | TracePane
  | SolidityPane
  | SolidityViewport
  | TestPickerPane
  | BrowserPane
  deriving (Eq, Show, Ord)

type UiWidget = Widget Name

data UiVmState = UiVmState
  { _uiVm             :: VM
  , _uiVmNextStep     :: Stepper ()
  , _uiVmStackList    :: List Name (Int, Word)
  , _uiVmBytecodeList :: List Name (Int, Op)
  , _uiVmTraceList    :: List Name Text
  , _uiVmSolidityList :: List Name (Int, ByteString)
  , _uiVmSolc         :: Maybe SolcContract
  , _uiVmDapp         :: Maybe DappInfo
  , _uiVmStepCount    :: Int
  , _uiVmFirstState   :: UiVmState
  , _uiVmMessage      :: Maybe String
  , _uiVmNotes        :: [String]
  , _uiVmShowMemory   :: Bool
  }

data UiTestPickerState = UiTestPickerState
  { _testPickerList :: List Name (Text, Text)
  , _testPickerDapp :: DappInfo
  }

data UiBrowserState = UiBrowserState
  { _browserContractList :: List Name (Addr, Contract)
  , _browserVm :: UiVmState
  }

data UiState
  = UiVmScreen UiVmState
  | UiVmBrowserScreen UiBrowserState
  | UiTestPickerScreen UiTestPickerState

makeLenses ''UiVmState
makeLenses ''UiTestPickerState
makeLenses ''UiBrowserState
makePrisms ''UiState

type Pred a = a -> Bool

data StepMode
  = StepOne                        -- ^ Finish after one opcode step
  | StepMany !Int                  -- ^ Run a specific number of steps
  | StepNone                       -- ^ Finish before the next opcode
  | StepUntil (Pred VM)            -- ^ Finish when a VM predicate holds

-- | Each step command in the terminal should finish immediately
-- with one of these outcomes.
data StepOutcome a
  = Returned a                -- ^ Program finished
  | Stepped  (Stepper a)      -- ^ Took one step; more steps to go
  | Blocked  (IO (Stepper a)) -- ^ Came across blocking request

-- | This turns a @Stepper@ into a state action usable
-- from within the TTY loop, yielding a @StepOutcome@ depending on the @StepMode@.
interpret
  :: (?fetcher :: Fetcher)
  => StepMode
  -> Stepper a
  -> State UiVmState (StepOutcome a)
interpret mode =

  -- Like the similar interpreters in @EVM.UnitTest@ and @EVM.VMTest@,
  -- this one is implemented as an "operational monad interpreter".

  eval . Operational.view
  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> State UiVmState (StepOutcome a)

    eval (Operational.Return x) =
      pure (Returned x)

    eval (action Operational.:>>= k) =
      case action of

        -- Stepper wants to keep executing?
        Stepper.Exec -> do

          let
            -- When pausing during exec, we should later restart
            -- the exec with the same continuation.
            restart = Stepper.exec >>= k

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
                  pure (Stepped restart)
                Just r ->
                  -- If returning, proceed directly the continuation,
                  -- but stopping before the next instruction.
                  interpret StepNone (k r)

            StepMany 0 -> do
              -- Finish the continuation until the next instruction;
              -- then, pause & await user.
              interpret StepNone restart

            StepMany i -> do
              -- Run one instruction.
              interpret StepOne restart >>=
                \case
                  Stepped stepper ->
                    interpret (StepMany (i - 1)) stepper

                  -- This shouldn't happen, because re-stepping needs
                  -- to avoid blocking and halting.
                  r -> pure r

            StepUntil p -> do
              vm <- use uiVm
              case p vm of
                True ->
                  interpret StepNone restart
                False ->
                  interpret StepOne restart >>=
                    \case
                      Stepped stepper ->
                        interpret (StepUntil p) stepper

                      -- This means that if we hit a blocking query
                      -- or a return, we pause despite the predicate.
                      --
                      -- This could be fixed if we allowed query I/O
                      -- here, instead of only in the TTY event loop;
                      -- let's do it later.
                      r -> pure r

        -- Stepper wants to make a query and wait for the results?
        Stepper.Wait q -> do
          -- Tell the TTY to run an I/O action to produce the next stepper.
          pure . Blocked $ do
            -- First run the fetcher, getting a VM state transition back.
            m <- ?fetcher q
            -- Join that transition with the stepper script's continuation.
            pure (Stepper.evm m >> k ())

        -- Stepper wants to modify the VM.
        Stepper.EVM m -> do
          vm0 <- use uiVm
          let (r, vm1) = runState m vm0
          modify (flip updateUiVmState vm1)
          interpret mode (k r)

        -- Stepper wants to emit a message.
        Stepper.Note s -> do
          assign uiVmMessage (Just (unpack s))
          modifying uiVmNotes (unpack s :)
          interpret mode (k ())

        -- Stepper wants to exit because of a failure.
        Stepper.Fail e ->
          error ("VM error: " ++ show e)

isUnitTestContract :: Text -> DappInfo -> Bool
isUnitTestContract name dapp =
  elem name (map fst (view dappUnitTests dapp))

mkVty :: IO Vty.Vty
mkVty = do
  vty <- Vty.mkVty Vty.defaultConfig
  Vty.setMode (Vty.outputIface vty) Vty.BracketedPaste True
  return vty

runFromVM :: VM -> IO VM
runFromVM vm = do
  let
    ui0 = UiVmState
           { _uiVm = vm
           , _uiVmNextStep =
               void Stepper.execFully >> Stepper.evm (finalize False)
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
           , _uiVmShowMemory = False
           }
    ui1 = updateUiVmState ui0 vm & set uiVmFirstState ui1

    testOpts = UnitTestOptions
      { oracle            = Fetch.zero
      , verbose           = False
      , match             = ""
      , vmModifier        = id
      , testParams        = error "irrelevant"
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
          dapp = dappInfo root contractMap sourceCache
          ui = UiTestPickerScreen $ UiTestPickerState
            { _testPickerList =
                list
                  TestPickerPane
                  (Vec.fromList
                   (concatMap
                    (\(a, xs) -> [(a, x) | x <- xs])
                    (view dappUnitTests dapp)))
                  1
            , _testPickerDapp = dapp
            }

        _ <- customMain mkVty Nothing (app opts) (ui :: UiState)
        return ()

-- ^ Specifies whether to do I/O blocking or VM halting while stepping.
-- When we step backwards, we don't want to allow those things.
data StepPolicy
  = StepNormally    -- ^ Allow blocking and returning
  | StepTimidly     -- ^ Forbid blocking and returning

takeStep
  :: (?fetcher :: Fetcher)
  => UiVmState
  -> StepPolicy
  -> StepMode
  -> EventM n (Next UiState)
takeStep ui policy mode = do
  let m = interpret mode (view uiVmNextStep ui)

  case runState (m <* modify renderVm) ui of

    (Stepped stepper, ui') -> do
      continue (UiVmScreen (ui' & set uiVmNextStep stepper))

    (Blocked blocker, ui') ->
      case policy of
        StepNormally -> do
          stepper <- liftIO blocker
          takeStep
            (execState (assign uiVmNextStep stepper) ui')
            StepNormally StepNone

        StepTimidly ->
          error "step blocked unexpectedly"

    (Returned (), ui') ->
      case policy of
        StepNormally ->
          halt (UiVmScreen ui')
        StepTimidly ->
          error "step halted unexpectedly"

app :: UnitTestOptions -> App UiState () Name
app opts =
  let ?fetcher = oracle opts
  in App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = \ui e ->

      case (ui, e) of
        (UiVmBrowserScreen s, VtyEvent (Vty.EvKey Vty.KEsc [])) ->
          continue (UiVmScreen (view browserVm s))

        (UiVmBrowserScreen s, VtyEvent e'@(Vty.EvKey Vty.KDown [])) -> do
          s' <- handleEventLensed s
            browserContractList
            handleListEvent
            e'
          continue (UiVmBrowserScreen s')

        (UiVmBrowserScreen s, VtyEvent e'@(Vty.EvKey Vty.KUp [])) -> do
          s' <- handleEventLensed s
            browserContractList
            handleListEvent
            e'
          continue (UiVmBrowserScreen s')

        (_, VtyEvent (Vty.EvKey Vty.KEsc [])) ->
          halt ui

        (UiVmScreen s, VtyEvent (Vty.EvKey Vty.KEnter [])) ->
            continue . UiVmBrowserScreen $ UiBrowserState
              { _browserContractList =
                  list
                    BrowserPane
                    (Vec.fromList (Map.toList (view (uiVm . env . contracts) s)))
                    2
              , _browserVm = s
              }

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) ->
          let
            loop = do
              Just hey <- Readline.getInputLine "% "
              Readline.outputStrLn hey
              Just hey' <- Readline.getInputLine "% "
              Readline.outputStrLn hey'
              return (UiVmScreen s)
          in
            suspendAndResume $
              Readline.runInputT Readline.defaultSettings loop


        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'n') [])) ->
          takeStep s StepNormally StepOne

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'N') [])) ->
          takeStep s
            StepNormally
            (StepUntil (isNextSourcePosition s))

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl])) ->
          takeStep s
            StepNormally
            (StepUntil (isNextSourcePositionWithoutEntering s))

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'p') [])) ->
          case view uiVmStepCount s of
            0 ->
              -- We're already at the first step; ignore command.
              continue ui

            n -> do
              -- To step backwards, we revert to the first state
              -- and execute n - 1 instructions from there.
              --
              -- We keep the current cache so we don't have to redo
              -- any blocking queries, and also the memory view.
              let
                s0 = view uiVmFirstState s
                s1 = set (uiVm . cache)   (view (uiVm . cache) s) s0
                s2 = set (uiVmShowMemory) (view uiVmShowMemory s) s1

              -- Take n steps; "timidly," because all queries
              -- ought to be cached.
              takeStep s2 StepTimidly (StepMany (n - 1))

        (UiVmScreen s, VtyEvent (Vty.EvKey (Vty.KChar 'm') [])) ->
          continue (UiVmScreen (over uiVmShowMemory not s))

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
  -> UiVmState
initialUiVmStateForTest opts@(UnitTestOptions {..}) dapp (theContractName, theTestName) =
  ui1
  where
    script = do
      Stepper.evm . pushTrace . EntryTrace $
        "test " <> theTestName <> " (" <> theContractName <> ")"
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
        , _uiVmShowMemory   = False
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

drawUi :: UiState -> [UiWidget]
drawUi (UiVmScreen s) = drawVm s
drawUi (UiTestPickerScreen s) = drawTestPicker s
drawUi (UiVmBrowserScreen s) = drawVmBrowser s

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

drawVmBrowser :: UiBrowserState -> [UiWidget]
drawVmBrowser ui =
  [ hBox
      [ borderWithLabel (txt "Contracts") .
          hLimit 60 $
            renderList
              (\selected (k, c) ->
                 withHighlight selected . txt . mconcat $
                   [ fromMaybe "<unknown contract>" . flip preview ui $
                       ( browserVm . uiVmDapp . _Just . dappSolcByHash . ix (view codehash c)
                       . _2 . contractName )
                   , "\n"
                   , "  ", pack (show k)
                   ])
              True
              (view browserContractList ui)
      , let
          Just (_, (_, c)) = listSelectedElement (view browserContractList ui)
          Just dapp = view (browserVm . uiVmDapp) ui
        in case flip preview ui (browserVm . uiVmDapp . _Just . dappSolcByHash . ix (view codehash c) . _2) of
          Nothing -> txt ("n/a; codehash " <> pack (show (view codehash c)))
          Just solc ->
            hBox
              [ borderWithLabel (txt "Contract information") . padBottom Max . padRight (Pad 2) $ vBox
                  [ txt "Name: " <+> txt (contractNamePart (view contractName solc))
                  , txt "File: " <+> txt (contractPathPart (view contractName solc))
                  , txt " "
                  , txt "Constructor inputs:"
                  , vBox . flip map (view constructorInputs solc) $
                      \(name, abiType) -> txt ("  " <> name <> ": " <> abiTypeSolidity abiType)
                  , txt "Public methods:"
                  , vBox . flip map (sort (Map.elems (view abiMap solc))) $
                      \method -> txt ("  " <> view methodSignature method)
                  ]
                , borderWithLabel (txt "Storage slots") . padBottom Max . padRight Max $ vBox
                    (map txt (storageLayout dapp solc))
                ]
      ]
  ]

drawVm :: UiVmState -> [UiWidget]
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
        , vLimit 2 $ drawHelpBar
        ]
      )
      ( vBox
        [ hBox
          [ vLimit 20 $ drawBytecodePane ui
          , vLimit 20 $ drawStackPane ui
          ]
        , hBox $
          [ drawSolidityPane ui
          , drawTracePane ui
          ]
        , vLimit 2 $ drawHelpBar
        ]
      )
  ]

drawHelpBar :: UiWidget
drawHelpBar = hBorder <=> hCenter help
  where
    help =
      hBox (map (\(k, v) -> txt k <+> dim (txt (" (" <> v <> ")  "))) helps)

    helps =
      [
        ("n", "step")
      , ("p", "step back")
      , ("N", "step more")
      , ("C-n", "step over")
--      , ("  Enter", "browse")
      , ("m", "toggle memory")
      , ("  Esc", "exit")
      ]

stepOneOpcode :: UiVmState -> UiVmState
stepOneOpcode ui =
  let
    nextVm = execState exec1 (view uiVm ui)
  in
    ui & over uiVmStepCount (+ 1)
       & set uiVm nextVm

isNextSourcePosition
  :: UiVmState -> Pred VM
isNextSourcePosition ui vm =
  let
    Just dapp       = view uiVmDapp ui
    initialPosition = currentSrcMap dapp (view uiVm ui)
  in
    currentSrcMap dapp vm /= initialPosition

isNextSourcePositionWithoutEntering
  :: UiVmState -> Pred VM
isNextSourcePositionWithoutEntering ui vm =
  let
    vm0             = view uiVm ui
    Just dapp       = view uiVmDapp ui
    initialPosition = currentSrcMap dapp vm0
    initialHeight   = length (view frames vm0)
  in
    case currentSrcMap dapp vm of
      Nothing ->
        True
      Just here ->
        let
          moved = Just here /= initialPosition
          deeper = length (view frames vm) > initialHeight
          boring =
            case srcMapCode (view dappSources dapp) here of
              Just bs ->
                BS.isPrefixOf "contract " bs
              Nothing ->
                True
        in
           moved && not deeper && not boring

currentSrcMap :: DappInfo -> VM -> Maybe SrcMap
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

currentSolc :: DappInfo -> VM -> Maybe SolcContract
currentSolc dapp vm =
  let
    this = vm ^?! env . contracts . ix (view (state . contract) vm)
    h = view codehash this
  in
    preview (dappSolcByHash . ix h . _2) dapp

renderVm :: UiVmState -> UiVmState
renderVm ui = updateUiVmState ui (view uiVm ui)

updateUiVmState :: UiVmState -> VM -> UiVmState
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
             (view codeOps (fromJust (currentContract vm)))
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
                 . Text.lines
                 . showTraceTree dapp
                 $ vm)
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

drawStackPane :: UiVmState -> UiWidget
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
           , dim (txt ("   " <> showWordExplanation w (view uiVmDapp ui)))
           ])
      False
      (view uiVmStackList ui)

showWordExplanation :: W256 -> Maybe DappInfo -> Text
showWordExplanation w Nothing = showDec Unsigned w
showWordExplanation w _ | w > 0xffffffff = showDec Unsigned w
showWordExplanation w (Just dapp) =
  let
    fullAbiMap =
      mconcat (map (view abiMap) (Map.elems (view dappSolcByName dapp)))
  in
    case Map.lookup (fromIntegral w) fullAbiMap of
      Nothing -> showDec Unsigned w
      Just x  -> "keccak(\"" <> view methodSignature x <> "\")"

drawBytecodePane :: UiVmState -> UiWidget
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

drawTracePane :: UiVmState -> UiWidget
drawTracePane ui =
  case view uiVmShowMemory ui of
    False ->
      hBorderWithLabel (txt "Trace") <=>
        renderList
          (\_ x -> txt x)
          False
          (view uiVmTraceList ui)
    True ->
      vBox
      [ hBorderWithLabel (txt "Calldata") <=>
          str (prettyHex 40 (view (uiVm . state . calldata) ui))
      , hBorderWithLabel (txt "Returndata") <=>
          str (prettyHex 40 (view (uiVm . state . returndata) ui))
      , hBorderWithLabel (txt "Memory") <=>
          str (prettyHex 40 (view (uiVm . state . memory) ui))
      ]

drawSolidityPane :: UiVmState -> UiWidget
drawSolidityPane ui@(view uiVmDapp -> Just dapp) =
  case currentSrcMap dapp (view uiVm ui) of
    Nothing -> padBottom Max (hBorderWithLabel (txt "<no source map>"))
    Just sm ->
      case view (dappSources . sourceLines . at (srcMapFile sm)) dapp of
        Nothing -> padBottom Max (hBorderWithLabel (txt "<source not found>"))
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
                txt (maybe "<unknown>" contractPathPart
                      (preview (uiVmSolc . _Just . contractName) ui))
                  <+> str (":" ++ show lineNo)

                  -- Show the AST node type if present
                  <+> txt (" (" <> fromMaybe "?"
                                    ((view dappAstSrcMap dapp) sm
                                       >>= preview (key "name" . _String)) <> ")")
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

showPc :: (Integral a, Show a) => a -> String
showPc x =
  if x < 0x10
  then '0' : showHex x ""
  else showHex x ""

opWidget :: (Integral a, Show a) => (a, Op) -> Widget n
opWidget (i, o) = str (showPc i <> " ") <+> case o of
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
  OpShl -> txt "SHL"
  OpShr -> txt "SHR"
  OpSar -> txt "SAR"
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
  OpReturndatasize -> txt "RETURNDATASIZE"
  OpReturndatacopy -> txt "RETURNDATACOPY"
  OpExtcodehash -> txt "EXTCODEHASH"
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
  OpStaticcall -> txt "STATICCALL"
  OpCallcode -> txt "CALLCODE"
  OpReturn -> txt "RETURN"
  OpDelegatecall -> txt "DELEGATECALL"
  OpCreate2 -> txt "CREATE2"
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
