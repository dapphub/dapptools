{-# Language TemplateHaskell #-}
{-# Language ImplicitParams #-}
{-# Language DataKinds #-}
module EVM.TTY where

import Prelude hiding (lookup, Word)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import EVM
import EVM.ABI (abiTypeSolidity, decodeAbiValue, AbiType(..), emptyAbi)
import EVM.Symbolic (SymWord(..))
import EVM.SymExec (maxIterationsReached)
import EVM.Dapp (DappInfo, dappInfo)
import EVM.Dapp (dappUnitTests, unitTestMethods, dappSolcByName, dappSolcByHash, dappSources)
import EVM.Dapp (dappAstSrcMap)
import EVM.Debug
import EVM.Format (showWordExact, showWordExplanation)
import EVM.Format (contractNamePart, contractPathPart, showTraceTree)
import EVM.Hexdump (prettyHex)
import EVM.Op
import EVM.Solidity
import EVM.Types hiding (padRight)
import EVM.UnitTest (UnitTestOptions (..))
import EVM.UnitTest (initialUnitTestVm, initializeUnitTest, runUnitTest)
import EVM.StorageLayout

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import EVM.Fetch (Fetcher)

import Control.Lens
import Control.Monad.State.Strict hiding (state)

import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Map (Map, insert, lookupLT, singleton, filter)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (sort, lookup)
import Data.Version (showVersion)
import Data.SBV hiding (solver)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec
import qualified Graphics.Vty as V
import qualified System.Console.Haskeline as Readline

import qualified EVM.TTYCenteredList as Centered

import qualified Paths_hevm as Paths

data Name
  = AbiPane
  | StackPane
  | BytecodePane
  | TracePane
  | SolidityPane
  | TestPickerPane
  | BrowserPane
  | Pager
  deriving (Eq, Show, Ord)

type UiWidget = Widget Name

data UiVmState = UiVmState
  { _uiVm           :: VM
  , _uiStep         :: Int
  , _uiSnapshots    :: Map Int (VM, Stepper ())
  , _uiStepper      :: Stepper ()
  , _uiStackList    :: List Name (Int, (SymWord))
  , _uiBytecodeList :: List Name (Int, Op)
  , _uiTraceList    :: List Name Text
  , _uiSolidityList :: List Name (Int, ByteString)
  , _uiMessage      :: Maybe String
  , _uiShowMemory   :: Bool
  , _uiSolc         :: Maybe SolcContract
  , _uiTestOpts     :: UnitTestOptions
  }

data UiTestPickerState = UiTestPickerState
  { _testPickerList :: List Name (Text, Text)
  , _testPickerDapp :: DappInfo
  , _testOpts       :: UnitTestOptions
  }

data UiBrowserState = UiBrowserState
  { _browserContractList :: List Name (Addr, Contract)
  , _browserVm :: UiVmState
  }

data UiState
  = ViewVm UiVmState
  | ViewContracts UiBrowserState
  | ViewPicker UiTestPickerState
  | ViewHelp UiVmState

makeLenses ''UiVmState
makeLenses ''UiTestPickerState
makeLenses ''UiBrowserState
makePrisms ''UiState

-- caching VM states lets us backstep efficiently
snapshotInterval :: Int
snapshotInterval = 50

type Pred a = a -> Bool

data StepMode
  = Step !Int                  -- ^ Run a specific number of steps
  | StepUntil (Pred VM)        -- ^ Finish when a VM predicate holds

-- | Each step command in the terminal should finish immediately
-- with one of these outcomes.
data Continuation a
     = Stopped a              -- ^ Program finished
     | Continue (Stepper a)   -- ^ Took one step; more steps to go


-- | This turns a @Stepper@ into a state action usable
-- from within the TTY loop, yielding a @StepOutcome@ depending on the @StepMode@.
interpret
  :: (?fetcher :: Fetcher
  ,   ?maxIter :: Maybe Integer)
  => StepMode
  -> Stepper a
  -> StateT UiVmState IO (Continuation a)
interpret mode =

  -- Like the similar interpreters in @EVM.UnitTest@ and @EVM.VMTest@,
  -- this one is implemented as an "operational monad interpreter".

  eval . Operational.view
  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> StateT UiVmState IO (Continuation a)

    eval (Operational.Return x) =
      pure (Stopped x)

    eval (action Operational.:>>= k) =
      case action of

        -- Stepper wants to keep executing?
        Stepper.Exec -> do

          -- Have we reached the final result of this action?
          use (uiVm . result) >>= \case
            Just r ->
              -- Yes, proceed with the next action.
              interpret mode (k r)
            Nothing -> do
              -- No, keep performing the current action
              let restart = Stepper.exec >>= k

              case mode of
                Step 0 -> do
                  -- We come here when we've continued while stepping,
                  -- either from a query or from a return;
                  -- we should pause here and wait for the user.
                  pure (Continue restart)

                Step i -> do
                  -- Run one instruction and recurse
                  stepOneOpcode restart
                  interpret (Step (i - 1)) restart

                StepUntil p -> do
                  vm <- use uiVm
                  case p vm of
                    True ->
                      interpret (Step 0) restart
                    False -> do
                      -- Run one instruction and recurse
                      stepOneOpcode restart
                      interpret (StepUntil p) restart

        -- Stepper is waiting for user input from a query
        Stepper.Ask (EVM.PleaseChoosePath cont) -> do
          -- ensure we aren't stepping past max iterations
          vm <- use uiVm
          case maxIterationsReached vm ?maxIter of
            Nothing -> pure $ Continue (k ())
            Just n -> interpret mode (Stepper.evm (cont (not n)) >>= k)

        -- Stepper wants to make a query and wait for the results?
        Stepper.Wait q -> do
          do m <- liftIO (?fetcher q)
             interpret mode (Stepper.evm m >>= k)

        -- Stepper wants to modify the VM.
        Stepper.EVM m -> do
          vm <- use uiVm
          let (r, vm1) = runState m vm
          assign uiVm vm1
          interpret mode (Stepper.exec >> (k r))

isUnitTestContract :: Text -> DappInfo -> Bool
isUnitTestContract name dapp =
  elem name (map fst (view dappUnitTests dapp))

mkVty :: IO V.Vty
mkVty = do
  vty <- V.mkVty V.defaultConfig
  V.setMode (V.outputIface vty) V.BracketedPaste True
  return vty

runFromVM :: Maybe Integer -> DappInfo -> (Query -> IO (EVM ())) -> VM -> IO VM
runFromVM maxIter' dappinfo oracle' vm = do

  let
    opts = UnitTestOptions
      { oracle            = oracle'
      , verbose           = Nothing
      , maxIter           = maxIter'
      , match             = ""
      , fuzzRuns          = 1
      , replay            = error "irrelevant"
      , vmModifier        = id
      , testParams        = error "irrelevant"
      , dapp              = dappinfo
      }
    ui0 = initUiVmState vm opts (void Stepper.execFully)

  v <- mkVty
  ui2 <- customMain v mkVty Nothing (app opts) (ViewVm ui0)
  case ui2 of
    ViewVm ui -> return (view uiVm ui)
    _ -> error "internal error: customMain returned prematurely"


initUiVmState :: VM -> UnitTestOptions -> Stepper () -> UiVmState
initUiVmState vm0 opts script =
  renderVm $
  UiVmState
    { _uiVm           = vm0
    , _uiStepper      = script
    , _uiStackList    = undefined
    , _uiBytecodeList = undefined
    , _uiTraceList    = undefined
    , _uiSolidityList = undefined
    , _uiSolc         = currentSolc (dapp opts) vm0
    , _uiStep         = 0
    , _uiSnapshots    = singleton 0 (vm0, script)
    , _uiMessage      = Just "Creating unit test contract"
    , _uiShowMemory   = False
    , _uiTestOpts     = opts
    }


-- filters out fuzztests, unless they have
-- explicitly been given an argument by `replay`
concreteTests :: UnitTestOptions -> (Text, [(Text, [AbiType])]) -> [(Text, Text)]
concreteTests UnitTestOptions{..} (contractname, tests) = case replay of
  Nothing -> [(contractname, fst x) | x <- tests,
                                      null $ snd x]
  Just (sig, _) -> [(contractname, fst x) | x <- tests,
                                            null (snd x) || fst x == sig]

main :: UnitTestOptions -> FilePath -> FilePath -> IO ()
main opts root jsonFilePath =
  readSolc jsonFilePath >>=
    \case
      Nothing ->
        error "Failed to read Solidity JSON"
      Just (contractMap, sourceCache) -> do
        let
          dapp = dappInfo root contractMap sourceCache
          ui = ViewPicker $ UiTestPickerState
            { _testPickerList =
                list
                  TestPickerPane
                  (Vec.fromList
                   (concatMap
                    (concreteTests opts)
                    (view dappUnitTests dapp)))
                  1
            , _testPickerDapp = dapp
            , _testOpts = opts
            }
        v <- mkVty
        _ <- customMain v mkVty Nothing (app opts) (ui :: UiState)
        return ()

takeStep
  :: (?fetcher :: Fetcher
     ,?maxIter :: Maybe Integer)
  => UiVmState
  -> StepMode
  -> EventM n (Next UiState)
takeStep ui mode =
  liftIO nxt >>= \case
    (Stopped (), ui') ->
      continue (ViewVm ui')
    (Continue steps, ui') -> do
      continue (ViewVm (ui' & set uiStepper steps))
  where
    m = interpret mode (view uiStepper ui)
    nxt = runStateT (m <* modify renderVm) ui

backstep
  :: (?fetcher :: Fetcher
     ,?maxIter :: Maybe Integer)
  => UiVmState -> EventM n UiVmState
backstep s = case view uiStep s of
  0 -> return s
  n ->
    let
      (step, (vm, stepper)) = fromJust $ lookupLT n (view uiSnapshots s)
      s1 = s
        & set uiVm vm
        & set (uiVm . cache) (view (uiVm . cache) s)
        & set uiStep step
        & set uiStepper stepper
      stepsToTake = n - step - 1

    in
      liftIO $ runStateT (interpret (Step stepsToTake) stepper) s1 >>= \case
        (Continue steps, ui') -> return $ ui' & set uiStepper steps
        _ -> error "unexpected end"

appEvent
  :: (?fetcher::Fetcher, ?maxIter :: Maybe Integer) =>
  UiState ->
  BrickEvent Name e ->
  EventM Name (Next UiState)

-- Contracts: Down - list down
appEvent (ViewContracts s) (VtyEvent e@(V.EvKey V.KDown [])) = do
  s' <- handleEventLensed s
    browserContractList
    handleListEvent
    e
  continue (ViewContracts s')

-- Contracts: Up - list up
appEvent (ViewContracts s) (VtyEvent e@(V.EvKey V.KUp [])) = do
  s' <- handleEventLensed s
    browserContractList
    handleListEvent
    e
  continue (ViewContracts s')

-- Vm Overview: Esc - return to test picker or exit
appEvent st@(ViewVm s) (VtyEvent (V.EvKey V.KEsc [])) =
  let opts = view uiTestOpts s
      dapp' = dapp (view uiTestOpts s)
      tests = concatMap
                (concreteTests opts)
                (view dappUnitTests dapp')
  in case tests of
    [] -> halt st
    ts ->
      continue . ViewPicker $
      UiTestPickerState
        { _testPickerList =
            list
              TestPickerPane
              (Vec.fromList
              ts)
              1
        , _testPickerDapp = dapp'
        , _testOpts = opts
        }

-- Vm Overview: Enter - open contracts view
appEvent (ViewVm s) (VtyEvent (V.EvKey V.KEnter [])) =
  continue . ViewContracts $ UiBrowserState
    { _browserContractList =
        list
          BrowserPane
          (Vec.fromList (Map.toList (view (uiVm . env . contracts) s)))
          2
    , _browserVm = s
    }

-- Vm Overview: m - toggle memory pane
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'm') [])) =
  continue (ViewVm (over uiShowMemory not s))

-- Vm Overview: h - open help view
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'h') []))
  = continue . ViewHelp $ s

-- Vm Overview: spacebar - read input
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  let
    loop = do
      Readline.getInputLine "% " >>= \case
        Just hey -> Readline.outputStrLn hey
        Nothing  -> pure ()
      Readline.getInputLine "% " >>= \case
        Just hey' -> Readline.outputStrLn hey'
        Nothing   -> pure ()
      return (ViewVm s)
  in
    suspendAndResume $
      Readline.runInputT Readline.defaultSettings loop

-- Vm Overview: n - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'n') [])) =
  case view (uiVm . result) s of
    Just _ -> continue (ViewVm s)
    _ -> takeStep s (Step 1)

-- Vm Overview: N - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'N') [])) =
  takeStep s
    (StepUntil (isNextSourcePosition s))

-- Vm Overview: C-n - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) =
  takeStep s
    (StepUntil (isNextSourcePositionWithoutEntering s))

-- Vm Overview: e - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'e') [])) =
  takeStep s
    (StepUntil (isExecutionHalted s))

-- Vm Overview: a - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'a') [])) =
      -- We keep the current cache so we don't have to redo
      -- any blocking queries.
      let
        (vm, stepper) = fromJust (Map.lookup 0 (view uiSnapshots s))
        s' = s
          & set uiVm vm
          & set (uiVm . cache) (view (uiVm . cache) s)
          & set uiStep 0
          & set uiStepper stepper

      in takeStep s' (Step 0)

-- Vm Overview: p - backstep
appEvent st@(ViewVm s) (VtyEvent (V.EvKey (V.KChar 'p') [])) =
  case view uiStep s of
    0 ->
      -- We're already at the first step; ignore command.
      continue st
    n -> do
      -- To step backwards, we revert to the previous snapshot
      -- and execute n - 1 `mod` snapshotInterval steps from there.
      --
      -- We keep the current cache so we don't have to redo
      -- any blocking queries, and also the memory view.
      let
        (step, (vm, stepper)) = fromJust $ lookupLT n (view uiSnapshots s)
        s1 = s
          & set uiVm vm
          & set (uiVm . cache) (view (uiVm . cache) s)
          & set uiStep step
          & set uiStepper stepper
        stepsToTake = n - step - 1

      takeStep s1 (Step stepsToTake)

-- Vm Overview: P - backstep
appEvent st@(ViewVm s) (VtyEvent (V.EvKey (V.KChar 'P') [])) =
  case view uiStep s of
    0 ->
      -- We're already at the first step; ignore command.
      continue st
    n -> do
      s1 <- backstep s
      let
        -- find a vm with a different source location than s1
        snapshots' = Data.Map.filter (isNextSourcePosition s1 . fst) (view uiSnapshots s1)
      case lookupLT n snapshots' of
          -- s2 source position is the first one. Go to the beginning.
          Nothing ->
            let
              (step', (vm', stepper')) = fromJust $ lookupLT (n - 1) (view uiSnapshots s)
              s2 = s1
                & set uiVm vm'
                & set (uiVm . cache) (view (uiVm . cache) s1)
                & set uiStep step'
                & set uiStepper stepper'
            in takeStep s2 (Step 0)
          -- step until we reach the source location of s1
          Just (step', (vm', stepper')) ->
            let
              s2 = s1
                & set uiVm vm'
                & set (uiVm . cache) (view (uiVm . cache) s1)
                & set uiStep step'
                & set uiStepper stepper'
            in takeStep s2 (StepUntil (not . isNextSourcePosition s1))

-- Vm Overview: c-p - backstep
appEvent st@(ViewVm s) (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) =
  case view uiStep s of
    0 ->
      -- We're already at the first step; ignore command.
      continue st
    n -> do
      s1 <- backstep s
      let
        -- find a vm with a different source location than s1
        snapshots' = Data.Map.filter (isNextSourcePositionWithoutEntering s1 . fst) (view uiSnapshots s1)
      case lookupLT n snapshots' of
          -- s2 source position is the first one. Go to the beginning.
          Nothing ->
            let
              (step', (vm', stepper')) = fromJust $ lookupLT (n - 1) (view uiSnapshots s)
              s2 = s1
                & set uiVm vm'
                & set (uiVm . cache) (view (uiVm . cache) s1)
                & set uiStep step'
                & set uiStepper stepper'
            in takeStep s2 (Step 0)
          -- step until we reach the source location of s1
          Just (step', (vm', stepper')) ->
            let
              s2 = s1
                & set uiVm vm'
                & set (uiVm . cache) (view (uiVm . cache) s1)
                & set uiStep step'
                & set uiStepper stepper'
            in takeStep s2 (StepUntil (not . isNextSourcePosition s1))

-- Vm Overview: 0 - choose no jump
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar '0') [])) =
  case view (uiVm . result) s of
    Just (VMFailure (Choose (PleaseChoosePath contin))) ->
      takeStep (s & set uiStepper (Stepper.evm (contin True) >> (view uiStepper s)))
        (Step 1)
    _ -> continue (ViewVm s)

-- Vm Overview: 1 - choose jump
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar '1') [])) =
  case view (uiVm . result) s of
    Just (VMFailure (Choose (PleaseChoosePath contin))) ->
      takeStep (s & set uiStepper (Stepper.evm (contin False) >> (view uiStepper s)))
        (Step 1)
    _ -> continue (ViewVm s)


-- Any: Esc - return to Vm Overview or Exit
appEvent s (VtyEvent (V.EvKey V.KEsc [])) =
  case s of
    (ViewHelp x) -> overview x
    (ViewContracts x) -> overview $ view browserVm x
    _ -> halt s
  where
    overview = continue . ViewVm

-- UnitTest Picker: Enter - select from list
appEvent (ViewPicker s) (VtyEvent (V.EvKey V.KEnter [])) =
  case listSelectedElement (view testPickerList s) of
    Nothing -> error "nothing selected"
    Just (_, x) ->
      continue . ViewVm $
        initialUiVmStateForTest (view testOpts s) x
--          (view testPickerDapp s) x

-- UnitTest Picker: (main) - render list
appEvent (ViewPicker s) (VtyEvent e) = do
  s' <- handleEventLensed s
    testPickerList
    handleListEvent
    e
  continue (ViewPicker s')

-- Page: Down - scroll
appEvent s (VtyEvent (V.EvKey V.KDown [])) =
  vScrollBy (viewportScroll TracePane) 1 >> continue s

-- Page: Up - scroll
appEvent s (VtyEvent (V.EvKey V.KUp [])) =
  vScrollBy (viewportScroll TracePane) (-1) >> continue s

-- Page: C-f - Page down
appEvent s (VtyEvent (V.EvKey (V.KChar 'f') [V.MCtrl])) =
  vScrollPage (viewportScroll TracePane) Down >> continue s

-- Page: C-b - Page up
appEvent s (VtyEvent (V.EvKey (V.KChar 'b') [V.MCtrl])) =
  vScrollPage (viewportScroll TracePane) Up >> continue s

-- Default
appEvent s _ = continue s

app :: UnitTestOptions -> App UiState () Name
app opts =
  let ?fetcher = oracle opts
      ?maxIter = maxIter opts
  in App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap V.defAttr myTheme)
  }

initialUiVmStateForTest
  :: UnitTestOptions
  -> (Text, Text)
  -> UiVmState
initialUiVmStateForTest opts@UnitTestOptions{..} (theContractName, theTestName) =
  ui
  where
    Just typesig = lookup theTestName (unitTestMethods testContract)
    args = case replay of
      Nothing -> emptyAbi
      Just (sig, callData) ->
        if theTestName == sig
        then decodeAbiValue (AbiTupleType (Vec.fromList typesig)) callData
        else emptyAbi
    script = do
      Stepper.evm . pushTrace . EntryTrace $
        "test " <> theTestName <> " (" <> theContractName <> ")"
      initializeUnitTest opts
      void (runUnitTest opts theTestName args)
    ui = initUiVmState vm0 opts script
    Just testContract =
      view (dappSolcByName . at theContractName) dapp
    vm0 =
      initialUnitTestVm opts testContract

myTheme :: [(AttrName, V.Attr)]
myTheme =
  [ (selectedAttr, V.defAttr `V.withStyle` V.standout)
  , (dimAttr, V.defAttr `V.withStyle` V.dim)
  , (borderAttr, V.defAttr `V.withStyle` V.dim)
  , (wordAttr, fg V.yellow)
  , (boldAttr, V.defAttr `V.withStyle` V.bold)
  , (activeAttr, V.defAttr `V.withStyle` V.standout)
  ]

drawUi :: UiState -> [UiWidget]
drawUi (ViewVm s) = drawVm s
drawUi (ViewPicker s) = drawTestPicker s
drawUi (ViewContracts s) = drawVmBrowser s
drawUi (ViewHelp _) = drawHelpView

drawHelpView :: [UiWidget]
drawHelpView =
    [ center . borderWithLabel version .
      padLeftRight 4 . padTopBottom 2 .  str $
        "Esc    Exit the debugger\n\n" <>
        "a      Step to start\n" <>
        "e      Step to end\n" <>
        "n      Step fwds by one instruction\n" <>
        "N      Step fwds to the next source position\n" <>
        "C-n    Step fwds to the next source position skipping CALL & CREATE\n" <>
        "p      Step back by one instruction\n\n" <>
        "P      Step back to the previous source position\n\n" <>
        "C-p    Step back to the previous source position skipping CALL & CREATE\n\n" <>
        "m      Toggle memory pane\n" <>
        "0      Choose the branch which does not jump \n" <>
        "1      Choose the branch which does jump \n" <>
        "Down   Scroll memory pane fwds\n" <>
        "Up     Scroll memory pane back\n" <>
        "C-f    Page memory pane fwds\n" <>
        "C-b    Page memory pane back\n\n" <>
        "Enter  Contracts browser"
    ]
    where
      version =
        txt "Hevm " <+>
        str (showVersion Paths.version) <+>
        txt " - Key bindings"

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
              (\selected (k, c') ->
                 withHighlight selected . txt . mconcat $
                   [ fromMaybe "<unknown contract>" . flip preview dapp' $
                       ( dappSolcByHash . ix (view codehash c')
                       . _2 . contractName )
                   , "\n"
                   , "  ", pack (show k)
                   ])
              True
              (view browserContractList ui)
      , case flip preview dapp' (dappSolcByHash . ix (view codehash c) . _2) of
          Nothing ->
            hBox
              [ borderWithLabel (txt "Contract information") . padBottom Max . padRight Max $ vBox
                  [ txt ("Codehash: " <>    pack (show (view codehash c)))
                  , txt ("Nonce: "    <> showWordExact (view nonce    c))
                  , txt ("Balance: "  <> showWordExact (view balance  c))
                  , txt ("Storage: "  <> storageDisplay (view storage c))
                  ]
                ]
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
                  , txt ("Storage:" <> storageDisplay (view storage c))
                  ]
              , borderWithLabel (txt "Storage slots") . padBottom Max . padRight Max $ vBox
                  (map txt (storageLayout dapp' solc))
              ]
      ]
  ]
  where storageDisplay (Concrete s) = pack ( show ( Map.toList s))
        storageDisplay (Symbolic a) = pack ("<symbolic> " ++ show a)
        dapp' = dapp (view (browserVm . uiTestOpts) ui)
        Just (_, (_, c)) = listSelectedElement (view browserContractList ui)
--        currentContract  = view (dappSolcByHash . ix ) dapp

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
        , vLimit 2 drawHelpBar
        ]
      )
      ( vBox
        [ hBox
          [ vLimit 20 $ drawBytecodePane ui
          , vLimit 20 $ drawStackPane ui
          ]
        , hBox
          [ drawSolidityPane ui
          , drawTracePane ui
          ]
        , vLimit 2 drawHelpBar
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
      , ("a", "step to start")
      , ("e", "step to end")
      , ("m", "toggle memory")
      , ("Esc", "exit")
      , ("h", "more help")
      ]

stepOneOpcode :: Stepper a -> StateT UiVmState IO ()
stepOneOpcode restart = do
  n <- use uiStep
  when (n > 0 && n `mod` snapshotInterval == 0) $ do
    vm <- use uiVm
    modifying uiSnapshots (insert n (vm, void restart))
  modifying uiVm (execState exec1)
  modifying uiStep (+ 1)


isNextSourcePosition
  :: UiVmState -> Pred VM
isNextSourcePosition ui vm =
  let dapp' = dapp (view uiTestOpts ui)
      initialPosition = currentSrcMap dapp' (view uiVm ui)
  in currentSrcMap dapp' vm /= initialPosition

isNextSourcePositionWithoutEntering
  :: UiVmState -> Pred VM
isNextSourcePositionWithoutEntering ui vm =
  let
    dapp'           = dapp (view uiTestOpts ui)
    vm0             = view uiVm ui
    initialPosition = currentSrcMap dapp' vm0
    initialHeight   = length (view frames vm0)
  in
    case currentSrcMap dapp' vm of
      Nothing ->
        False
      Just here ->
        let
          moved = Just here /= initialPosition
          deeper = length (view frames vm) > initialHeight
          boring =
            case srcMapCode (view dappSources dapp') here of
              Just bs ->
                BS.isPrefixOf "contract " bs
              Nothing ->
                True
        in
           moved && not deeper && not boring

isExecutionHalted :: UiVmState -> Pred VM
isExecutionHalted _ vm = isJust (view result vm)

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
    move = maybe id listMoveTo (vmOpIx vm)
    address = view (state . contract) vm
    message =
      case view result vm of
        Just (VMSuccess (ConcreteBuffer msg)) ->
          Just ("VMSuccess: " <> (show $ ByteStringS msg))
        Just (VMSuccess (SymbolicBuffer msg)) ->
          Just ("VMSuccess: <symbolicbuffer> " <> (show msg))
        Just (VMFailure (Revert msg)) ->
          Just ("VMFailure: " <> (show . ByteStringS $ msg))
        Just (VMFailure err) ->
          Just ("VMFailure: " <> show err)
        Nothing ->
          Just ("Executing EVM code in " <> show address)
    in ui
      & set uiVm vm
      & set uiStackList
          (list StackPane (Vec.fromList $ zip [1..] (view (state . stack) vm)) 2)
      & set uiBytecodeList
          (move $ list BytecodePane
             (view codeOps (fromJust (currentContract vm)))
             1)
      & set uiMessage message
      & set uiTraceList
          (list
            TracePane
            (Vec.fromList
              . Text.lines
              . showTraceTree dapp'
              $ vm)
            1)
      & set uiSolidityList
          (list SolidityPane
              (case currentSrcMap dapp' vm of
                Nothing -> mempty
                Just x ->
                  view (dappSources
                        . sourceLines
                        . ix (srcMapFile x)
                        . to (Vec.imap (,)))
                    dapp')
              1)
      where
        dapp' = dapp (view uiTestOpts ui)

drawStackPane :: UiVmState -> UiWidget
drawStackPane ui =
  let
    gasText = showWordExact (view (uiVm . state . gas) ui)
    labelText = txt ("Gas available: " <> gasText <> "; stack:")
  in hBorderWithLabel labelText <=>
    renderList
      (\_ (i, x@(S a w)) ->
         vBox
           [ withHighlight True (str ("#" ++ show i ++ " "))
               <+> str (show w)
           , dim (txt ("   " <> (case unliteral w of
                       Nothing -> pack $ show a
                       Just u -> showWordExplanation (fromSizzle u) $ dapp (view uiTestOpts ui)) <> pack (" " ++ (show a))))
           ])
      False
      (view uiStackList ui)

drawBytecodePane :: UiVmState -> UiWidget
drawBytecodePane ui =
  hBorderWithLabel (case view uiMessage ui of { Nothing -> str ""; Just s -> str s }) <=>
    Centered.renderList
      (\active x -> if not active
                    then withDefAttr dimAttr (opWidget x)
                    else withDefAttr boldAttr (opWidget x))
      False
      (view uiBytecodeList ui)

dim :: Widget n -> Widget n
dim = withDefAttr dimAttr

withHighlight :: Bool -> Widget n -> Widget n
withHighlight False = withDefAttr dimAttr
withHighlight True  = withDefAttr boldAttr

prettyIfConcrete :: Buffer -> String
prettyIfConcrete (SymbolicBuffer x) = show x
prettyIfConcrete (ConcreteBuffer x) = prettyHex 40 x

drawTracePane :: UiVmState -> UiWidget
drawTracePane s =
  case view uiShowMemory s of
    True ->
      hBorderWithLabel (txt "Calldata")
      <=> str (prettyIfConcrete $ fst (view (uiVm . state . calldata) s))
      <=> hBorderWithLabel (txt "Returndata")
      <=> str (prettyIfConcrete (view (uiVm . state . returndata) s))
      <=> hBorderWithLabel (txt "Output")
      <=> str (maybe "" show (view (uiVm . result) s))
      <=> hBorderWithLabel (txt "Cache")
      <=> str (show (view (uiVm . cache . path) s))
      <=> hBorderWithLabel (txt "Path Conditions")
      <=> (str $ show $ map snd $ view (uiVm . pathConditions) s)
      <=> hBorderWithLabel (txt "Memory")
      <=> viewport TracePane Vertical
            (str (prettyIfConcrete (view (uiVm . state . memory) s)))
    False ->
      hBorderWithLabel (txt "Trace")
      <=> renderList
            (\_ x -> txt x)
            False
            (view uiTraceList s)

drawSolidityPane :: UiVmState -> UiWidget
drawSolidityPane ui =
  let dapp' = dapp (view uiTestOpts ui)
  in case currentSrcMap dapp' (view uiVm ui) of
    Nothing -> padBottom Max (hBorderWithLabel (txt "<no source map>"))
    Just sm ->
      case view (dappSources . sourceLines . at (srcMapFile sm)) dapp' of
        Nothing -> padBottom Max (hBorderWithLabel (txt "<source not found>"))
        Just rows ->
          let
            subrange = lineSubrange rows (srcMapOffset sm, srcMapLength sm)
            lineNo =
              (snd . fromJust $
                (srcMapCodePos
                 (view dappSources dapp')
                 sm)) - 1
          in vBox
            [ hBorderWithLabel $
                txt (maybe "<unknown>" contractPathPart
                      (preview (uiSolc . _Just . contractName) ui))
                  <+> str (":" ++ show lineNo)

                  -- Show the AST node type if present
                  <+> txt (" (" <> fromMaybe "?"
                                    ((view dappAstSrcMap dapp') sm
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
                  (view uiSolidityList ui))
            ]

ifTallEnough :: Int -> Widget n -> Widget n -> Widget n
ifTallEnough need w1 w2 =
  Widget Greedy Greedy $ do
    c <- getContext
    if view availHeightL c > need
      then render w1
      else render w2

opWidget :: (Integral a, Show a) => (a, Op) -> Widget n
opWidget = txt . pack . opString

selectedAttr :: AttrName; selectedAttr = "selected"
dimAttr :: AttrName; dimAttr = "dim"
wordAttr :: AttrName; wordAttr = "word"
boldAttr :: AttrName; boldAttr = "bold"
activeAttr :: AttrName; activeAttr = "active"
