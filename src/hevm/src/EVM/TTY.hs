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
import EVM.SymExec (maxIterationsReached, symCalldata)
import EVM.Dapp (DappInfo, dappInfo, Test, extractSig, Test(..), srcMap)
import EVM.Dapp (dappUnitTests, unitTestMethods, dappSolcByName, dappSolcByHash, dappSources)
import EVM.Dapp (dappAstSrcMap)
import EVM.Debug
--import EVM.Format (showWordExact, showWordExplanation)
import EVM.Format (contractNamePart, contractPathPart, showTraceTree)
import EVM.Hexdump (prettyHex)
import EVM.Op
import EVM.Solidity hiding (storageLayout)
import EVM.Types hiding (padRight)
import EVM.UnitTest
import EVM.StorageLayout

import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational

import EVM.Fetch (Fetcher)

import Control.Lens hiding (List)
import Control.Monad.Trans.Reader
import Control.Monad.State.Strict hiding (state)

import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Map (Map, insert, lookupLT, singleton, filter)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (sort, find)
import Data.Version (showVersion)

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vec
import qualified Data.Vector.Storable as SVec
import qualified Graphics.Vty as V
import qualified System.Console.Haskeline as Readline

import qualified EVM.TTYCenteredList as Centered

import qualified Paths_hevm as Paths

  {-

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
  , _uiShowMemory   :: Bool
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

        Stepper.Run -> do
          -- Have we reached the final result of this action?
          use (uiVm . result) >>= \case
            Just _ -> do
              -- Yes, proceed with the next action.
              vm <- use uiVm
              interpret mode (k vm)
            Nothing -> do
              -- No, keep performing the current action
              keepExecuting mode (Stepper.run >>= k)

        -- Stepper wants to keep executing?
        Stepper.Exec -> do
          -- Have we reached the final result of this action?
          use (uiVm . result) >>= \case
            Just r ->
              -- Yes, proceed with the next action.
              interpret mode (k r)
            Nothing -> do
              -- No, keep performing the current action
              keepExecuting mode (Stepper.exec >>= k)

        -- Stepper is waiting for user input from a query
        Stepper.Ask (PleaseChoosePath _ cont) -> do
          -- ensure we aren't stepping past max iterations
          vm <- use uiVm
          case maxIterationsReached vm ?maxIter of
            Nothing -> pure $ Continue (k ())
            Just n -> interpret mode (Stepper.evm (cont (not n)) >>= k)

        -- Stepper wants to make a query and wait for the results?
        Stepper.Wait q -> do
          do m <- liftIO (?fetcher q)
             interpret mode (Stepper.evm m >>= k)

        -- Stepper wants to make a query and wait for the results?
        Stepper.IOAct q -> do
          zoom uiVm (StateT (runStateT q)) >>= interpret mode . k

        -- Stepper wants to modify the VM.
        Stepper.EVM m -> do
          vm <- use uiVm
          let (r, vm1) = runState m vm
          assign uiVm vm1
          interpret mode (Stepper.exec >> (k r))

keepExecuting :: (?fetcher :: Fetcher
              ,   ?maxIter :: Maybe Integer)
              => StepMode
              -> Stepper a
              -> StateT UiVmState IO (Continuation a)
keepExecuting mode restart = case mode of
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
    if p vm
      then
        interpret (Step 0) restart
      else do
        -- Run one instruction and recurse
        stepOneOpcode restart
        interpret (StepUntil p) restart

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
      { oracle        = oracle'
      , verbose       = Nothing
      , maxIter       = maxIter'
      , askSmtIters   = Nothing
      , smtTimeout    = Nothing
      , smtState      = Nothing
      , solver        = Nothing
      , maxDepth      = Nothing
      , match         = ""
      , fuzzRuns      = 1
      , replay        = error "irrelevant"
      , vmModifier    = id
      , testParams    = error "irrelevant"
      , dapp          = dappinfo
      , ffiAllowed    = False
      , covMatch       = Nothing
      }
    ui0 = initUiVmState vm opts (void Stepper.execFully)

  v <- mkVty
  ui2 <- customMain v mkVty Nothing (app opts) (ViewVm ui0)
  case ui2 of
    ViewVm ui -> return (view uiVm ui)
    _ -> error "internal error: customMain returned prematurely"


initUiVmState :: VM -> UnitTestOptions -> Stepper () -> UiVmState
initUiVmState vm0 opts script =
  UiVmState
    { _uiVm           = vm0
    , _uiStepper      = script
    , _uiStep         = 0
    , _uiSnapshots    = singleton 0 (vm0, script)
    , _uiShowMemory   = False
    , _uiTestOpts     = opts
    }


-- filters out fuzztests, unless they have
-- explicitly been given an argument by `replay`
debuggableTests :: UnitTestOptions -> (Text, [(Test, [AbiType])]) -> [(Text, Text)]
debuggableTests UnitTestOptions{..} (contractname, tests) = case replay of
  Nothing -> [(contractname, extractSig $ fst x) | x <- tests, not $ isFuzzTest x]
  Just (sig, _) -> [(contractname, extractSig $ fst x) | x <- tests, not (isFuzzTest x) || extractSig (fst x) == sig]

isFuzzTest :: (Test, [AbiType]) -> Bool
isFuzzTest (SymbolicTest _, _) = False
isFuzzTest (ConcreteTest _, []) = False
isFuzzTest (ConcreteTest _, _) = True
isFuzzTest (InvariantTest _, _) = True

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
                    (debuggableTests opts)
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
    nxt = runStateT m ui

backstepUntil
  :: (?fetcher :: Fetcher
     ,?maxIter :: Maybe Integer)
  => (UiVmState -> Pred VM) -> UiVmState -> EventM n (Next UiState)
backstepUntil p s =
  case view uiStep s of
    0 -> continue (ViewVm s)
    n -> do
      s1 <- backstep s
      let
        -- find a previous vm that satisfies the predicate
        snapshots' = Data.Map.filter (p s1 . fst) (view uiSnapshots s1)
      case lookupLT n snapshots' of
        -- If no such vm exists, go to the beginning
        Nothing ->
          let
            (step', (vm', stepper')) = fromJust $ lookupLT (n - 1) (view uiSnapshots s)
            s2 = s1
              & set uiVm vm'
              & set (uiVm . cache) (view (uiVm . cache) s1)
              & set uiStep step'
              & set uiStepper stepper'
          in takeStep s2 (Step 0)
        -- step until the predicate doesn't hold
        Just (step', (vm', stepper')) ->
          let
            s2 = s1
              & set uiVm vm'
              & set (uiVm . cache) (view (uiVm . cache) s1)
              & set uiStep step'
              & set uiStepper stepper'
          in takeStep s2 (StepUntil (not . p s1))

backstep
  :: (?fetcher :: Fetcher
     ,?maxIter :: Maybe Integer)
  => UiVmState -> EventM n UiVmState
backstep s = case view uiStep s of
  -- We're already at the first step; ignore command.
  0 -> return s
  -- To step backwards, we revert to the previous snapshot
  -- and execute n - 1 `mod` snapshotInterval steps from there.

  -- We keep the current cache so we don't have to redo
  -- any blocking queries, and also the memory view.
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
                (debuggableTests opts)
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

-- todo refactor to zipper step forward
-- Vm Overview: n - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'n') [])) =
  if isJust $ view (uiVm . result) s
  then continue (ViewVm s)
  else takeStep s (Step 1)

-- Vm Overview: N - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'N') [])) =
  if isJust $ view (uiVm . result) s
  then continue (ViewVm s)
  else takeStep s
       (StepUntil (isNextSourcePosition s))

-- Vm Overview: C-n - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) =
  if isJust $ view (uiVm . result) s
  then continue (ViewVm s)
  else takeStep s
    (StepUntil (isNextSourcePositionWithoutEntering s))

-- Vm Overview: e - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'e') [])) =
  if isJust $ view (uiVm . result) s
  then continue (ViewVm s)
  else takeStep s
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

      -- We keep the current cache so we don't have to redo
      -- any blocking queries, and also the memory view.
      let
        (step, (vm, stepper)) = fromJust $ lookupLT n (view uiSnapshots s)
        s1 = s
          & set uiVm vm -- set the vm to the one from the snapshot
          & set (uiVm . cache) (view (uiVm . cache) s) -- persist the cache
          & set uiStep step
          & set uiStepper stepper
        stepsToTake = n - step - 1

      takeStep s1 (Step stepsToTake)

-- Vm Overview: P - backstep to previous source
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'P') [])) =
  backstepUntil isNextSourcePosition s

-- Vm Overview: c-p - backstep to previous source avoiding CALL and CREATE
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'p') [V.MCtrl])) =
  backstepUntil isNextSourcePositionWithoutEntering s

-- Vm Overview: 0 - choose no jump
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar '0') [])) =
  case view (uiVm . result) s of
    Just (VMFailure (Choose (PleaseChoosePath _ contin))) ->
      takeStep (s & set uiStepper (Stepper.evm (contin True) >> (view uiStepper s)))
        (Step 1)
    _ -> continue (ViewVm s)

-- Vm Overview: 1 - choose jump
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar '1') [])) =
  case view (uiVm . result) s of
    Just (VMFailure (Choose (PleaseChoosePath _ contin))) ->
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
    Just (_, x) -> do
      initVm <- liftIO $ initialUiVmStateForTest (view testOpts s) x
      continue . ViewVm $ initVm

-- UnitTest Picker: (main) - render list
appEvent (ViewPicker s) (VtyEvent e) = do
  s' <- handleEventLensed s
    testPickerList
    handleListEvent
    e
  continue (ViewPicker s')

-- Page: Down - scroll
appEvent (ViewVm s) (VtyEvent (V.EvKey V.KDown [])) =
  if view uiShowMemory s then
    vScrollBy (viewportScroll TracePane) 1 >> continue (ViewVm s)
  else
    if isJust $ view (uiVm . result) s
    then continue (ViewVm s)
    else takeStep s
         (StepUntil (isNewTraceAdded s))

-- Page: Up - scroll
appEvent (ViewVm s) (VtyEvent (V.EvKey V.KUp [])) =
  if view uiShowMemory s then
    vScrollBy (viewportScroll TracePane) (-1) >> continue (ViewVm s)
  else
    backstepUntil isNewTraceAdded s

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
  -> IO UiVmState
initialUiVmStateForTest opts@UnitTestOptions{..} (theContractName, theTestName) = do
  let state' = fromMaybe (error "Internal Error: missing smtState") smtState
  (buf, len) <- case test of
    SymbolicTest _ -> flip runReaderT state' $ SBV.runQueryT $ symCalldata theTestName types []
    _ -> return (error "unreachable", error "unreachable")
  let script = do
        Stepper.evm . pushTrace . EntryTrace $
          "test " <> theTestName <> " (" <> theContractName <> ")"
        initializeUnitTest opts testContract
        case test of
          ConcreteTest _ -> do
            let args = case replay of
                         Nothing -> emptyAbi
                         Just (sig, callData) ->
                           if theTestName == sig
                           then decodeAbiValue (AbiTupleType (Vec.fromList types)) callData
                           else emptyAbi
            void (runUnitTest opts theTestName args)
          SymbolicTest _ -> do
            Stepper.evm $ modify symbolify
            void (execSymTest opts theTestName (SymbolicBuffer buf, w256lit len))
          InvariantTest _ -> do
            targets <- getTargetContracts opts
            let randomRun = initialExplorationStepper opts theTestName [] targets (fromMaybe 20 maxDepth)
            void $ case replay of
              Nothing -> randomRun
              Just (sig, cd) ->
                if theTestName == sig
                then initialExplorationStepper opts theTestName (decodeCalls cd) targets (length (decodeCalls cd))
                else randomRun
  pure $ initUiVmState vm0 opts script
  where
    Just (test, types) = find (\(test',_) -> extractSig test' == theTestName) $ unitTestMethods testContract
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
        "Down   Step to next entry in the callstack / Scroll memory pane\n" <>
        "Up     Step to previous entry in the callstack / Scroll memory pane\n" <>
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
          Just sol ->
            hBox
              [ borderWithLabel (txt "Contract information") . padBottom Max . padRight (Pad 2) $ vBox
                  [ txt "Name: " <+> txt (contractNamePart (view contractName sol))
                  , txt "File: " <+> txt (contractPathPart (view contractName sol))
                  , txt " "
                  , txt "Constructor inputs:"
                  , vBox . flip map (view constructorInputs sol) $
                      \(name, abiType) -> txt ("  " <> name <> ": " <> abiTypeSolidity abiType)
                  , txt "Public methods:"
                  , vBox . flip map (sort (Map.elems (view abiMap sol))) $
                      \method -> txt ("  " <> view methodSignature method)
                  , txt ("Storage:" <> storageDisplay (view storage c))
                  ]
              , borderWithLabel (txt "Storage slots") . padBottom Max . padRight Max $ vBox
                  (map txt (storageLayout dapp' sol))
              ]
      ]
  ]
  where storageDisplay (Concrete s) = pack ( show ( Map.toList s))
        storageDisplay (Symbolic v _) = pack $ show v
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

isNewTraceAdded
  :: UiVmState -> Pred VM
isNewTraceAdded ui vm =
  let
    currentTraceTree = length <$> traceForest (view uiVm ui)
    newTraceTree = length <$> traceForest vm
  in currentTraceTree /= newTraceTree

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
currentSrcMap dapp vm = do
  this <- currentContract vm
  i <- (view opIxMap this) SVec.!? (view (state . pc) vm)
  srcMap dapp this i

drawStackPane :: UiVmState -> UiWidget
drawStackPane ui =
  let
    gasText = showWordExact (view (uiVm . state . gas) ui)
    labelText = txt ("Gas available: " <> gasText <> "; stack:")
    stackList = list StackPane (Vec.fromList $ zip [(1 :: Int)..] (view (uiVm . state . stack) ui)) 2
  in hBorderWithLabel labelText <=>
    renderList
      (\_ (i, x@(S _ w)) ->
         vBox
           [ withHighlight True (str ("#" ++ show i ++ " "))
               <+> str (show x)
           , dim (txt ("   " <> case unliteral w of
                       Nothing -> ""
                       Just u -> showWordExplanation (fromSizzle u) $ dapp (view uiTestOpts ui)))
           ])
      False
      stackList

message :: VM -> String
message vm =
  case view result vm of
    Just (VMSuccess (ConcreteBuffer msg)) ->
      "VMSuccess: " <> (show $ ByteStringS msg)
    Just (VMSuccess (SymbolicBuffer msg)) ->
      "VMSuccess: <symbolicbuffer> " <> (show msg)
    Just (VMFailure (Revert msg)) ->
      "VMFailure: " <> (show . ByteStringS $ msg)
    Just (VMFailure err) ->
      "VMFailure: " <> show err
    Nothing ->
      "Executing EVM code in " <> show (view (state . contract) vm)


drawBytecodePane :: UiVmState -> UiWidget
drawBytecodePane ui =
  let
    vm = view uiVm ui
    move = maybe id listMoveTo $ vmOpIx vm
  in
    hBorderWithLabel (str $ message vm) <=>
    Centered.renderList
      (\active x -> if not active
                    then withDefAttr dimAttr (opWidget x)
                    else withDefAttr boldAttr (opWidget x))
      False
      (move $ list BytecodePane
        (maybe mempty (view codeOps) (currentContract vm))
        1)


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
  let vm = view uiVm s
      dapp' = dapp (view uiTestOpts s)
      traceList =
        list
          TracePane
          (Vec.fromList
            . Text.lines
            . showTraceTree dapp'
            $ vm)
          1

  in case view uiShowMemory s of
    True ->
      hBorderWithLabel (txt "Calldata")
      <=> str (prettyIfConcrete $ fst (view (state . calldata) vm))
      <=> hBorderWithLabel (txt "Returndata")
      <=> str (prettyIfConcrete (view (state . returndata) vm))
      <=> hBorderWithLabel (txt "Output")
      <=> str (maybe "" show (view result vm))
      <=> hBorderWithLabel (txt "Cache")
      <=> str (show (view (cache . path) vm))
      <=> hBorderWithLabel (txt "Path Conditions")
      <=> (str $ show $ snd <$> view constraints vm)
      <=> hBorderWithLabel (txt "Memory")
      <=> viewport TracePane Vertical
            (str (prettyIfConcrete (view (state . memory) vm)))
    False ->
      hBorderWithLabel (txt "Trace")
      <=> renderList
            (\_ x -> txt x)
            False
            (listMoveTo (length traceList) traceList)

solidityList :: VM -> DappInfo -> List Name (Int, ByteString)
solidityList vm dapp' =
  list SolidityPane
    (case currentSrcMap dapp' vm of
        Nothing -> mempty
        Just x ->
          view (dappSources
            . sourceLines
            . ix (srcMapFile x)
            . to (Vec.imap (,)))
          dapp')
    1

drawSolidityPane :: UiVmState -> UiWidget
drawSolidityPane ui =
  let dapp' = dapp (view uiTestOpts ui)
      dappSrcs = view dappSources dapp'
      vm = view uiVm ui
  in case currentSrcMap dapp' vm of
    Nothing -> padBottom Max (hBorderWithLabel (txt "<no source map>"))
    Just sm ->
          let
            rows = (_sourceLines dappSrcs) !! srcMapFile sm
            subrange = lineSubrange rows (srcMapOffset sm, srcMapLength sm)
            fileName :: Maybe Text
            fileName = preview (dappSources . sourceFiles . ix (srcMapFile sm) . _1) dapp'
            lineNo :: Maybe Int
            lineNo = maybe Nothing (\a -> Just (a - 1))
              (snd <$>
                (srcMapCodePos
                 (view dappSources dapp')
                 sm))
          in vBox
            [ hBorderWithLabel $
                txt (fromMaybe "<unknown>" fileName)
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
                ((maybe id listMoveTo lineNo)
                  (solidityList vm dapp'))
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
-}
