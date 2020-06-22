{-# Language TemplateHaskell #-}
{-# Language ImplicitParams #-}
{-# Language DataKinds #-}
module EVM.TTY where

import Prelude hiding (Word)

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List

import EVM
import EVM.ABI (abiTypeSolidity, decodeAbiValue, AbiType(..), emptyAbi)
import EVM.Symbolic (SymWord(..), maybeLitBytes)
import EVM.Dapp (DappInfo, dappInfo)
import EVM.Dapp (dappUnitTests, unitTestMethods, dappSolcByName, dappSolcByHash, dappSources)
import EVM.Dapp (dappAstSrcMap)
import EVM.Debug
import EVM.Format (Signedness (..), showDec, showWordExact)
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
import Data.Monoid ((<>))
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.List (sort)
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
  { _uiVm             :: VM
  , _uiVmNextStep     :: Stepper ()
  , _uiVmStackList    :: List Name (Int, (SymWord))
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
  , _uiVmTestOpts     :: UnitTestOptions
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

            StepMany 0 ->
              -- Finish the continuation until the next instruction;
              -- then, pause & await user.
              interpret StepNone restart

            StepMany i ->
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
                      Returned _ ->
                        interpret StepNone restart

                      -- This means that if we hit a blocking query
                      -- or a return, we pause despite the predicate.
                      --
                      -- This could be fixed if we allowed query I/O
                      -- here, instead of only in the TTY event loop;
                      -- let's do it later.
                      r -> pure r

        -- Stepper is waiting for user input from a query
        Stepper.Option _ -> do
                  -- pause & await user.
                  pure (Stepped (Operational.singleton action >>= k))

        -- Stepper wants to make a query and wait for the results?
        Stepper.Wait q ->
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

mkVty :: IO V.Vty
mkVty = do
  vty <- V.mkVty V.defaultConfig
  V.setMode (V.outputIface vty) V.BracketedPaste True
  return vty

runFromVM :: Maybe (FilePath, FilePath) -> (Query -> IO (EVM ())) -> VM -> IO VM
runFromVM maybesrcinfo oracle' vm = do
  uiDappSolc <- case maybesrcinfo of
                   Nothing -> return Nothing
                   Just (root,json) -> readSolc json >>= \case
                     Nothing -> return Nothing
                     Just (contractMap, sourceCache) ->
                       let dapp = dappInfo root contractMap sourceCache
                       in return $ ((,) dapp) <$> (currentSolc dapp vm)
                           
                         

  let
    opts = UnitTestOptions
      { oracle            = oracle'
      , verbose           = Nothing
      , match             = ""
      , fuzzRuns          = 1
      , replay            = error "irrelevant"
      , vmModifier        = id
      , testParams        = error "irrelevant"
      }
    ui0 = UiVmState
           { _uiVm = vm
           , _uiVmNextStep = void Stepper.execFully
           , _uiVmStackList = undefined
           , _uiVmBytecodeList = undefined
           , _uiVmTraceList = undefined
           , _uiVmSolidityList = undefined
           , _uiVmSolc = snd <$> uiDappSolc
           , _uiVmDapp = fst <$> uiDappSolc
           , _uiVmStepCount = 0
           , _uiVmFirstState = undefined
           , _uiVmMessage = Just $ "Executing EVM code in " <> show (view (state . contract) vm)
           , _uiVmNotes = []
           , _uiVmShowMemory = False
           , _uiVmTestOpts = opts
           }
    ui1 = updateUiVmState ui0 vm & set uiVmFirstState ui1
  v <- mkVty
  ui2 <- customMain v mkVty Nothing (app opts) (ViewVm ui1)
  case ui2 of
    ViewVm ui -> return (view uiVm ui)
    _ -> error "internal error: customMain returned prematurely"


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
takeStep ui policy mode =
  case nxt of
    (Stepped stepper, ui') ->
      if vmResult (view (uiVm . result) ui)
        then continue (ViewVm ui)
      else
        continue (ViewVm (ui' & set uiVmNextStep stepper))

    (Blocked blocker, ui') ->
      case policy of
        StepNormally -> do
          stepper <- liftIO blocker
          takeStep
            (execState (assign uiVmNextStep stepper) ui')
            StepNormally StepNone

        StepTimidly ->
          error $ "step blocked unexpectedly" ++ show (view (uiVm . result) ui')

    (Returned (), ui') ->
      case policy of
        StepNormally ->
          continue (ViewVm ui')
        StepTimidly ->
          error $ "step halted unexpectedly" ++ show (view (uiVm . result) ui)
  where
    vmResult Nothing = False
    vmResult (Just (VMFailure (Query _))) = False
    vmResult (Just _) = True
    m = interpret mode (view uiVmNextStep ui)
    nxt = runState (m <* modify renderVm) ui

appEvent
  :: (?fetcher::Fetcher) =>
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
  let opts = view uiVmTestOpts s in
  case view uiVmDapp s of
    Just dapp ->
      continue . ViewPicker $
      UiTestPickerState
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
    Nothing ->
      halt st

-- Vm Overview: C - open contracts view
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
  continue (ViewVm (over uiVmShowMemory not s))

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
  takeStep s StepNormally StepOne


-- Vm Overview: N - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'N') [])) =
  takeStep s
    StepNormally
    (StepUntil (isNextSourcePosition s))

-- Vm Overview: C-n - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl])) =
  takeStep s
    StepNormally
    (StepUntil (isNextSourcePositionWithoutEntering s))

-- Vm Overview: e - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'e') [])) =
  takeStep s
    StepNormally
    (StepUntil (isExecutionHalted s))

-- Vm Overview: a - step
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar 'a') [])) =
      -- We keep the current cache so we don't have to redo
      -- any blocking queries, and also the memory view.
      let
        s0 = view uiVmFirstState s
        s1 = set (uiVm . cache)   (view (uiVm . cache) s) s0
        s2 = set uiVmShowMemory (view uiVmShowMemory s) s1
        s3 = set uiVmTestOpts   (view uiVmTestOpts s) s2

      in takeStep s3 StepTimidly StepNone

-- Vm Overview: p - step
appEvent st@(ViewVm s) (VtyEvent (V.EvKey (V.KChar 'p') [])) =
  case view uiVmStepCount s of
    0 ->
      -- We're already at the first step; ignore command.
      continue st
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
        s3 = set (uiVmTestOpts)   (view uiVmTestOpts s) s2

      -- Take n steps; "timidly," because all queries
      -- ought to be cached.
      takeStep s3 StepTimidly (StepMany (n - 1))

-- Vm Overview: 0 - choose no jump
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar '0') [])) =
  case view (uiVm . result) s of
    Just (VMFailure (Choose (PleaseChoosePath contin))) ->
      takeStep (s & set uiVm (execState (contin 0) (view uiVm s)))
        StepNormally
        StepOne
    _ -> continue (ViewVm s)

-- Vm Overview: 1 - choose jump
appEvent (ViewVm s) (VtyEvent (V.EvKey (V.KChar '1') [])) =
  case view (uiVm . result) s of
    Just (VMFailure (Choose (PleaseChoosePath contin))) ->
      takeStep (s & set uiVm (execState (contin 1) (view uiVm s)))
        StepNormally
        StepOne
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
        initialUiVmStateForTest (view testOpts s)
          (view testPickerDapp s) x

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
  in App
  { appDraw = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent = appEvent
  , appStartEvent = return
  , appAttrMap = const (attrMap V.defAttr myTheme)
  }

initialUiVmStateForTest
  :: UnitTestOptions
  -> DappInfo
  -> (Text, Text)
  -> UiVmState
initialUiVmStateForTest opts@UnitTestOptions{..} dapp (theContractName, theTestName) =
  ui1
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
        , _uiVmTestOpts     = opts
        }
    Just testContract =
      view (dappSolcByName . at theContractName) dapp
    vm0 =
      initialUnitTestVm opts testContract (Map.elems (view dappSolcByName dapp))
    ui1 =
      updateUiVmState ui0 vm0 & set uiVmFirstState ui1

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
          Nothing ->
            hBox
              [ borderWithLabel (txt "Contract information") . padBottom Max . padRight Max $ vBox
                  [ txt ("Codehash: " <>    pack (show (view codehash c)))
                  , txt ("Nonce: "    <> showWordExact (view nonce    c))
                  , txt ("Balance: "  <> showWordExact (view balance  c))
                  , txt ("Storage: "  <> storageDisplay (view storage c))
                  ]
                ]
             where storageDisplay (Concrete s) = pack ( show ( Map.toList s))
                   storageDisplay (Symbolic _) = pack "<symbolic>"
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
  case view uiVmDapp ui of
     Just dapp ->
       let initialPosition = currentSrcMap dapp (view uiVm ui)
       in currentSrcMap dapp vm /= initialPosition
     Nothing -> True

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
        Just (VMSuccess msg) ->
          Just ("VMSuccess: " <> (show msg))
        Just (VMFailure (Revert msg)) ->
          Just ("VMFailure: " <> (show . ByteStringS $ msg))
        Just (VMFailure err) ->
          Just ("VMFailure: " <> show err)
        Nothing ->
          Just ("Executing EVM code in " <> showAddrWith0x address)
    ui' = ui
      & set uiVm vm
      & set uiVmStackList
          (list StackPane (Vec.fromList $ zip [1..] (view (state . stack) vm)) 2)
      & set uiVmBytecodeList
          (move $ list BytecodePane
             (view codeOps (fromJust (currentContract vm)))
             1)
      & set uiVmMessage message
  in
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
      where
        dapp =
          fromMaybe
            (dappInfo "" mempty (SourceCache mempty mempty mempty mempty))
            (view uiVmDapp ui)

drawStackPane :: UiVmState -> UiWidget
drawStackPane ui =
  let
    gasText = showWordExact (view (uiVm . state . gas) ui)
    labelText = txt ("Gas available: " <> gasText <> "; stack:")
  in hBorderWithLabel labelText <=>
    renderList
      (\_ (i, x@(S _ w)) ->
         vBox
           [ withHighlight True (str ("#" ++ show i ++ " "))
               <+> str (show x)
           , dim (txt ("   " <> case unliteral w of
                       Nothing -> ""
                       Just u -> showWordExplanation (fromSizzle u) (view uiVmDapp ui)))
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

prettyIfConcrete :: [SWord 8] -> String
prettyIfConcrete x = case maybeLitBytes x of
  Just lits -> prettyHex 40 lits
  Nothing -> show x

drawTracePane :: UiVmState -> UiWidget
drawTracePane s =
  case view uiVmShowMemory s of
    True ->
      hBorderWithLabel (txt "Calldata")
      <=> str (prettyIfConcrete $ fst (view (uiVm . state . calldata) s))
      <=> hBorderWithLabel (txt "Returndata")
      <=> str (prettyIfConcrete (view (uiVm . state . returndata) s))
      <=> hBorderWithLabel (txt "Output")
      <=> str (maybe "" show (view (uiVm . result) s))
      <=> hBorderWithLabel (txt "Cache")
      <=> str (show (view (uiVm . cache . path) s))
      <=> hBorderWithLabel (txt "Memory")
      <=> viewport TracePane Vertical
            (str (prettyIfConcrete (view (uiVm . state . memory) s)))
    False ->
      hBorderWithLabel (txt "Trace")
      <=> renderList
            (\_ x -> txt x)
            False
            (view uiVmTraceList s)

drawSolidityPane :: UiVmState -> UiWidget
drawSolidityPane ui@(view uiVmDapp -> Just dapp) =
  case currentSrcMap dapp (view uiVm ui) of
    Nothing -> padBottom Max (hBorderWithLabel (txt "<no source map>"))
    Just sm ->
      case view (dappSources . sourceLines . at (srcMapFile sm)) dapp of
        Nothing -> padBottom Max (hBorderWithLabel (txt "<source not found>"))
        Just rows ->
          let
            subrange = lineSubrange rows (srcMapOffset sm, srcMapLength sm)
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

opWidget :: (Integral a, Show a) => (a, Op) -> Widget n
opWidget = txt . pack . opString

selectedAttr :: AttrName; selectedAttr = "selected"
dimAttr :: AttrName; dimAttr = "dim"
wordAttr :: AttrName; wordAttr = "word"
boldAttr :: AttrName; boldAttr = "bold"
activeAttr :: AttrName; activeAttr = "active"
