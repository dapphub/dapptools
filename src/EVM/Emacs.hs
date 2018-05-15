{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}
{-# Language FlexibleInstances #-}

module EVM.Emacs where

import EVM.TTY (currentSrcMap)
import qualified EVM.Fetch as Fetch
import System.IO
import Control.Monad.IO.Class
import System.Directory
import Data.Text (Text, pack, unpack)
import EVM
import qualified Data.Map as Map
import Data.Monoid
import EVM.UnitTest hiding (interpret)
import EVM.Dapp
import EVM.Fetch (Fetcher)
import EVM.Solidity
import EVM.Stepper (Stepper)
import qualified Control.Monad.Operational as Operational
import qualified EVM.Stepper as Stepper
import Control.Monad.State.Strict hiding (state)
import Data.SCargot
import Data.SCargot.Repr
import Data.SCargot.Repr.Basic
import Data.SCargot.Language.HaskLike
import Control.Lens

data UiVmState = UiVmState
  { _uiVm             :: VM
  , _uiVmNextStep     :: Stepper ()
  , _uiVmSolc         :: Maybe SolcContract
  , _uiVmDapp         :: Maybe DappInfo
  , _uiVmStepCount    :: Int
  , _uiVmFirstState   :: UiVmState
  , _uiVmFetcher      :: Fetcher
  }

makeLenses ''UiVmState

type Pred a = a -> Bool

data StepMode
  = StepOne                        -- ^ Finish after one opcode step
  | StepMany !Int                  -- ^ Run a specific number of steps
  | StepNone                       -- ^ Finish before the next opcode
  | StepUntil (Pred VM)            -- ^ Finish when a VM predicate holds

data StepOutcome a
  = Returned a                -- ^ Program finished
  | Stepped  (Stepper a)      -- ^ Took one step; more steps to go
  | Blocked  (IO (Stepper a)) -- ^ Came across blocking request

interpret
  :: StepMode
  -> Stepper a
  -> State UiVmState (StepOutcome a)
interpret mode =
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
          fetcher <- use uiVmFetcher
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
          interpret mode (k r)

        -- Stepper wants to emit a message.
        Stepper.Note _ -> do
          interpret mode (k ())

        -- Stepper wants to exit because of a failure.
        Stepper.Fail e ->
          error ("VM error: " ++ show e)

stepOneOpcode :: UiVmState -> UiVmState
stepOneOpcode ui =
  let
    nextVm = execState exec1 (view uiVm ui)
  in
    ui & over uiVmStepCount (+ 1)
       & set uiVm nextVm

updateUiVmState :: UiVmState -> VM -> UiVmState
updateUiVmState ui vm =
  ui & set uiVm vm

type Sexp = WellFormedSExpr HaskLikeAtom

prompt :: Console (Maybe Sexp)
prompt = do
  line <- liftIO (putStr "> " >> hFlush stdout >> getLine)
  case decodeOne (asWellFormed haskLikeParser) (pack line) of
    Left e -> do
      output (L [A "error", A (txt e)])
      pure Nothing
    Right s ->
      pure (Just s)
          
class SDisplay a where
  sexp :: a -> SExpr Text

display :: SDisplay a => a -> Text
display = encodeOne (basicPrint id) . sexp

txt :: Show a => a -> Text
txt = pack . show

data UiState
  = UiStarted
  | UiDappLoaded DappInfo
  | UiVm UiVmState

type Console a = StateT UiState IO a

output :: SDisplay a => a -> Console ()
output = liftIO . putStrLn . unpack . display

main :: IO ()
main = do
  putStrLn ";; Welcome to Hevm's Emacs integration."
  _ <- execStateT loop UiStarted
  pure ()

loop :: Console ()
loop = 
  prompt >>=
    \case
      Nothing -> pure ()
      Just command -> do
        handle command
        loop

handle :: Sexp -> Console ()
handle (WFSList (WFSAtom (HSIdent cmd) : args)) =
  do s <- get
     handleCmd s (cmd, args)
handle _ =
  output (L [A ("unrecognized-command" :: Text)])

handleCmd :: UiState -> (Text, [Sexp]) -> Console ()
handleCmd UiStarted = \case
  ("load-dapp",
   [WFSAtom (HSString (unpack -> root)),
    WFSAtom (HSString (unpack -> jsonPath))]) ->
    do liftIO (setCurrentDirectory root)
       liftIO (readSolc jsonPath) >>=
         \case
           Nothing -> 
             output (L [A ("error" :: Text)])
           Just (contractMap, sourceCache) ->
             let
               dapp = dappInfo root contractMap sourceCache
             in do
               output dapp
               put (UiDappLoaded dapp)
               
  _ ->
    output (L [A ("unrecognized-command" :: Text)])

handleCmd (UiDappLoaded dapp) = \case
  ("run-test", [WFSAtom (HSString contractPath),
                WFSAtom (HSString testName)]) -> do
    opts <- defaultUnitTestOptions
    put (UiVm (initialStateForTest opts dapp (contractPath, testName)))
    output (L [A ("ok" :: Text)])
  _ ->
    output (L [A ("unrecognized-command" :: Text)])

handleCmd (UiVm s) = \case
  ("step-once", []) -> do
    takeStep s StepNormally StepOne
    let
      noMap =
        output $
          L [ A "step"
            , L [A ("pc" :: Text), A (txt (view (uiVm . state . pc) s))]]
    case view uiVmDapp s of
      Nothing -> noMap
      Just dapp -> do
        case currentSrcMap dapp (view uiVm s) of
          Nothing -> noMap
          Just sm ->
            case view (dappSources . sourceFiles . at (srcMapFile sm)) dapp of
              Nothing -> noMap
              Just (fileName, _) -> do
                output $
                  L [ A "step"
                    , L [A ("pc" :: Text), A (txt (view (uiVm . state . pc) s))]
                    , L [A ("file" :: Text), A (txt fileName)]
                    , L [ A ("srcmap" :: Text)
                        , A (txt (srcMapOffset sm))
                        , A (txt (srcMapLength sm))
                        , A (txt (srcMapJump sm))
                        ]
                    ]
  _ ->
    output (L [A ("unrecognized-command" :: Text)])

-- ^ Specifies whether to do I/O blocking or VM halting while stepping.
-- When we step backwards, we don't want to allow those things.
data StepPolicy
  = StepNormally    -- ^ Allow blocking and returning
  | StepTimidly     -- ^ Forbid blocking and returning

takeStep
  :: UiVmState
  -> StepPolicy
  -> StepMode
  -> Console ()
takeStep ui policy mode = do
  let m = interpret mode (view uiVmNextStep ui)

  case runState m ui of

    (Stepped stepper, ui') -> do
      put (UiVm (ui' & set uiVmNextStep stepper))

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
          put (UiVm ui')
        StepTimidly ->
          error "step halted unexpectedly"

  -- readSolc jsonPath >>=
  --   \case
  --     Nothing -> error "Failed to read Solidity JSON"
  --     Just (contractMap, sourceCache) -> do
  --       let
  --         dapp = dappInfo root contractMap sourceCache
  --       putStrLn (unpack (display dapp))

instance SDisplay DappInfo where
  sexp x =
    L [ A "dapp-info"
      , L [A "root", A (txt $ view dappRoot x)]
      , L (A "unit-tests" :
            [ L [A (txt a), L (map (A . txt) b)]
            | (a, b) <- view dappUnitTests x])
      ]

instance SDisplay (SExpr Text) where
  sexp = id

defaultUnitTestOptions :: MonadIO m => m UnitTestOptions
defaultUnitTestOptions = do
  params <- liftIO getParametersFromEnvironmentVariables
  pure UnitTestOptions
    { oracle            = Fetch.zero
    , verbose           = False
    , match             = ""
    , vmModifier        = id
    , testParams        = params
    }

initialStateForTest
  :: UnitTestOptions
  -> DappInfo
  -> (Text, Text)
  -> UiVmState
initialStateForTest opts@(UnitTestOptions {..}) dapp (contractPath, testName) =
  ui1
  where
    script = do
      Stepper.evm . pushTrace . EntryTrace $
        "test " <> testName <> " (" <> contractPath <> ")"
      initializeUnitTest opts
      void (runUnitTest opts testName)
    ui0 =
      UiVmState
        { _uiVm             = vm0
        , _uiVmNextStep     = script
        , _uiVmSolc         = Just testContract
        , _uiVmDapp         = Just dapp
        , _uiVmStepCount    = 0
        , _uiVmFirstState   = undefined
        , _uiVmFetcher      = oracle
        }
    Just testContract =
      view (dappSolcByName . at contractPath) dapp
    vm0 =
      initialUnitTestVm opts testContract (Map.elems (view dappSolcByName dapp))
    ui1 =
      updateUiVmState ui0 vm0 & set uiVmFirstState ui1

