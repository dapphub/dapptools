{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}

module EVM.Emacs where

import Data.Text (Text, pack, unpack)
import EVM (VM, result, exec1)
import EVM.UnitTest (UnitTestOptions (..))
import EVM.Dapp
import EVM.Fetch (Fetcher)
import EVM.Solidity (SolcContract, readSolc)
import EVM.Stepper (Stepper)
import qualified Control.Monad.Operational as Operational
import qualified EVM.Stepper as Stepper
import Control.Monad.State.Strict hiding (state)
import Data.SCargot
import Data.SCargot.Repr.Basic
import Control.Lens

data UiVmState = UiVmState
  { _uiVm             :: VM
  , _uiVmNextStep     :: Stepper ()
  , _uiVmSolc         :: Maybe SolcContract
  , _uiVmDapp         :: Maybe DappInfo
  , _uiVmStepCount    :: Int
  , _uiVmFirstState   :: UiVmState
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
  :: (?fetcher :: Fetcher)
  => StepMode
  -> Stepper a
  -> StateT UiVmState IO (StepOutcome a)
interpret mode =
  eval . Operational.view
  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> StateT UiVmState IO (StepOutcome a)

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

main :: UnitTestOptions -> FilePath -> FilePath -> IO ()
main _opts root jsonPath =
  readSolc jsonPath >>=
    \case
      Nothing -> error "Failed to read Solidity JSON"
      Just (contractMap, sourceCache) -> do
        let
          dapp = dappInfo root contractMap sourceCache
        putStrLn (unpack (display dapp))

class SDisplay a where
  sexp :: a -> SExpr Text

display :: SDisplay a => a -> Text
display = encodeOne (basicPrint id) . sexp

txt :: Show a => a -> Text
txt = pack . show

instance SDisplay DappInfo where
  sexp x =
    L [ A "dapp-info"
      , L [A "root", A (txt $ view dappRoot x)]
      , L (A "unit-tests" :
            [ L [A (txt a), L (map (A . txt) b)]
            | (a, b) <- view dappUnitTests x])
      ]
