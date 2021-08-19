{-# Language ImplicitParams #-}
{-# Language TemplateHaskell #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}

module EVM.Emacs where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.State.Strict hiding (state)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.SCargot
import Data.SCargot.Language.HaskLike
import Data.SCargot.Repr
import Data.SCargot.Repr.Basic
import Data.Set (Set)
import Data.Text (Text, pack, unpack)
import Data.SBV hiding (Word, output)
import EVM
import EVM.ABI
import EVM.Symbolic
import EVM.Dapp
import EVM.Debug (srcMapCodePos)
import EVM.Fetch (Fetcher)
import EVM.Solidity
import EVM.Stepper (Stepper)
import EVM.TTY (currentSrcMap)
import EVM.Types
import EVM.UnitTest
import Prelude hiding (Word)
import System.Directory
import System.IO
import qualified Control.Monad.Operational as Operational
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified EVM.Fetch as Fetch
import qualified EVM.Stepper as Stepper

data UiVmState = UiVmState
  { _uiVm             :: VM
  , _uiVmNextStep     :: Stepper ()
  , _uiVmSolc         :: Maybe SolcContract
  , _uiVmDapp         :: Maybe DappInfo
  , _uiVmStepCount    :: Int
  , _uiVmFirstState   :: UiVmState
  , _uiVmFetcher      :: Fetcher
  , _uiVmMessage      :: Maybe Text
  , _uiVmSentHashes   :: Set W256
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
          modify updateSentHashes
          interpret mode (k r)

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

updateSentHashes :: UiVmState -> UiVmState
updateSentHashes ui =
  let sent = allHashes (view uiVm ui) in
    ui & set uiVmSentHashes sent

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

handleCmd (UiDappLoaded _) = \case
  ("run-test", [WFSAtom (HSString contractPath),
                WFSAtom (HSString testName)]) -> do
    opts <- defaultUnitTestOptions
    put (UiVm (initialStateForTest opts (contractPath, testName)))
    outputVm
  _ ->
    output (L [A ("unrecognized-command" :: Text)])

handleCmd (UiVm s) = \case
  ("step", [WFSAtom (HSString modeName)]) ->
    case parseStepMode s modeName of
      Just mode -> do
        takeStep s StepNormally mode
        outputVm
      Nothing ->
        output (L [A ("unrecognized-command" :: Text)])
  ("step", [WFSList [ WFSAtom (HSString "file-line")
                    , WFSAtom (HSString fileName)
                    , WFSAtom (HSInt (fromIntegral -> lineNumber))
                    ]]) ->
    case view uiVmDapp s of
      Nothing ->
        output (L [A ("impossible" :: Text)])
      Just dapp -> do
        takeStep s StepNormally
          (StepUntil (atFileLine dapp fileName lineNumber))
        outputVm
  _ ->
    output (L [A ("unrecognized-command" :: Text)])

atFileLine :: DappInfo -> Text -> Int -> VM -> Bool
atFileLine dapp wantedFileName wantedLineNumber vm =
  case currentSrcMap dapp vm of
    Nothing -> False
    Just sm ->
          let
            (currentFileName, currentLineNumber) =
              fromJust (srcMapCodePos (view dappSources dapp) sm)
          in
            currentFileName == wantedFileName &&
              currentLineNumber == wantedLineNumber

codeByHash :: W256 -> VM -> Maybe Buffer
codeByHash h vm = do
  let cs = view (env . contracts) vm
  c <- List.find (\c -> h == (view codehash c)) (Map.elems cs)
  return (view bytecode c)

allHashes :: VM -> Set W256
allHashes vm = let cs = view (env . contracts) vm
  in Set.fromList ((view codehash) <$> Map.elems cs)

outputVm :: Console ()
outputVm = do
  UiVm s <- get
  let vm = view uiVm s
      sendHashes = Set.difference (allHashes vm) (view uiVmSentHashes s)
      sendCodes = Map.fromSet (`codeByHash` vm) sendHashes
      noMap =
        output $
        L [ A "step"
          , L [A ("vm" :: Text), sexp (view uiVm s)]
          ]
  fromMaybe noMap $ do
    dapp <- view uiVmDapp s
    sm <- currentSrcMap dapp (view uiVm s)
    let (fileName, _) = view (dappSources . sourceFiles) dapp !! srcMapFile sm
    pure . output $
      L [ A "step"
        , L [A ("vm" :: Text), sexp (view uiVm s)]
        , L [A ("file" :: Text), A (txt fileName)]
        , L [ A ("srcmap" :: Text)
            , A (txt (srcMapOffset sm))
            , A (txt (srcMapLength sm))
            , A (txt (srcMapJump sm))
            ]
        ]


isNextSourcePosition
  :: UiVmState -> Pred VM
isNextSourcePosition ui vm =
  let
    Just dapp       = view uiVmDapp ui
    initialPosition = currentSrcMap dapp (view uiVm ui)
  in
    currentSrcMap dapp vm /= initialPosition

parseStepMode :: UiVmState -> Text -> Maybe StepMode
parseStepMode s =
  \case
    "once" -> Just StepOne
    "source-location" -> Just (StepUntil (isNextSourcePosition s))
    _ -> Nothing

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

    (Stepped stepper, ui') ->
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

instance SDisplay Storage where
  sexp (Symbolic _ _) = error "idk"
  sexp (Concrete d) = sexp d

instance SDisplay VM where
  sexp x =
    L [ L [A "result", sexp (view result x)]
      , L [A "state", sexp (view state x)]
      , L [A "frames", sexp (view frames x)]
      , L [A "contracts", sexp (view (env . contracts) x)]
      ]

quoted :: Text -> Text
quoted x = "\"" <> x <> "\""

instance SDisplay Addr where
  sexp = A . quoted . pack . show

instance SDisplay Contract where
  sexp x =
    L [ L [A "storage", sexp (view storage x)]
      , L [A "balance", sexp (view balance x)]
      , L [A "nonce", sexp (view nonce x)]
      , L [A "codehash", sexp (view codehash x)]
      ]

instance SDisplay W256 where
  sexp x = A (txt (txt x))

-- no idea what's going on here
instance SDisplay (SWord 256) where
  sexp x = A (txt (txt x))

-- no idea what's going on here
instance SDisplay (SymWord) where
  sexp x = A (txt (txt x))

-- no idea what's going on here
instance SDisplay (SWord 8) where
  sexp x = A (txt (txt x))

-- no idea what's going on here
instance SDisplay Buffer where
  sexp (SymbolicBuffer x) = sexp x
  sexp (ConcreteBuffer x) = sexp x

instance (SDisplay k, SDisplay v) => SDisplay (Map k v) where
  sexp x = L [L [sexp k, sexp v] | (k, v) <- Map.toList x]

instance SDisplay a => SDisplay (Maybe a) where
  sexp Nothing = A "nil"
  sexp (Just x) = sexp x

instance SDisplay VMResult where
  sexp = \case
    VMFailure e -> L [A "vm-failure", A (txt (txt e))]
    VMSuccess b -> L [A "vm-success", sexp b]

instance SDisplay Frame where
  sexp x =
    L [A "frame", sexp (view frameContext x), sexp (view frameState x)]

instance SDisplay FrameContext where
  sexp _x = A "some-context"

instance SDisplay FrameState where
  sexp x =
    L [ L [A "contract", sexp (view contract x)]
      , L [A "code-contract", sexp (view codeContract x)]
      , L [A "pc", A (txt (view pc x))]
      , L [A "stack", sexp (view stack x)]
      , L [A "memory", sexpMemory (view memory x)]
      ]

instance SDisplay a => SDisplay [a] where
  sexp = L . map sexp

-- this overlaps the neighbouring [a] instance
instance {-# OVERLAPPING #-} SDisplay String where
  sexp x = A (txt x)

instance SDisplay Word where
  sexp (C (FromKeccak bs) x) =
    L [A "hash", A (txt x), sexp bs]
  sexp (C _ x) = A (quoted (txt x))

instance SDisplay ByteString where
  sexp = A . txt . pack . show . ByteStringS

sexpMemory :: Buffer -> SExpr Text
sexpMemory bs =
  if len bs > 1024
  then L [A "large-memory", A (txt (len bs))]
  else sexp bs

defaultUnitTestOptions :: MonadIO m => m UnitTestOptions
defaultUnitTestOptions = do
  params <- liftIO $ getParametersFromEnvironmentVariables Nothing
  pure UnitTestOptions
    { oracle      = Fetch.zero
    , verbose     = Nothing
    , maxIter     = Nothing
    , askSmtIters = Nothing
    , smtTimeout  = Nothing
    , smtState    = Nothing
    , solver      = Nothing
    , match       = ""
    , covMatch    = Nothing
    , fuzzRuns    = 100
    , replay      = Nothing
    , vmModifier  = id
    , dapp        = emptyDapp
    , testParams  = params
    , maxDepth    = Nothing
    , ffiAllowed  = False
    }

initialStateForTest
  :: UnitTestOptions
  -> (Text, Text)
  -> UiVmState
initialStateForTest opts@(UnitTestOptions {..}) (contractPath, testName) =
  ui1
  where
    script = do
      Stepper.evm . pushTrace . EntryTrace $
        "test " <> testName <> " (" <> contractPath <> ")"
      initializeUnitTest opts testContract
      void (runUnitTest opts testName (AbiTuple mempty))
    ui0 =
      UiVmState
        { _uiVm             = vm0
        , _uiVmNextStep     = script
        , _uiVmSolc         = Just testContract
        , _uiVmDapp         = Nothing
        , _uiVmStepCount    = 0
        , _uiVmFirstState   = undefined
        , _uiVmFetcher      = oracle
        , _uiVmMessage      = Nothing
        , _uiVmSentHashes   = Set.empty
        }
    Just testContract =
      view (dappSolcByName . at contractPath) dapp
    vm0 =
      initialUnitTestVm opts testContract
    ui1 =
      updateUiVmState ui0 vm0 & set uiVmFirstState ui1
