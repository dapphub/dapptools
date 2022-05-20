{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : SMT2.Exec
Description : Parallel execution of SMT2 queries
-}
module SMT2.Exec (
  Solver(..),
  SolverInstance(..),
  CheckSatResult(..),
  checkSat,
  withSolvers,
  SMT2.Exec.test
)where

import GHC.Natural
import Control.Monad
import System.Process (createProcess, cleanupProcess, proc, ProcessHandle, std_in, std_out, std_err, StdStream(..))
import GHC.IO.Handle (Handle, hGetLine, hPutStr, hFlush)
import Control.Concurrent.Chan (Chan(..), newChan, writeChan, readChan)
import Control.Concurrent (forkIO, killThread)

import SMT2.Syntax.Typed (Script(..), Command(..), Option(..))
import SMT2.Parse

-- | Supported solvers
data Solver
  = Z3
  | CVC5
  | Bitwuzla
  | Custom String

instance Show Solver where
  show Z3 = "z3"
  show CVC5 = "cvc5"
  show Bitwuzla = "bitwuzla"
  show (Custom s) = s


-- | A running solver instance
data SolverInstance = SolverInstance
  { _type :: Solver
  , _stdin :: Handle
  , _stdout :: Handle
  , _stderr :: Handle
  , _process :: ProcessHandle
  }

-- | A channel representing a group of solvers
newtype SolverGroup = SolverGroup (Chan Task)

-- | A script to be executed and a channel where the result should be written
data Task = Task Script (Chan CheckSatResult)

-- | The result of a call to (check-sat)
data CheckSatResult
  = Sat
  | Unsat
  | Unknown
  | Error String
  deriving (Show)

-- TODO: can I enforce at the type level that scripts here do not end with (check-sat)?
checkSat :: SolverGroup -> [Script] -> IO [(Script, CheckSatResult)]
checkSat (SolverGroup taskQueue) scripts = do
  -- prepare tasks
  tasks <- forM scripts $ \s -> do
    res <- newChan
    pure $ Task s res

  -- send tasks to solver group
  forM_ tasks (writeChan taskQueue)

  -- collect results
  forM tasks $ \(Task s r) -> do
    res <- readChan r
    pure (s, res)


withSolvers :: Solver -> Natural -> (SolverGroup -> IO a) -> IO a
withSolvers solver count cont = do
  -- spawn solvers
  instances <- mapM (const $ spawnSolver solver) [1..count]

  -- spawn orchestration thread
  taskQueue <- newChan
  availableInstances <- newChan
  forM_ instances (writeChan availableInstances)
  orchestrateId <- forkIO $ orchestrate taskQueue availableInstances

  -- run continuation with task queue
  res <- cont (SolverGroup taskQueue)

  -- cleanup and return results
  mapM_ stopSolver instances
  killThread orchestrateId
  pure res
  where
    orchestrate queue avail = do
      task <- readChan queue
      inst <- readChan avail
      _ <- forkIO $ runTask task inst avail
      pure ()

    runTask (Task (Script cmds) r) inst availableInstances = do
      -- reset solver and send all lines of provided script
      out <- sendScript inst (Script $ Reset : cmds)
      case out of
        -- if we got an error then return it
        Left e -> writeChan r (Error e)
        -- otherwise call (check-sat), parse the result, and send it down the result channel
        Right () -> do
          sat <- sendCommand inst CheckSat
          res <- case sat of
            "sat" -> pure Sat
            "unsat" -> pure Unsat
            "timeout" -> pure Unknown
            "unknown" -> pure Unknown
            _ -> pure . Error $ "Unable to parse solver output: " <> sat
          writeChan r res

      -- put the instance back in the list of available instances
      writeChan availableInstances inst


-- | Arguments used when spawing a solver instance
solverArgs :: Solver -> [String]
solverArgs = \case
  Z3 ->
    [ "-in" ]
  CVC5 ->
    [ "--lang=smt"
    , "--interactive"
    , "--no-interactive-prompt"
    , "--produce-models"
    ]

-- | Spawns a solver instance, and sets the various global config options that we use for our queries
spawnSolver :: Solver -> IO SolverInstance
spawnSolver solver = do
  let cmd = (proc (show solver) (solverArgs solver)) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just stdin, Just stdout, Just stderr, process) <- createProcess cmd
  let solverInstance = SolverInstance solver stdin stdout stderr process
  _ <- sendCommand solverInstance (SetOption $ PrintSuccess True)
  pure solverInstance

-- | Cleanly shutdown a running solver instnace
stopSolver :: SolverInstance -> IO ()
stopSolver (SolverInstance _ stdin stdout stderr process) = cleanupProcess (Just stdin, Just stdout, Just stderr, process)

-- | Sends a list of commands to the solver. Returns the first error, if there was one.
sendScript :: SolverInstance -> Script -> IO (Either String ())
sendScript solver (Script cmds) = case cmds of
  [] -> pure $ Right ()
  hd : tl -> do
    res <- sendCommand solver hd
    if res == "success"
       then sendScript solver (Script tl)
       else pure $ Left res

-- | Sends a single command to the solver, returns the first available line from the output buffer
sendCommand :: SolverInstance -> Command -> IO String
sendCommand (SolverInstance _ stdin stdout _ _) cmd = do
  hPutStr stdin (show cmd <> "\n")
  hFlush stdin
  hGetLine stdout


-- tests ----------------------------------------------------------------------------------------------

prog :: Script
prog = [smt2|
  (assert (or true (true) false))
  (assert (or false (true) false))
|]

test :: IO ()
test = withSolvers Z3 3 $ \solvers -> do
  results <- checkSat solvers (replicate 3 prog)
  forM_ results (print . snd)
