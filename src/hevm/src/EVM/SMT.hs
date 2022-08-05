{-# Language DataKinds #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language LambdaCase #-}
{-# Language QuasiQuotes #-}

{- |
    Module: EVM.SMT
    Description: Utilities for building and executing SMT queries from Expr instances
-}
module EVM.SMT where

import Prelude hiding (LT, GT)

import GHC.Natural
import Control.Monad
import GHC.IO.Handle (Handle, hGetLine, hPutStr, hFlush)
import Control.Concurrent.Chan (Chan, newChan, writeChan, readChan)
import Control.Concurrent (forkIO, killThread)
import Data.List (singleton)
import Data.Char (isSpace)
import Data.Containers.ListUtils (nubOrd)

import Data.String.Here
import System.Process (createProcess, cleanupProcess, proc, ProcessHandle, std_in, std_out, std_err, StdStream(..))

import EVM.Types
import EVM.Expr hiding (copySlice, writeWord, op1, op2)


-- ** Encoding ** ----------------------------------------------------------------------------------


newtype SMT2 = SMT2 [String]
  deriving (Eq, Show, Semigroup, Monoid)

formatSMT2 :: SMT2 -> String
formatSMT2 (SMT2 ls) = unlines ls

assertWord :: Expr EWord -> SMT2
assertWord e = prelude
            <> (declareBufs $ referencedBufs e)
            <> SMT2 [""]
            <> (declareVars $ referencedVars e)
            <> SMT2 [""]
            <> (declareFrameContext $ referencedFrameContext e)
            <> SMT2 [""]
            <> (SMT2 . singleton $ "(assert (= " <> exprToSMT (Lit 1) <> " " <> exprToSMT e <> ")")

assertWords :: [Expr EWord] -> SMT2
assertWords es = prelude
            <> (declareBufs . nubOrd $ foldl (<>) [] (fmap (referencedBufs) es))
            <> SMT2 [""]
            <> (declareVars . nubOrd $ foldl (<>) [] (fmap (referencedVars) es))
            <> SMT2 [""]
            <> (declareFrameContext . nubOrd $ foldl (<>) [] (fmap (referencedFrameContext) es))
            <> SMT2 [""]
            <> (SMT2 $ fmap (\e -> "(assert (= " <> exprToSMT (Lit 1) <> " " <> exprToSMT e <> "))") es)

assertProps :: [Prop] -> SMT2
assertProps ps = prelude
            <> (declareBufs . nubOrd $ foldl (<>) [] (fmap (referencedBufs') ps))
            <> SMT2 [""]
            <> (declareVars . nubOrd $ foldl (<>) [] (fmap (referencedVars') ps))
            <> SMT2 [""]
            <> (declareFrameContext . nubOrd $ foldl (<>) [] (fmap (referencedFrameContext') ps))
            <> SMT2 [""]
            <> (SMT2 $ fmap (\p -> "(assert " <> propToSMT p <> ")") ps)

prelude :: SMT2
prelude = SMT2 . fmap (dropWhile isSpace) . lines $ [i|
    ; hash functions
    (declare-fun keccak ((Array (_ BitVec 256) (_ BitVec 8))) (_ BitVec 256))
    (declare-fun sha256 ((Array (_ BitVec 256) (_ BitVec 8))) (_ BitVec 256))

    ; bufLength
    (declare-fun bufLength ((Array (_ BitVec 256) (_ BitVec 8))) (_ BitVec 256))

    ; block context
    (declare-fun blockhash ((_ BitVec 256)) (_ BitVec 256))
    (declare-const origin (_ BitVec 256))
    (declare-const coinbase (_ BitVec 256))
    (declare-const timestamp (_ BitVec 256))
    (declare-const blocknumber (_ BitVec 256))
    (declare-const difficulty (_ BitVec 256))
    (declare-const gaslimit (_ BitVec 256))
    (declare-const chainid (_ BitVec 256))
    (declare-const basefee (_ BitVec 256))

    ; storage
    (declare-const storage (Array (_ BitVec 256) (Array (_ BitVec 256) (_ BitVec 256))))
  |]

declareBufs :: [String] -> SMT2
declareBufs names = SMT2 $ ["; buffers"] <> fmap declare names
  where
    declare n = "(declare-const " <> n <> " (Array (_ BitVec 256) (_ BitVec 8)))"

referencedBufs :: Expr a -> [String]
referencedBufs expr = nubOrd (foldExpr go [] expr)
  where
    go :: Expr a -> [String]
    go = \case
      AbstractBuf s -> [s]
      _ -> []

referencedBufs' :: Prop -> [String]
referencedBufs' = \case
  (PEq a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PLT a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PGT a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PLEq a b) -> nubOrd $ referencedBufs a <> referencedBufs b
  (PGEq a b) -> nubOrd $ referencedBufs a <> referencedBufs b

referencedVars' :: Prop -> [String]
referencedVars' = \case
  (PEq a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PLT a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PGT a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PLEq a b) -> nubOrd $ referencedVars a <> referencedVars b
  (PGEq a b) -> nubOrd $ referencedVars a <> referencedVars b

referencedFrameContext' :: Prop -> [String]
referencedFrameContext' = \case
  (PEq a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PLT a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PGT a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PLEq a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b
  (PGEq a b) -> nubOrd $ referencedFrameContext a <> referencedFrameContext b

declareVars :: [String] -> SMT2
declareVars names = SMT2 $ ["; variables"] <> fmap declare names
  where
    declare n = "(declare-const " <> n <> " (_ BitVec 256))"

referencedVars :: Expr a -> [String]
referencedVars expr = nubOrd (foldExpr go [] expr)
  where
    go :: Expr a -> [String]
    go = \case
      Var s -> [s]
      _ -> []

declareFrameContext :: [String] -> SMT2
declareFrameContext names = SMT2 $ ["; frame context"] <> fmap declare names
  where
    declare n = "(declare-const " <> n <> " (_ BitVec 256))"

referencedFrameContext :: Expr a -> [String]
referencedFrameContext expr = nubOrd (foldExpr go [] expr)
  where
    go :: Expr a -> [String]
    go = \case
      CallValue a -> ["callvalue_" <> show a]
      Caller a -> ["caller_" <> show a]
      Address a -> ["address_" <> show a]
      Balance {} -> error "TODO: BALANCE"
      SelfBalance {} -> error "TODO: SELFBALANCE"
      Gas {} -> error "TODO: GAS"
      _ -> []

exprToSMT :: Expr a -> String
exprToSMT = \case
  Lit w -> "#x" <> (padLeftStr 64 . strip0x' . show $ w)
  Var s -> s
  JoinBytes
    zero one two three four five six seven
    eight nine ten eleven twelve thirteen fourteen fifteen
    sixteen seventeen eighteen nineteen twenty twentyone twentytwo twentythree
    twentyfour twentyfive twentysix twentyseven twentyeight twentynine thirty thirtyone
    -> concatBytes
        [ zero, one, two, three, four, five, six, seven
        , eight, nine, ten, eleven, twelve, thirteen, fourteen, fifteen
        , sixteen, seventeen, eighteen, nineteen, twenty, twentyone, twentytwo, twentythree
        , twentyfour, twentyfive, twentysix, twentyseven, twentyeight, twentynine, thirty, thirtyone]

  Add a b -> op2 "bvadd" a b
  Sub a b -> op2 "bvsub" a b
  Mul a b -> op2 "bvmul" a b
  Div a b -> op2 "bvudiv" a b
  Exp a b -> case b of
               Lit b' -> expandExp a b'
               _ -> error "cannot encode symbolic exponentation into SMT"
  Min a b -> "(ite (<= " <> exprToSMT a <> " " <> exprToSMT b <> ") " <> exprToSMT a <> " " <> exprToSMT b <> ")"

  LT a b -> "(ite " <> op2 "bvult" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  SLT a b -> "(ite " <> op2 "bvslt" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  GT a b -> "(ite " <> op2 "bvugt" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  LEq a b -> exprToSMT $ Not (LT b a)
  GEq a b -> exprToSMT $ Not (LT a b)
  Eq a b -> "(ite " <> op2 "=" a b <> " " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"
  IsZero a -> "(ite (= " <> exprToSMT a <> " " <> exprToSMT (Lit 0) <> ") " <> exprToSMT (Lit 1) <> " " <> exprToSMT (Lit 0) <> ")"

  And a b -> op2 "bvand" a b
  Or a b -> op2 "bvor" a b
  Xor a b -> exprToSMT $ And (Or a b) (Not (And a b))
  Not a -> op1 "bvnot" a
  SHL a b -> op2 "bvshl" a b
  SHR a b -> op2 "bvlshr" b a -- TODO: is lshr the same as shr?
  EqByte a b -> "(= " <> exprToSMT a  <> " " <> exprToSMT b <> ")"

  Keccak a -> "(keccak " <> exprToSMT a <> ")"
  SHA256 a -> "(sha256 " <> exprToSMT a <> ")"

  CallValue a -> "callvalue_" <> show a
  Caller a -> "caller_" <> show a
  Address a -> "address_" <> show a

  Origin -> "origin"
  BlockHash a -> "(blockhash " <> exprToSMT a <> ")"
  Coinbase -> "coinbase"
  Timestamp -> "timestamp"
  BlockNumber -> "blocknumber"
  Difficulty -> "difficulty"
  GasLimit -> "gaslimit"
  ChainId -> "chainid"
  BaseFee -> "basefee"

  -- TODO: make me binary...
  LitByte b -> "#x" <> (strip0x' . show $ (num b :: W256))
  IndexWord w idx -> "((_ extract " <> exprToSMT (add idx (Lit 7)) <> " " <> exprToSMT idx  <> ") " <> exprToSMT w <> ")"
  ReadByte idx src -> op2 "select" src idx

  EmptyBuf -> "((as const (Array (_ BitVec 256) (_ BitVec 8))) #b00000000)"
  ConcreteBuf bs -> error "TODO: concreteBuf"
  AbstractBuf s -> s
  WriteByte idx val prev -> op3 "store" prev idx val
  WriteWord idx val prev -> writeWord idx val prev
  ReadWord idx prev -> EVM.SMT.readWord idx prev
  BufLength b -> op1 "bufLength" b
  CopySlice dstIdx srcIdx size src dst -> copySlice dstIdx srcIdx size src dst

  EmptyStore -> "((as const (Array (_ BitVec 256) (Array (_ BitVec 256) (_ BitVec 256)))) ((as const (Array (_ BitVec 256) (_ BitVec 256)))" <> exprToSMT (Lit 0) <> "))"
  ConcreteStore s -> error "TODO: concretestore"
  AbstractStore -> "storage"
  SStore addr idx val store -> "(store " <> exprToSMT store <> " " <> exprToSMT addr <> " (store (select " <> exprToSMT addr <> " " <> exprToSMT store <> ") " <> exprToSMT idx <> " " <> exprToSMT val <> "))"
  SLoad addr idx store -> "(select " <> exprToSMT idx <> " (select " <> exprToSMT addr <> " " <> exprToSMT store <> "))"

  a -> error $ "TODO: implement: " <> show a
  where
    op1 op a = "(" <> op <> " " <> exprToSMT a <> ")"
    op2 op a b = "(" <> op <> " " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    op3 op a b c = "(" <> op <> " " <> exprToSMT a <> " " <> exprToSMT b <> " " <> exprToSMT c <> ")"

propToSMT :: Prop -> String
propToSMT = \case
    PEq a b -> "(= " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PLT a b -> "(< " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PGT a b -> "(> " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PLEq a b -> "(<= " <> exprToSMT a <> " " <> exprToSMT b <> ")"
    PGEq a b -> "(>= " <> exprToSMT a <> " " <> exprToSMT b <> ")"


-- ** Execution ** -------------------------------------------------------------------------------


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

-- | A script to be executed, a list of models to be extracted in the case of a sat result, and a channel where the result should be written
data Task = Task
  { script :: SMT2
  , models :: [String]
  , resultChan :: Chan CheckSatResult
  }

-- | The result of a call to (check-sat)
data CheckSatResult
  = Sat [String]
  | Unsat
  | Unknown
  | Error String
  deriving (Show)

isSat :: CheckSatResult -> Bool
isSat (Sat _) = True
isSat _ = False

isErr :: CheckSatResult -> Bool
isErr (Error _) = True
isErr _ = False

checkSat' :: SolverGroup -> (SMT2, [String]) -> IO (CheckSatResult)
checkSat' (SolverGroup taskQueue) (script, models) = do
  -- prepare tasks
  res <- newChan
  let task = Task script models res

  -- send tasks to solver group
  writeChan taskQueue task

  -- collect results
  readChan res

checkSat :: SolverGroup -> [(SMT2, [String])] -> IO [(SMT2, CheckSatResult)]
checkSat (SolverGroup taskQueue) scripts = do
  -- prepare tasks
  tasks <- forM scripts $ \(s, ms) -> do
    res <- newChan
    pure $ Task s ms res

  -- send tasks to solver group
  forM_ tasks (writeChan taskQueue)

  -- collect results
  forM tasks $ \(Task s _ r) -> do
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
      orchestrate queue avail

    runTask (Task s@(SMT2 cmds) ms r) inst availableInstances = do
      -- reset solver and send all lines of provided script
      out <- sendScript inst (SMT2 $ "(reset)" : cmds)
      case out of
        -- if we got an error then return it
        Left e -> writeChan r (Error e)
        -- otherwise call (check-sat), parse the result, and send it down the result channel
        Right () -> do
          sat <- sendLine inst "(check-sat)"
          res <- case sat of
            "sat" -> do
              models <- forM ms $ \m -> do
                getValue inst m
              pure $ Sat models
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
  Bitwuzla -> error "TODO: Bitwuzla args"
  Z3 ->
    [ "-in" ]
  CVC5 ->
    [ "--lang=smt"
    , "--interactive"
    , "--no-interactive-prompt"
    , "--produce-models"
    ]
  Custom _ -> []

-- | Spawns a solver instance, and sets the various global config options that we use for our queries
spawnSolver :: Solver -> IO SolverInstance
spawnSolver solver = do
  let cmd = (proc (show solver) (solverArgs solver)) { std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe }
  (Just stdin, Just stdout, Just stderr, process) <- createProcess cmd
  let solverInstance = SolverInstance solver stdin stdout stderr process
  _ <- sendCommand solverInstance "(set-option :print-success true)"
  pure solverInstance

-- | Cleanly shutdown a running solver instnace
stopSolver :: SolverInstance -> IO ()
stopSolver (SolverInstance _ stdin stdout stderr process) = cleanupProcess (Just stdin, Just stdout, Just stderr, process)

-- | Sends a list of commands to the solver. Returns the first error, if there was one.
sendScript :: SolverInstance -> SMT2 -> IO (Either String ())
sendScript solver (SMT2 cmds) = case cmds of
  [] -> pure $ Right ()
  hd : tl -> do
    res <- sendCommand solver hd
    if res == "success"
       then sendScript solver (SMT2 tl)
       else pure $ Left res

-- | Sends a single command to the solver, returns the first available line from the output buffer
sendCommand :: SolverInstance -> String -> IO String
sendCommand inst cmd = do
  -- trim leading whitespace
  let cmd' = dropWhile isSpace cmd
  case cmd' of
    "" -> pure "success"      -- ignore blank lines
    ';' : _ -> pure "success" -- ignore comments
    _ -> sendLine inst cmd'

-- | Sends a string to the solver and appends a newline, returns the first available line from the output buffer
sendLine :: SolverInstance -> String -> IO String
sendLine (SolverInstance _ stdin stdout _ _) cmd = do
  hPutStr stdin (cmd <> "\n")
  hFlush stdin
  hGetLine stdout

-- | Returns a string representation of the model for the requested variable
getValue :: SolverInstance -> String -> IO String
getValue (SolverInstance _ stdin stdout _ _) var = do
  hPutStr stdin ("(get-value (" <> var <> "))\n")
  hFlush stdin
  fmap (unlines . reverse) (readSExpr stdout)

-- | Reads lines from h until we have a balanced sexpr
readSExpr :: Handle -> IO [String]
readSExpr h = go 0 0 []
  where
    go :: Int -> Int -> [String] -> IO [String]
    go 0 0 _ = do
      line <- hGetLine h
      let ls = length $ filter (== '(') line
          rs = length $ filter (== ')') line
      if ls == rs
         then pure [line]
         else go ls rs [line]
    go ls rs prev = do
      line <- hGetLine h
      let ls' = length $ filter (== '(') line
          rs' = length $ filter (== ')') line
      if (ls + ls') == (rs + rs')
         then pure $ line : prev
         else go (ls + ls') (rs + rs') (line : prev)



-- ** Helpers ** ---------------------------------------------------------------------------------


writeWord :: Expr EWord -> Expr EWord -> Expr Buf -> String
writeWord idx val buf = go buf 31
  where
    go :: Expr Buf -> Int -> String
    go b n
      | n == 0 = "(store " <> exprToSMT b  <> " " <> exprToSMT idx <> exprToSMT (IndexWord val (Lit 0)) <> ")"
      | otherwise = "(store " <> go b (n - 1) <> " " <> exprToSMT (add idx (Lit $ num n)) <> " " <> (exprToSMT $ IndexWord val (Lit $ num n)) <> ")"

readWord :: Expr EWord -> Expr Buf -> String
readWord idx buf = concatBytes $ go 0
  where
    go :: W256 -> [Expr Byte]
    go 31 = [(ReadByte (add idx (Lit 31)) buf)]
    go n = (ReadByte (add idx (Lit n)) buf) : go (n + 1)

-- | Stores a region of src into dst
copySlice :: Expr EWord -> Expr EWord -> Expr EWord -> Expr Buf -> Expr Buf -> String
copySlice dstOffset srcOffset size@(Lit _) src dst
  | size == (Lit 0) = "(store " <> exprToSMT dst <> " " <> exprToSMT dstOffset <> "(select " <> exprToSMT srcOffset <> " " <> exprToSMT src <> "))"
  | otherwise = "(store " <> copySlice dstOffset srcOffset (sub size (Lit 1)) src dst <> "(+ " <> exprToSMT dstOffset <> " " <> exprToSMT size <> ") (select (+ " <> exprToSMT srcOffset <> " " <> exprToSMT size <> ") " <> exprToSMT src <> ")"
copySlice _ _ _ _ _ = error "TODO: implement copySlice with a symbolically sized region"

-- | Unrolls an exponentiation into a series of multiplications
expandExp :: Expr EWord -> W256 -> String
expandExp base expnt
  | expnt == 1 = exprToSMT base
  | otherwise = "(* " <> exprToSMT base <> " " <> expandExp base (expnt - 1) <> ")"

-- | Concatenates a list of bytes into a larger bitvector
concatBytes :: [Expr Byte] -> String
concatBytes [b] = exprToSMT b
concatBytes (hd : tl) = "(concat " <> exprToSMT hd <> " " <> concatBytes tl <> ")"
concatBytes [] = error "cannot concat an empty list of bytes" -- TODO: use nonempty here?
