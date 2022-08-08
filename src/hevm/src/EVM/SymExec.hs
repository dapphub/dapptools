{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}

module EVM.SymExec where

import Prelude hiding (Word)

import Debug.Trace

import Control.Lens hiding (pre)
import EVM hiding (Query, Revert, push)
import qualified EVM
import EVM.Exec
import qualified EVM.Fetch as Fetch
import EVM.ABI
import EVM.SMT
import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational
import Control.Monad.State.Strict hiding (state)
import EVM.Types
import EVM.Concrete (createAddress)
import qualified EVM.FeeSchedule as FeeSchedule
import Data.DoubleWord (Word256)
import GHC.Conc (numCapabilities)
import Control.Concurrent.Async
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Control.Monad.State.Class as State
import Data.Bifunctor (first, second)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (isJust, fromJust)

data ProofResult a b c = Qed a | Cex b | Timeout c
  deriving (Show)
type VerifyResult = ProofResult () [(Expr End, [String])] ()
type EquivalenceResult = ProofResult ([VM], [VM]) VM ()


-- | Abstract calldata argument generation
symAbiArg :: AbiType -> Expr Buf
symAbiArg = undefined
--symAbiArg (AbiUIntType n) | n `mod` 8 == 0 && n <= 256 =
  --do x <- concatMapM (const mkByte) [0..(n `div` 8) - 1]
     --return (padLeft' 32 x, 32)
                          -- | otherwise = error "bad type"

--symAbiArg (AbiIntType n)  | n `mod` 8 == 0 && n <= 256 =
  --do x <- concatMapM (const mkByte) [(0 :: Int) ..(n `div` 8) - 1]
     --return (padLeft' 32 x, 32)

                          -- | otherwise = error "bad type"
--symAbiArg AbiBoolType =
  --do x <- mkByte
     --return (padLeft' 32 x, 32)

--symAbiArg AbiAddressType =
  --do x <- concatMapM (const mkByte) [(0 :: Int)..19]
     --return (padLeft' 32 x, 32)

--symAbiArg (AbiBytesType n) | n <= 32 =
  --do x <- concatMapM (const mkByte) [0..n - 1]
     --return (padLeft' 32 x, 32)

                           -- | otherwise = error "bad type"

---- TODO: is this encoding correct?
--symAbiArg (AbiArrayType len typ) =
  --do args <- replicateM len symAbiArg
     --return (litBytes (encodeAbiValue (AbiUInt 256 (fromIntegral len))) <> (concat $ fst <$> args),
             --32 + (sum $ snd <$> args))

--symAbiArg (AbiTupleType tuple) =
  --do args <- mapM symAbiArg (toList tuple)
     --return (concat $ fst <$> args, sum $ snd <$> args)
--symAbiArg n =
  --error $ "Unsupported symbolic abiencoding for"
    -- <> show n
    -- <> ". Please file an issue at https://github.com/dapphub/dapptools if you really need this."

-- | Generates calldata matching given type signature, optionally specialized
-- with concrete arguments.
-- Any argument given as "<symbolic>" or omitted at the tail of the list are
-- kept symbolic.
symCalldata :: Text -> [AbiType] -> [String] -> Expr Buf
symCalldata sig typesignature concreteArgs = undefined
  {-
symCalldata sig typesignature concreteArgs =
  let args = concreteArgs <> replicate (length typesignature - length concreteArgs)  "<symbolic>"
      mkArg typ "<symbolic>" = symAbiArg typ
      mkArg typ arg = let n = litBytes . encodeAbiValue $ makeAbiValue typ arg
                      in return (n, num (length n))
      sig' = litBytes $ selector sig
  in do calldatas <- zipWithM mkArg typesignature args
        return (sig' <> concat (fst <$> calldatas), 4 + (sum $ snd <$> calldatas))
  -}

abstractVM :: Maybe (Text, [AbiType]) -> [String] -> ByteString -> StorageModel -> VM
abstractVM typesignature concreteArgs x storagemodel = undefined
  {-
  (cd', cdlen, cdconstraint) <-
    case typesignature of
      Nothing -> do cd <- sbytes256
                    len <- freshVar_
                    return (cd, var "calldataLength" len, (len .<= 256, Todo "calldatalength < 256" []))
      Just (name, typs) -> do (cd, cdlen) <- symCalldata name typs concreteArgs
                              return (cd, S (Literal cdlen) (literal $ num cdlen), (sTrue, Todo "Trivial" []))
  symstore <- case storagemodel of
    SymbolicS -> Symbolic [] <$> freshArray_ Nothing
    InitialS -> Symbolic [] <$> freshArray_ (Just 0)
    ConcreteS -> return $ Concrete mempty
  c <- SAddr <$> freshVar_
  value' <- var "CALLVALUE" <$> freshVar_
  return $ loadSymVM (RuntimeCode (ConcreteBuffer x)) symstore storagemodel c value' (SymbolicBuffer cd', cdlen) & over constraints ((<>) [cdconstraint])
  -}

loadSymVM :: ContractCode -> Expr Storage -> Expr EWord -> Expr EWord -> Expr Buf -> VM
loadSymVM x initStore addr callvalue' calldata' =
  (makeVm $ VMOpts
    { vmoptContract = initialContract x
    , vmoptCalldata = calldata'
    , vmoptValue = callvalue'
    , vmoptStorageBase = Symbolic
    , vmoptAddress = createAddress ethrunAddress 1
    , vmoptCaller = addr
    , vmoptOrigin = ethrunAddress --todo: generalize
    , vmoptCoinbase = 0
    , vmoptNumber = 0
    , vmoptTimestamp = (Lit 0)
    , vmoptBlockGaslimit = 0
    , vmoptGasprice = 0
    , vmoptDifficulty = 0
    , vmoptGas = 0xffffffffffffffff
    , vmoptGaslimit = 0xffffffffffffffff
    , vmoptBaseFee = 0
    , vmoptPriorityFee = 0
    , vmoptMaxCodeSize = 0xffffffff
    , vmoptSchedule = FeeSchedule.berlin
    , vmoptChainId = 1
    , vmoptCreate = False
    , vmoptTxAccessList = mempty
    , vmoptAllowFFI = False
    }) & set (env . contracts . at (createAddress ethrunAddress 1))
             (Just (initialContract x))
       & set (env . storage) initStore

doInterpret :: Fetch.Fetcher -> Maybe Integer -> Maybe Integer -> VM -> Expr End
doInterpret fetcher maxIter askSmtIters vm = undefined
--doInterpret fetcher maxIter askSmtIters vm = let
      --f (vm', cs) = Node (BranchInfo (if null cs then vm' else vm) Nothing) cs
    --in f <$> interpret' fetcher maxIter askSmtIters vm

-- | Interpreter which explores all paths at branching points.
-- returns an Expr representing the possible executions
interpret
  :: Fetch.Fetcher
  -> Maybe Integer -- max iterations
  -> Maybe Integer -- ask smt iterations
  -> Stepper (Expr End)
  -> StateT VM IO (Expr End)
interpret fetcher maxIter askSmtIters =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action (Expr End)
      -> StateT VM IO (Expr End)

    eval (Operational.Return x) = pure x

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          exec >>= interpret fetcher maxIter askSmtIters . k
        Stepper.Run ->
          run >>= interpret fetcher maxIter askSmtIters . k
        Stepper.IOAct q ->
          mapStateT liftIO q >>= interpret fetcher maxIter askSmtIters . k
        Stepper.Ask (EVM.PleaseChoosePath cond continue) -> do
          assign result Nothing
          vm <- get
          --liftIO $ print $ "BRANCH" <> show (view (state . pc) vm)
          case maxIterationsReached vm maxIter of
            -- TODO: parallelise
            Nothing -> do
              a <- interpret fetcher maxIter askSmtIters (Stepper.evm (continue True) >>= k)
              put vm
              b <- interpret fetcher maxIter askSmtIters (Stepper.evm (continue False) >>= k)
              return $ ITE cond a b
            Just n ->
              interpret fetcher maxIter askSmtIters (Stepper.evm (continue (not n)) >>= k)
        Stepper.Wait q -> do
          let performQuery = do
                m <- liftIO (fetcher q)
                interpret fetcher maxIter askSmtIters (Stepper.evm m >>= k)

          case q of
            --PleaseAskSMT _ _ continue -> do
              --codelocation <- getCodeLocation <$> get
              --iteration <- num . fromMaybe 0 <$> use (iterations . at codelocation)

              ---- if this is the first time we are branching at this point,
              ---- explore both branches without consulting SMT.
              ---- Exploring too many branches is a lot cheaper than
              ---- consulting our SMT solver.
              --if iteration < (fromMaybe 5 askSmtIters)
              --then interpret fetcher maxIter askSmtIters (Stepper.evm (continue EVM.Unknown) >>= k)
              --else performQuery

            _ -> performQuery

        Stepper.EVM m ->
          State.state (runState m) >>= interpret fetcher maxIter askSmtIters . k

maxIterationsReached :: VM -> Maybe Integer -> Maybe Bool
maxIterationsReached _ Nothing = Nothing
maxIterationsReached vm (Just maxIter) =
  let codelocation = getCodeLocation vm
      iters = view (iterations . at codelocation . non 0) vm
  in if num maxIter <= iters
     then view (cache . path . at (codelocation, iters - 1)) vm
     else Nothing

-- TODO: do we need a predicate language here?
type Precondition = VM -> Bool
type Postcondition = Expr End -> Bool

checkAssert :: [Word256] -> ByteString -> Maybe (Text, [AbiType]) -> [String] -> (VerifyResult, VM)
checkAssert errs c signature' concreteArgs = undefined
--checkAssert errs c signature' concreteArgs = verifyContract c signature' concreteArgs SymbolicS (const sTrue) (Just $ checkAssertions errs)

{- |Checks if an assertion violation has been encountered

  hevm recognises the following as an assertion violation:

  1. the invalid opcode (0xfe) (solc < 0.8)
  2. a revert with a reason of the form `abi.encodeWithSelector("Panic(uint256)", code)`, where code is one of the following (solc >= 0.8):
    - 0x00: Used for generic compiler inserted panics.
    - 0x01: If you call assert with an argument that evaluates to false.
    - 0x11: If an arithmetic operation results in underflow or overflow outside of an unchecked { ... } block.
    - 0x12; If you divide or modulo by zero (e.g. 5 / 0 or 23 % 0).
    - 0x21: If you convert a value that is too big or negative into an enum type.
    - 0x22: If you access a storage byte array that is incorrectly encoded.
    - 0x31: If you call .pop() on an empty array.
    - 0x32: If you access an array, bytesN or an array slice at an out-of-bounds or negative index (i.e. x[i] where i >= x.length or i < 0).
    - 0x41: If you allocate too much memory or create an array that is too large.
    - 0x51: If you call a zero-initialized variable of internal function type.

  see: https://docs.soliditylang.org/en/v0.8.6/control-structures.html?highlight=Panic#panic-via-assert-and-error-via-require
-}
checkAssertions :: [Word256] -> Postcondition
checkAssertions errs out = undefined
--checkAssertions errs (_, out) = case view result out of
  --Just (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) -> sFalse
  --Just (EVM.VMFailure (EVM.Revert msg)) -> if msg `elem` (fmap panicMsg errs) then sFalse else sTrue
  --_ -> sTrue

-- |By default hevm checks for all assertions except those which result from arithmetic overflow
defaultPanicCodes :: [Word256]
defaultPanicCodes = [ 0x00, 0x01, 0x12, 0x21, 0x22, 0x31, 0x32, 0x41, 0x51 ]

allPanicCodes :: [Word256]
allPanicCodes = [ 0x00, 0x01, 0x11, 0x12, 0x21, 0x22, 0x31, 0x32, 0x41, 0x51 ]

-- |Produces the revert message for solc >=0.8 assertion violations
panicMsg :: Word256 -> ByteString
panicMsg err = (selector "Panic(uint256)") <> (encodeAbiValue $ AbiUInt 256 err)

verifyContract :: ByteString -> Maybe (Text, [AbiType]) -> [String] -> StorageModel -> Precondition -> Maybe Postcondition -> (VerifyResult, VM)
verifyContract theCode signature' concreteArgs storagemodel pre maybepost = undefined
    --preStateRaw <- abstractVM signature' concreteArgs theCode  storagemodel
    ---- add the pre condition to the pathconditions to ensure that we are only exploring valid paths
    --let preState = over constraints ((++) [(pre preStateRaw, Todo "assumptions" [])]) preStateRaw
    --v <- verify preState Nothing Nothing Nothing maybepost
    --return (v, preState)

pruneDeadPaths :: [VM] -> [VM]
pruneDeadPaths =
  filter $ \vm -> case view result vm of
    Just (VMFailure DeadPath) -> False
    _ -> True

consistentPath :: VM -> Maybe VM
consistentPath vm = undefined
  --resetAssertions
  --constrain $ sAnd $ fst <$> view constraints vm
  --checkSat >>= \case
    --Sat -> return $ Just vm
    --Unk -> return $ Just vm -- the path may still be consistent
    --Unsat -> return Nothing
    --DSat _ -> error "unexpected DSAT"

-- | Stepper that parses the result of Stepper.runFully into an Expr End
runExpr :: Stepper.Stepper (Expr End)
runExpr = do
  vm <- Stepper.runFully
  pure $ case view result vm of
    Nothing -> error "Internal Error: vm in intermediate state after call to runFully"
    Just (VMSuccess buf) -> Return buf (view (env . storage) vm)
    Just (VMFailure e) -> case e of
      UnrecognizedOpcode _ -> Invalid
      SelfDestruction -> SelfDestruct
      EVM.IllegalOverflow -> EVM.Types.IllegalOverflow
      EVM.Revert buf -> EVM.Types.Revert buf
      e' -> EVM.Types.TmpErr $ show e'


-- | Converts a given top level expr into a list of final states and the associated path conditions for each state
flattenExpr :: Expr End -> [(Expr End, [Expr EWord])]
flattenExpr = go []
  where
    go :: [Expr EWord] -> Expr End -> [(Expr End, [Expr EWord])]
    go pcs = \case
      ITE c t f -> go ((Eq c (Lit 1)) : pcs) t <> go ((Eq c (Lit 0)) : pcs) f
      Invalid -> [(Invalid, pcs)]
      SelfDestruct -> [(SelfDestruct, pcs)]
      Revert buf -> [(Revert buf, pcs)]
      Return  buf store -> [(Return buf store, pcs)]
      _ -> undefined

-- | Simple recursive match based AST simplification
-- Note: may not terminate!
simplify :: Expr a -> Expr a
simplify e = if (mapExpr go e == e)
               then e
               else simplify (mapExpr go e)
  where
    go :: Expr a -> Expr a
    -- redundant CopySlice
    go (CopySlice (Lit 0x0) (Lit 0x0) (Lit 0x0) _ dst) = dst
    -- redundant Eq
    go o@(Eq a b)
      | a == b = (Lit 1)
      | otherwise = o
    -- redundant ITE
    go o@(ITE c a b)
      | c == Lit 1 = a
      | c == Lit 0 = b
      | a == b = a
      | otherwise = o
    -- redundant add / sub
    go o@(Sub (Add a b) c)
      | a == c = b
      | b == c = a
      | otherwise = o
    go a = a

reachableQueries :: SolverGroup -> Expr End -> IO [SMT2]
reachableQueries solvers = go []
  where
    go :: [Prop] -> Expr End -> IO [SMT2]
    go pcs = \case
      ITE c t f -> do
        let
          tquery = assertProps (PEq c (Lit 1) : pcs)
          fquery = assertProps (PEq c (Lit 0) : pcs)
        tchildren <- go (PEq c (Lit 1) : pcs) t
        fchildren <- go (PEq c (Lit 0) : pcs) f
        pure $ [tquery, fquery] <> tchildren <> fchildren
      Invalid -> pure []
      SelfDestruct -> pure []
      Revert msg -> pure []
      Return msg store -> pure []
      EVM.Types.IllegalOverflow -> pure []
      TmpErr e -> error $ "TmpErr: " <> show e

-- | Strips unreachable branches from a given expr
-- Returns a list of executed SMT queries alongside the reduced expression for debugging purposes
-- Note that the reduced expression loses information relative to the original
-- one if jump conditions are removed. This restriction can be removed once
-- Expr supports attaching knowledge to AST nodes.
-- Although this algorithm currently parallelizes nicely, it does not exploit
-- the incremental nature of the task at hand. Introducing support for
-- incremental queries might let us go even faster here.
-- TODO: handle errors properly
reachable2 :: SolverGroup -> Expr End -> IO ([SMT2], Expr End)
reachable2 solvers e = do
    res <- go [] e
    pure $ second (fromMaybe (error "oops")) res
  where
    {-
       walk down the tree and collect pcs.
       dispatch a reachability query at each leaf.
       if reachable return the expr wrapped in a Just. If not return Nothing.
       when walking back up the tree drop unreachable subbranches.
    -}
    go :: [Prop] -> Expr End -> IO ([SMT2], Maybe (Expr End))
    go pcs = \case
      ITE c t f -> do
        (tres, fres) <- concurrently
          (go (PEq (Lit 1) c : pcs) t)
          (go (PEq (Lit 0) c : pcs) f)
        let subexpr = case (snd tres, snd fres) of
              (Just t', Just f') -> Just $ ITE c t' f'
              (Just t', Nothing) -> Just t'
              (Nothing, Just f') -> Just f'
              (Nothing, Nothing) -> Nothing
        pure (fst tres <> fst fres, subexpr)
      leaf -> do
        let query = assertProps pcs
        res <- checkSat' solvers (query, [])
        case res of
          Sat _ -> pure ([query], Just leaf)
          Unsat -> pure ([query], Nothing)
          r -> error $ "Invalid solver result: " <> show r

-- | Strips unreachable branches from a given expr
-- Returns a list of executed SMT queries alongside the reduced expression for debugging purposes
-- Note that the reduced expression loses information relative to the original
-- one if jump conditions are removed. This restriction can be removed once
-- Expr supports attaching knowledge to AST nodes.
-- Although this algorithm currently parallelizes nicely, it does not exploit
-- the incremental nature of the task at hand. Introducing support for
-- incremental queries might let us go even faster here.
-- TODO: handle errors properly
reachable :: SolverGroup -> Expr End -> IO ([SMT2], Expr End)
reachable solvers = go []
  where
    go :: [Prop] -> Expr End -> IO ([SMT2], Expr End)
    go pcs = \case
      ITE c t f -> do
        let
          tquery = assertProps (PEq c (Lit 1) : pcs)
          fquery = assertProps (PEq c (Lit 0) : pcs)
        tres <- (checkSat' solvers (tquery, []))
        fres <- (checkSat' solvers (fquery, []))
        print (tres, fres)
        case (tres, fres) of
          (Error tm, Error fm) -> do
            writeFile "tquery.smt2" (T.unpack $ formatSMT2 tquery)
            writeFile "fquery.smt2" (T.unpack $ formatSMT2 fquery)
            error $ "Solver Errors: " <> (T.unpack . T.unlines $ [tm, fm])
          (Error tm, _) -> do
            putStrLn $ T.unpack $ formatSMT2 tquery
            error $ "Solver Error: " <> T.unpack tm
          (_ , Error fm) -> error $ "Solver Error: " <> T.unpack fm
          (EVM.SMT.Unknown, _) -> error "Solver timeout, unable to analyze reachability"
          (_, EVM.SMT.Unknown) -> error "Solver timeout, unable to analyze reachability"
          (Unsat, Sat _) -> go (PEq c (Lit 0) : pcs) f
          (Sat _, Unsat) -> go (PEq c (Lit 1) : pcs) t
          (Sat _, Sat _) -> do
            ((tqs, texp), (fqs, fexp)) <- concurrently
              (go (PEq c (Lit 1) : pcs) t)
              (go (PEq c (Lit 0) : pcs) f)
            pure ([tquery, fquery] <> tqs <> fqs, ITE c texp fexp)
          (Unsat, Unsat) -> do
            putStrLn $ "pcs: " <> show pcs
            putStrLn $ "tquery:\n " <> (T.unpack $ formatSMT2 tquery)
            putStrLn $ "fquery:\n " <> (T.unpack $ formatSMT2 fquery)
            error "Internal Error: two unsat branches found"
      Invalid -> pure ([], Invalid)
      SelfDestruct -> pure ([], SelfDestruct)
      Revert msg -> pure ([], Revert msg)
      Return msg store -> pure ([], Return msg store)
      EVM.Types.IllegalOverflow -> pure ([], EVM.Types.IllegalOverflow)
      TmpErr e -> error $ "TmpErr: " <> show e


-- | Symbolically execute the VM and check all endstates against the postcondition, if available.
verify :: VM -> Maybe Integer -> Maybe Integer -> Maybe (Fetch.BlockNumber, Text) -> Maybe Postcondition -> IO VerifyResult
verify preState maxIter askSmtIters rpcinfo maybepost = do
  expr <- evalStateT (interpret (Fetch.oracle Nothing False) Nothing Nothing runExpr) preState
  let leaves = flattenExpr expr
      -- we are only interested in end states where the postcondition has been violated
      violated = case maybepost of
                   Just p -> filter (p . fst) leaves
                   Nothing -> leaves
      queries = fmap (second (\pcs -> (assertWords pcs, ["txdata", "storage"]))) violated
      findExpr s = fmap fst $ find (\(e,(s', _)) -> s == s') queries
  reachable <- withSolvers Z3 (num numCapabilities) $ \solvers -> do
    forM_ queries $ \(_, (SMT2 q,_)) -> do
      putStrLn $ T.unpack $ T.unlines q
    rs <- checkSat solvers (fmap snd queries)
    let rs' = fmap (first findExpr) rs
        rs'' = fmap (first fromJust) (filter (isJust . fst) rs')
        rs''' = filter (isSat . snd) rs''
    pure rs'''
  pure $ if null reachable then Qed () else Cex (fmap (second (fmap T.unpack . getModels)) reachable)
  where
    getModels (Sat ms) = ms
    getModels _ = []

  -- check prop on each leaf
  -- if prop violated then:
  --   - gather path conditions
  --   - check satisfiability of path conditions
  --pure ()
  --case maybepost of
    --(Just post) -> do
      --let livePaths = pruneDeadPaths $ leaves tree
          ---- have we hit max iterations at any point in a given path
          --maxReached :: VM -> Bool
          --maxReached p = case maxIter of
            --Just maxI -> any (>= (fromInteger maxI)) (view iterations p)
            --Nothing -> False
          ---- is there any path which can possibly violate the postcondition?
          ---- can also do these queries individually (even concurrently!). Could save time and report multiple violations
          --postC = sOr $ fmap (\postState -> (sAnd (fst <$> view constraints postState)) .&& sNot (post (preState, postState))) livePaths
      --resetAssertions
      --constrain postC
      --io $ putStrLn "checking postcondition..."
      --checkSat >>= \case
        --Unk -> do io $ putStrLn "postcondition query timed out"
                  --return $ Timeout tree
        --Unsat -> do
          --if any maxReached livePaths
            --then io $ putStrLn "WARNING: max iterations reached, execution halted prematurely"
            --else io $ putStrLn "Q.E.D."
          --return $ Qed tree
        --Sat -> return $ Cex tree
        --DSat _ -> error "unexpected DSAT"

    --Nothing -> do io $ putStrLn "Nothing to check"
                  --return $ Qed tree

-- | Compares two contract runtimes for trace equivalence by running two VMs and comparing the end states.
equivalenceCheck :: ByteString -> ByteString -> Maybe Integer -> Maybe Integer -> Maybe (Text, [AbiType]) -> EquivalenceResult
equivalenceCheck bytecodeA bytecodeB maxiter askSmtIters signature' = undefined
  --let
    --bytecodeA' = if BS.null bytecodeA then BS.pack [0] else bytecodeA
    --bytecodeB' = if BS.null bytecodeB then BS.pack [0] else bytecodeB
  --preStateA <- abstractVM signature' [] bytecodeA' SymbolicS

  --let preself = preStateA ^. state . contract
      --precaller = preStateA ^. state . caller
      --callvalue' = preStateA ^. state . callvalue
      --prestorage = preStateA ^?! env . contracts . ix preself . storage
      --(calldata', cdlen) = view (state . calldata) preStateA
      --pathconds = view constraints preStateA
      --preStateB = loadSymVM (RuntimeCode (ConcreteBuffer bytecodeB')) prestorage SymbolicS precaller callvalue' (calldata', cdlen) & set constraints pathconds

  --smtState <- queryState
  --push 1
  --aVMs <- doInterpret (Fetch.oracle (Just smtState) Nothing False) maxiter askSmtIters preStateA
  --pop 1
  --push 1
  --bVMs <- doInterpret (Fetch.oracle (Just smtState) Nothing False) maxiter askSmtIters preStateB
  --pop 1
  ---- Check each pair of endstates for equality:
  --let differingEndStates = uncurry distinct <$> [(a,b) | a <- pruneDeadPaths (leaves aVMs), b <- pruneDeadPaths (leaves bVMs)]
      --distinct a b =
        --let (aPath, bPath) = both' (view constraints) (a, b)
            --(aSelf, bSelf) = both' (view (state . contract)) (a, b)
            --(aEnv, bEnv) = both' (view (env . contracts)) (a, b)
            --(aResult, bResult) = both' (view result) (a, b)
            ----(Symbolic _ aStorage, Symbolic _ bStorage) = (view storage (aEnv ^?! ix aSelf), view storage (bEnv ^?! ix bSelf))
            --differingResults = case (aResult, bResult) of

              --(Just (VMSuccess aOut), Just (VMSuccess bOut)) ->
                --aOut ./= bOut .|| aStorage ./= bStorage .|| fromBool (aSelf /= bSelf)

              --(Just (VMFailure UnexpectedSymbolicArg), _) ->
                --error $ "Unexpected symbolic argument at opcode: " <> maybe "??" show (vmOp a) <> ". Not supported (yet!)"

              --(_, Just (VMFailure UnexpectedSymbolicArg)) ->
                --error $ "Unexpected symbolic argument at opcode: " <> maybe "??" show (vmOp a) <> ". Not supported (yet!)"

              --(Just (VMFailure _), Just (VMFailure _)) -> sFalse

              --(Just _, Just _) -> sTrue

              --errormsg -> error $ show errormsg

        --in sAnd (fst <$> aPath) .&& sAnd (fst <$> bPath) .&& differingResults
  ---- If there exists a pair of endstates where this is not the case,
  ---- the following constraint is satisfiable
  --constrain $ sOr differingEndStates

  --checkSat >>= \case
     --Unk -> return $ Timeout ()
     --Sat -> return $ Cex preStateA
     --Unsat -> return $ Qed (leaves aVMs, leaves bVMs)
     --DSat _ -> error "unexpected DSAT"

both' :: (a -> b) -> (a, a) -> (b, b)
both' f (x, y) = (f x, f y)

showCounterexample :: VM -> Maybe (Text, [AbiType]) -> ()
showCounterexample vm maybesig = undefined
  --let (calldata', S _ cdlen) = view (EVM.state . EVM.calldata) vm
      --S _ cvalue = view (EVM.state . EVM.callvalue) vm
      --SAddr caller' = view (EVM.state . EVM.caller) vm
  --cdlen' <- num <$> getValue cdlen
  --calldatainput <- case calldata' of
    --SymbolicBuffer cd -> mapM (getValue.fromSized) (take cdlen' cd) >>= return . pack
    --ConcreteBuffer cd -> return $ BS.take cdlen' cd
  --callvalue' <- getValue cvalue
  --caller'' <- num <$> getValue caller'
  --io $ do
    --putStrLn "Calldata:"
    --print $ ByteStringS calldatainput

    ---- pretty print calldata input if signature is available
    --case maybesig of
      --Just (name, types) -> putStrLn $ unpack (head (splitOn "(" name)) ++
        --show (decodeAbiValue (AbiTupleType (fromList types)) $ Lazy.fromStrict (BS.drop 4 calldatainput))
      --Nothing -> return ()

    --putStrLn "Caller:"
    --print (Addr caller'')
    --putStrLn "Callvalue:"
    --print callvalue'
