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
import qualified EVM.Expr as Expr
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
import Data.List (foldl')

import Data.ByteString (ByteString)
import qualified Control.Monad.State.Class as State
import Data.Bifunctor (first, second)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (find)
import Data.Maybe (isJust, fromJust)
import Debug.Trace

data ProofResult a b c = Qed a | Cex b | Timeout c
  deriving (Show)
type VerifyResult = ProofResult (Expr End) (Expr End, [Text]) (Expr End)
type EquivalenceResult = ProofResult ([VM], [VM]) VM ()

inRange :: Int -> Expr EWord -> Prop
inRange sz e = PAnd (PGEq e (Lit 0)) (PLEq e (Lit $ 2 ^ sz - 1))

bool :: Expr EWord -> Prop
bool e = POr (PEq e (Lit 1)) (PEq e (Lit 0))

-- | Abstract calldata argument generation
symAbiArg :: Text -> AbiType -> CalldataFragment
symAbiArg name = \case
  AbiUIntType n ->
    if n `mod` 8 == 0 && n <= 256
    then let v = Var name in St $ Fact (inRange n v) v
    else error "bad type"
  AbiIntType n ->
    if n `mod` 8 == 0 && n <= 256
    -- TODO: is this correct?
    then let v = Var name in St $ Fact (inRange n v) v
    else error "bad type"
  AbiBoolType -> let v = Var name in St $ Fact (bool v) v
  AbiAddressType -> let v = Var name in St $ Fact (inRange 160 v) v
  AbiBytesType n
    -> if n > 0 && n <= 32
       then let v = Var name in St $ Fact (inRange (n * 8) v) v
       else error "bad type"
  AbiArrayType sz tp -> Comp $ fmap (\n -> symAbiArg (name <> n) tp) [T.pack (show n) | n <- [0..sz-1]]
  t -> error $ "TODO: symbolic abi encoding for " <> show t

data CalldataFragment
  = St (Expr EWord)
  | Dy (Expr EWord) (Expr Buf)
  | Comp [CalldataFragment]
  deriving (Show, Eq)

-- | Generates calldata matching given type signature, optionally specialized
-- with concrete arguments.
-- Any argument given as "<symbolic>" or omitted at the tail of the list are
-- kept symbolic.
symCalldata :: Text -> [AbiType] -> [String] -> Expr Buf -> Expr Buf
symCalldata sig typesignature concreteArgs base =
  let args = concreteArgs <> replicate (length typesignature - length concreteArgs)  "<symbolic>"
      mkArg :: AbiType -> String -> Int -> CalldataFragment
      mkArg typ "<symbolic>" n = symAbiArg (T.pack $ "arg" <> show n) typ
      mkArg typ arg _ = let v = makeAbiValue typ arg
                        in case v of
                             AbiUInt _ w -> St . Lit . num $ w
                             AbiInt _ w -> St . Lit . num $ w
                             AbiAddress w -> St . Lit . num $ w
                             AbiBool w -> St . Lit $ if w then 1 else 0
                             _ -> error "TODO"
      calldatas = zipWith3 mkArg typesignature args [1..]
      cdBuf = combineFragments calldatas (writeSelector base sig)
  in Fact (Expr.bufLength cdBuf .>= cdLen calldatas) cdBuf

cdLen :: [CalldataFragment] -> Expr EWord
cdLen = go (Lit 4)
  where
    go acc = \case
      [] -> acc
      (hd:tl) -> case hd of
                   St _ -> go (Expr.add acc (Lit 32)) tl
                   _ -> error "unsupported"

writeSelector :: Expr Buf -> Text -> Expr Buf
writeSelector buf sig = writeSel (Lit 0) $ writeSel (Lit 1) $ writeSel (Lit 2) $ writeSel (Lit 3) buf
  where
    sel = ConcreteBuf $ selector sig
    writeSel idx = Expr.writeByte idx (Expr.readByte idx sel)

combineFragments :: [CalldataFragment] -> Expr Buf -> Expr Buf
combineFragments = go (Lit 4)
  where
    go _ [] acc = acc
    go idx (f:rest) acc = case f of
                             St w -> go (Expr.add idx (Lit 32)) rest (Expr.writeWord idx w acc)
                             s -> error $ "unsupported cd fragment: " <> show s


abstractVM :: Maybe (Text, [AbiType]) -> [String] -> ByteString -> StorageModel -> VM
abstractVM typesignature concreteArgs contractCode storagemodel
  = loadSymVM code' store caller' value' calldata'
  where
    calldata' = case typesignature of
                 Nothing -> AbstractBuf "txdata"
                 Just (name, typs) -> symCalldata name typs concreteArgs (AbstractBuf "txdata")
    store = case storagemodel of
              SymbolicS -> AbstractStore
              InitialS -> EmptyStore
              ConcreteS -> ConcreteStore mempty
    caller' = Caller 0
    value' = CallValue 0
    code' = RuntimeCode $ fromJust $ Expr.toList (ConcreteBuf contractCode)

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


type Precondition = VM -> Prop
type Postcondition = VM -> Expr End -> Prop

checkAssert :: SolverGroup -> [Word256] -> ByteString -> Maybe (Text, [AbiType]) -> [String] -> IO [VerifyResult]
checkAssert solvers errs c signature' concreteArgs = verifyContract solvers c signature' concreteArgs SymbolicS Nothing (Just $ checkAssertions errs)

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
checkAssertions errs _ = \case
  Revert (ConcreteBuf msg) -> PBool $ msg `notElem` (fmap panicMsg errs)
  Revert b -> foldl' POr (PBool True) (fmap (PNeg . PEq b . ConcreteBuf . panicMsg) errs)
  _ -> PBool True

-- |By default hevm checks for all assertions except those which result from arithmetic overflow
defaultPanicCodes :: [Word256]
defaultPanicCodes = [ 0x00, 0x01, 0x12, 0x21, 0x22, 0x31, 0x32, 0x41, 0x51 ]

allPanicCodes :: [Word256]
allPanicCodes = [ 0x00, 0x01, 0x11, 0x12, 0x21, 0x22, 0x31, 0x32, 0x41, 0x51 ]

-- |Produces the revert message for solc >=0.8 assertion violations
panicMsg :: Word256 -> ByteString
panicMsg err = (selector "Panic(uint256)") <> (encodeAbiValue $ AbiUInt 256 err)

verifyContract :: SolverGroup -> ByteString -> Maybe (Text, [AbiType]) -> [String] -> StorageModel -> Maybe Precondition -> Maybe Postcondition -> IO [VerifyResult]
verifyContract solvers theCode signature' concreteArgs storagemodel pre maybepost = do
  let preState = abstractVM signature' concreteArgs theCode  storagemodel
  verify solvers preState Nothing Nothing Nothing pre maybepost

pruneDeadPaths :: [VM] -> [VM]
pruneDeadPaths =
  filter $ \vm -> case view result vm of
    Just (VMFailure DeadPath) -> False
    _ -> True

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
flattenExpr :: Expr End -> [([Prop], Expr End)]
flattenExpr = go []
  where
    go :: [Prop] -> Expr End -> [([Prop], Expr End)]
    go pcs = \case
      Fact p e -> go (p : pcs) e
      ITE c t f -> go ((PEq c (Lit 1)) : pcs) t <> go ((PEq c (Lit 0)) : pcs) f
      Invalid -> [(pcs, Invalid)]
      SelfDestruct -> [(pcs, SelfDestruct)]
      Revert buf -> [(pcs, Revert buf)]
      Return  buf store -> [(pcs, Return buf store)]
      EVM.Types.IllegalOverflow -> [(pcs, EVM.Types.IllegalOverflow)]
      TmpErr s -> error s

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

    -- simplify buffers
    go o@(ReadWord (Lit _) _) = Expr.simplifyReads o
    go o@(ReadByte (Lit _) _) = Expr.simplifyReads o

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
    go o@(And a (And b c))
      | a == b = (And b c)
      | otherwise = o
    go a = a

reachableQueries :: Expr End -> IO [SMT2]
reachableQueries = go []
  where
    go :: [Prop] -> Expr End -> IO [SMT2]
    go pcs = \case
      ITE c t f -> do
        (tres, fres) <- concurrently
          (go (PEq (Lit 1) c : pcs) t)
          (go (PEq (Lit 0) c : pcs) f)
        pure (tres <> fres)
      _ -> pure [assertProps pcs]

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
    pure $ second (fromMaybe (error "Internal Error: no reachable paths found")) res
  where
    {-
       Walk down the tree and collect pcs.
       Dispatch a reachability query at each leaf.
       If reachable return the expr wrapped in a Just. If not return Nothing.
       When walking back up the tree drop unreachable subbranches.
    -}
    go :: [Prop] -> Expr End -> IO ([SMT2], Maybe (Expr End))
    go pcs = \case
      Fact p e' -> go (p : pcs) e'
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
      Fact p e -> go (p : pcs) e
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

-- | Evaluate the provided proposition down to it's most concrete result
evalProp :: Prop -> Prop
evalProp = \case
  PBool b -> PBool b
  PNeg (PBool b) -> PBool (not b)
  PNeg p -> p
  PEq l r -> if l == r
             then PBool True
             else PEq l r
  PLT l r -> if l < r
             then PBool True
             else PEq l r
  PGT l r -> if l > r
             then PBool True
             else PEq l r
  PGEq l r -> if l >= r
             then PBool True
             else PEq l r
  PLEq l r -> if l <= r
             then PBool True
             else PEq l r
  PAnd l r -> case (evalProp l, evalProp r) of
                (PBool True, PBool True) -> PBool True
                (PBool _, PBool _) -> PBool False
                _ -> PAnd l r
  POr l r -> case (evalProp l, evalProp r) of
                (PBool False, PBool False) -> PBool False
                (PBool _, PBool _) -> PBool True
                _ -> POr l r


-- | Symbolically execute the VM and check all endstates against the postcondition, if available.
verify :: SolverGroup -> VM -> Maybe Integer -> Maybe Integer -> Maybe (Fetch.BlockNumber, Text) -> Maybe Precondition -> Maybe Postcondition -> IO [VerifyResult]
verify solvers preState maxIter askSmtIters rpcinfo maybePre maybepost = do
  putStrLn "Exploring contract"
  expr <- evalStateT (interpret (Fetch.oracle Nothing False) Nothing Nothing runExpr) preState
  putStrLn $ "Explored contract (" <> show (Expr.numBranches expr) <> " branches)"
  let leaves = flattenExpr expr
  case maybepost of
    Nothing -> pure [Qed expr]
    Just post -> do
      let
        -- Filter out any leaves that can be statically shown to be safe
        canViolate = flip filter leaves $
          \(_, leaf) -> case evalProp (post preState leaf) of
            PBool True -> False
            _ -> True
        assumes = case maybePre of
          Just pre -> [pre preState]
          Nothing -> []
        withQueries = fmap (\(pcs, leaf) -> (assertProps (PNeg (post preState leaf) : assumes <> pcs), leaf)) canViolate
      -- Dispatch the remaining branches to the solver to check for violations
      putStrLn $ "Checking for reachability of " <> show (length withQueries) <> " potential property violations"
      --putStrLn $ T.unpack . formatSMT2 . fst $ withQueries !! 0
      results <- flip mapConcurrently withQueries $ \(query, leaf) -> do
        res <- checkSat' solvers (query, ["txdata", "storage"])
        pure (res, leaf)
      let cexs = filter (\(res, _) -> not . isUnsat $ res) results
      pure $ if null cexs then [Qed expr] else fmap toVRes cexs
  where
    toVRes :: (CheckSatResult, Expr End) -> VerifyResult
    toVRes (res, leaf) = case res of
      Sat model -> Cex (leaf, model)
      EVM.SMT.Unknown -> Timeout leaf
      Unsat -> Qed leaf
      Error e -> error $ "Internal Error: solver responded with error: " <> show e

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
