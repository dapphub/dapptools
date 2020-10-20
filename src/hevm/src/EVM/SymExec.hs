{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language TemplateHaskell #-}

module EVM.SymExec where


import Prelude hiding (Word)

import Control.Lens hiding (pre)
import EVM hiding (Query, push)
import qualified EVM
import EVM.Exec hiding (exec)
import qualified EVM.Fetch as Fetch
import EVM.ABI
import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational
import Control.Monad.State.Strict hiding (state)
import EVM.Types hiding (Word)
import EVM.Concrete (Whiff(..))
import EVM.Symbolic (SymWord(..), sw256)
import EVM.Concrete (createAddress)
import qualified EVM.FeeSchedule as FeeSchedule
import Data.SBV.Trans.Control
import Data.SBV.Trans hiding (distinct, Word)
import Data.SBV hiding (runSMT, newArray_, addAxiom, distinct, sWord8s, Word)
import Data.Vector (toList, fromList)
import Data.Tree

import Control.Monad.IO.Class
import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as BS
import Data.Text (Text, splitOn, unpack)
import Control.Monad.State.Strict (runState, runStateT, execState, StateT(..), get, put, zipWithM, mapStateT)
import qualified Control.Monad.State.Class as State
import Control.Applicative
import Control.Monad.State.Class (MonadState)

-- | Convenience functions for generating large symbolic byte strings
sbytes32, sbytes128, sbytes256, sbytes512, sbytes1024 :: Query ([SWord 8])
sbytes32 = toBytes <$> freshVar_ @ (WordN 256)
sbytes128 = toBytes <$> freshVar_ @ (WordN 1024)
sbytes256 = liftA2 (++) sbytes128 sbytes128
sbytes512 = liftA2 (++) sbytes256 sbytes256
sbytes1024 = liftA2 (++) sbytes512 sbytes512

-- | Abstract calldata argument generation
-- We don't assume input types are restricted to their proper range here;
-- such assumptions should instead be given as preconditions.
-- This could catch some interesting calldata mismanagement errors.
symAbiArg :: AbiType -> Query ([SWord 8], SWord 32)
symAbiArg (AbiUIntType n) | n `mod` 8 == 0 && n <= 256 = do x <- sbytes32
                                                            return (x, 32)
                          | otherwise = error "bad type"

symAbiArg (AbiIntType n)  | n `mod` 8 == 0 && n <= 256 = do x <- sbytes32
                                                            return (x, 32)
                          | otherwise = error "bad type"
symAbiArg AbiBoolType = do x <- sbytes32
                           return (x, 32)

symAbiArg AbiAddressType = do x <- sbytes32
                              return (x, 32)

symAbiArg (AbiBytesType n) | n <= 32 = do x <- sbytes32
                                          return (x, 32)
                           | otherwise = error "bad type"

-- TODO: is this encoding correct?
symAbiArg (AbiArrayType len typ) =
  do args <- mapM symAbiArg (replicate len typ)
     return (litBytes (encodeAbiValue (AbiUInt 256 (fromIntegral len))) <> (concat $ fst <$> args),
             32 + (sum $ snd <$> args))

symAbiArg (AbiTupleType tuple) =
  do args <- mapM symAbiArg (toList tuple)
     return (concat $ fst <$> args, sum $ snd <$> args)
symAbiArg n =
  error $ "Unsupported symbolic abiencoding for"
    <> show n
    <> ". Please file an issue at https://github.com/dapphub/dapptools if you really need this."

-- | Generates calldata matching given type signature, optionally specialized
-- with concrete arguments.
-- Any argument given as "<symbolic>" or omitted at the tail of the list are
-- kept symbolic.
symCalldata :: Text -> [AbiType] -> [String] -> Query ([SWord 8], SWord 32)
symCalldata sig typesignature concreteArgs =
  let args = concreteArgs <> replicate (length typesignature - length concreteArgs)  "<symbolic>"
      mkArg typ "<symbolic>" = symAbiArg typ
      mkArg typ arg = let n = litBytes . encodeAbiValue $ makeAbiValue typ arg
                      in return (n, num (length n))
      sig' = litBytes $ selector sig
  in do calldatas <- zipWithM mkArg typesignature args
        return (sig' <> concat (fst <$> calldatas), 4 + (sum $ snd <$> calldatas))

abstractVM :: Maybe (Text, [AbiType]) -> [String] -> ByteString -> StorageModel -> Query VM
abstractVM typesignature concreteArgs x storagemodel = do
  (cd', cdlen, cdconstraint) <-
    case typesignature of
      Nothing -> do cd <- sbytes256
                    len <- freshVar_
                    return (cd, len, (len .<= 256, Val "calldatalength < 256"))
      Just (name, typs) -> do (cd, cdlen) <- symCalldata name typs concreteArgs
                              return (cd, cdlen, (sTrue, Val "True"))
  symstore <- case storagemodel of
    SymbolicS -> Symbolic <$> freshArray_ Nothing
    InitialS -> Symbolic <$> freshArray_ (Just 0)
    ConcreteS -> return $ Concrete mempty
  c <- SAddr <$> freshVar_
  value' <- sw256 <$> freshVar_
  return $ loadSymVM (RuntimeCode x) symstore storagemodel c value' (SymbolicBuffer cd', cdlen) & over constraints ((<>) [cdconstraint])

loadSymVM :: ContractCode -> Storage -> StorageModel -> SAddr -> SymWord -> (Buffer, SWord 32) -> VM
loadSymVM x initStore model addr callvalue' calldata' =
    (makeVm $ VMOpts
    { vmoptContract = contractWithStore x initStore
    , vmoptCalldata = calldata'
    , vmoptValue = callvalue'
    , vmoptAddress = createAddress ethrunAddress 1
    , vmoptCaller = addr
    , vmoptOrigin = ethrunAddress --todo: generalize
    , vmoptCoinbase = 0
    , vmoptNumber = 0
    , vmoptTimestamp = 0
    , vmoptBlockGaslimit = 0
    , vmoptGasprice = 0
    , vmoptDifficulty = 0
    , vmoptGas = 0xffffffffffffffff
    , vmoptGaslimit = 0xffffffffffffffff
    , vmoptMaxCodeSize = 0xffffffff
    , vmoptSchedule = FeeSchedule.istanbul
    , vmoptChainId = 1
    , vmoptCreate = False
    , vmoptStorageModel = model
    }) & set (env . contracts . at (createAddress ethrunAddress 1))
             (Just (contractWithStore x initStore))

-- todo branchinfo needs to contain two vm's,
-- the start vm, and the vm before the branching point/ at the (endstate/leaf)
data BranchInfo = BranchInfo
  { _vm                 :: VM,
    _branchCondition    :: Maybe Whiff
  }

data WrapVM = WrapVM
  { _wrapvm :: VM
  , _old :: VM
  }

makeLenses ''WrapVM

-- Nothing -> State.state (runState exec1) >> exec
-- exec1 :: EVM ()
exec :: MonadState WrapVM m => m VMResult
exec = use (wrapvm.EVM.result) >>= \case
    Nothing -> let
      fwrapvm vm2 (a, vm) = (a, WrapVM vm vm2)
      stateRunner (WrapVM vm vm2) = (fwrapvm vm2) $ runState exec1 vm
      in State.state stateRunner >> exec
    Just x  -> return x


-- | Interpreter which explores all paths at
-- | branching points.
-- | returns a list of possible final evm states
interpret :: Fetch.Fetcher -> Maybe Integer -> StateT WrapVM Query (Tree BranchInfo)
interpret fetcher maxIter =
  exec >>= \case
   VMFailure (EVM.Query q) ->
     let performQuery = do wvm <- get
                           liftIO (fetcher q) >>= embed wvm
                           interpret fetcher maxIter
     in case q of
       PleaseAskSMT _ _ continue -> do
         wvm <- get
         codelocation <- getCodeLocation <$> use wrapvm
         use (wrapvm . iterations . at codelocation) >>= \case
           -- if this is the first time we are branching at this point,
           -- explore both branches without consulting SMT.
           -- Exploring too many branches is a lot cheaper than
           -- consulting our SMT solver.
           Nothing -> embed wvm (continue EVM.Unknown) >> interpret fetcher maxIter
           _ -> performQuery
       _ -> performQuery
   VMFailure (Choose (EVM.PleaseChoosePath whiff continue)) -> do
     vm <- use wrapvm
     wvm <- get
     case maxIterationsReached vm maxIter of
       Nothing -> do push 1
                     a <- embed wvm (continue True) >> interpret fetcher maxIter
                     pop 1
                     put wvm
                     push 1
                     b <- embed wvm (continue False) >> interpret fetcher maxIter
                     pop 1
                     return $ Node (BranchInfo { _vm = vm, _branchCondition = Just whiff}) [a, b]
       Just n -> embed wvm (continue (not n)) >> interpret fetcher maxIter
   _ -> do vm <- use wrapvm
           return $ Node BranchInfo {_vm = vm, _branchCondition = Nothing } []
  where embed :: WrapVM -> StateT VM Identity () -> StateT WrapVM Query ()
        embed (WrapVM _ oldvm) (StateT runStateT) = StateT runWrapStateT
          where runWrapStateT :: WrapVM -> QueryT IO ((), WrapVM)
                runWrapStateT (WrapVM vm _) = pure (x, WrapVM vm' oldvm)
                  where (x, vm') = runIdentity $ runStateT vm

maxIterationsReached :: VM -> Maybe Integer -> Maybe Bool
maxIterationsReached _ Nothing = Nothing
maxIterationsReached vm (Just maxIter) =
  let codelocation = getCodeLocation vm
      iters = view (iterations . at codelocation . non 0) vm
  in if num maxIter <= iters
     then view (cache . path . at (codelocation, iters - 1)) vm
     else Nothing

type Precondition = VM -> SBool
type Postcondition = (VM, VM) -> SBool

checkAssert :: ByteString -> Maybe (Text, [AbiType]) -> [String] -> Query (Either (VM, Tree BranchInfo) VM)
checkAssert c signature' concreteArgs = verifyContract c signature' concreteArgs SymbolicS (const sTrue) (Just checkAssertions)

checkAssertions :: Postcondition
checkAssertions (_, out) = case view result out of
  Just (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) -> sFalse
  _ -> sTrue

verifyContract :: ByteString -> Maybe (Text, [AbiType]) -> [String] -> StorageModel -> Precondition -> Maybe Postcondition -> Query (Either (VM, Tree BranchInfo) VM)
verifyContract theCode signature' concreteArgs storagemodel pre maybepost = do
    preStateRaw <- abstractVM signature' concreteArgs theCode  storagemodel
    -- add the pre condition to the pathconditions to ensure that we are only exploring valid paths
    let preState = over constraints ((++) [(pre preStateRaw, Dull)]) preStateRaw
    verify preState Nothing Nothing maybepost

-- pruneDeadPaths :: Tree VM -> Tree VM
-- pruneDeadPaths =
--   filter $ \vm -> case view result vm of
--     Just (VMFailure DeadPath) -> False
--     _ -> True

leaves :: Tree BranchInfo -> [VM]
leaves (Node x []) = [_vm x]
leaves (Node _ xs) = concatMap leaves xs

-- | Symbolically execute the VM and check all endstates against the postcondition, if available.
-- Returns `Right VM` if the postcondition can be violated, where `VM` is a prestate counterexample,
-- or `Left (VM, [VM])`, a pair of `prestate` and post vm states.
-- TODO: check for inconsistency
verify :: VM -> Maybe Integer -> Maybe (Fetch.BlockNumber, Text) -> Maybe Postcondition -> Query (Either (VM, Tree BranchInfo) VM)
verify preState maxIter rpcinfo maybepost = do
  let model = view (env . storageModel) preState
  smtState <- queryState
  tree <- fst <$> runStateT (interpret (Fetch.oracle (Just smtState) rpcinfo model False) maxIter) (WrapVM preState preState)
  case maybepost of
    (Just post) -> do
      let livePaths = leaves tree
      -- can also do these queries individually (even concurrently!). Could save time and report multiple violations
          postC = sOr $ fmap (\postState -> (sAnd (fst <$> view constraints postState)) .&& sNot (post (preState, postState))) livePaths
      -- is there any path which can possibly violate
      -- the postcondition?
      resetAssertions
      constrain postC
      io $ putStrLn "checking postcondition..."
      checkSat >>= \case
        Unk -> do io $ putStrLn "postcondition query timed out"
                  return $ Left (preState, tree)
        Unsat -> do io $ putStrLn "Q.E.D."
                    return $ Left (preState, tree)
        Sat -> return $ Right preState

    Nothing -> do io $ putStrLn "Nothing to check"
                  return $ Left (preState, tree)

-- | Compares two contract runtimes for trace equivalence by running two VMs and comparing the end states.
equivalenceCheck :: ByteString -> ByteString -> Maybe Integer -> Maybe (Text, [AbiType]) -> Query (Either ([VM], [VM]) VM)
equivalenceCheck bytecodeA bytecodeB maxiter signature' = do
  preStateA <- abstractVM signature' [] bytecodeA SymbolicS

  let preself = preStateA ^. state . contract
      precaller = preStateA ^. state . caller
      callvalue' = preStateA ^. state . callvalue
      prestorage = preStateA ^?! env . contracts . ix preself . storage
      (calldata', cdlen) = view (state . calldata) preStateA
      pathconds = view constraints preStateA
      preStateB = loadSymVM (RuntimeCode bytecodeB) prestorage SymbolicS precaller callvalue' (calldata', cdlen) & set constraints pathconds

  smtState <- queryState
  push 1
  aVMs <- fst <$> runStateT (interpret (Fetch.oracle (Just smtState) Nothing SymbolicS False) maxiter) (WrapVM preStateA preStateA)
  pop 1
  push 1
  bVMs <- fst <$> runStateT (interpret (Fetch.oracle (Just smtState) Nothing SymbolicS False) maxiter) (WrapVM preStateB preStateB)
  pop 1
  -- Check each pair of endstates for equality:
  let differingEndStates = uncurry distinct <$> [(a,b) | a <- leaves aVMs, b <- leaves bVMs]
      distinct a b =
        let (aPath, bPath) = both' (view constraints) (a, b)
            (aSelf, bSelf) = both' (view (state . contract)) (a, b)
            (aEnv, bEnv) = both' (view (env . contracts)) (a, b)
            (aResult, bResult) = both' (view result) (a, b)
            (Symbolic aStorage, Symbolic bStorage) = (view storage (aEnv ^?! ix aSelf), view storage (bEnv ^?! ix bSelf))
            differingResults = case (aResult, bResult) of

              (Just (VMSuccess aOut), Just (VMSuccess bOut)) ->
                aOut ./= bOut .|| aStorage ./= bStorage .|| fromBool (aSelf /= bSelf)

              (Just (VMFailure UnexpectedSymbolicArg), _) ->
                error $ "Unexpected symbolic argument at opcode: " <> maybe "??" show (vmOp a) <> ". Not supported (yet!)"

              (_, Just (VMFailure UnexpectedSymbolicArg)) ->
                error $ "Unexpected symbolic argument at opcode: " <> maybe "??" show (vmOp a) <> ". Not supported (yet!)"

              (Just (VMFailure _), Just (VMFailure _)) -> sFalse

              (Just _, Just _) -> sTrue

              _ -> error "Internal error during symbolic execution (should not be possible)"

        in sAnd (map fst aPath) .&& sAnd (map fst bPath) .&& differingResults
  -- If there exists a pair of endstates where this is not the case,
  -- the following constraint is satisfiable
  constrain $ sOr differingEndStates

  checkSat >>= \case
     Unk -> error "solver said unknown!"
     Sat -> return $ Right preStateA
     Unsat -> return $ Left (leaves aVMs, leaves bVMs)

both' :: (a -> b) -> (a, a) -> (b, b)
both' f (x, y) = (f x, f y)

showCounterexample :: VM -> Maybe (Text, [AbiType]) -> Query ()
showCounterexample vm maybesig = do
  let (calldata', cdlen) = view (EVM.state . EVM.calldata) vm
      S _ cvalue = view (EVM.state . EVM.callvalue) vm
      SAddr caller' = view (EVM.state . EVM.caller) vm
  cdlen' <- num <$> getValue cdlen
  calldatainput <- case calldata' of
    SymbolicBuffer cd -> mapM (getValue.fromSized) (take cdlen' cd) >>= return . pack
    ConcreteBuffer cd -> return $ BS.take cdlen' cd
  callvalue' <- num <$> getValue cvalue
  caller'' <- num <$> getValue caller'
  io $ do
    putStrLn "Calldata:"
    print $ ByteStringS calldatainput

    -- pretty print calldata input if signature is available
    case maybesig of
      Just (name, types) -> putStrLn $ unpack (head (splitOn "(" name)) ++
        show (decodeAbiValue (AbiTupleType (fromList types)) $ Lazy.fromStrict (BS.drop 4 calldatainput))
      Nothing -> return ()

    putStrLn "Caller:"
    print (Addr caller'')
    putStrLn "Callvalue:"
    print callvalue'
