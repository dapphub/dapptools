{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}

module EVM.SymExec where


import Prelude hiding (Word)

import Control.Lens hiding (pre)
import EVM hiding (Query, push)
import qualified EVM
import EVM.Exec
import qualified EVM.Fetch as Fetch
import EVM.ABI
import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational
import Control.Monad.State.Strict hiding (state)
import Data.Maybe (catMaybes)
import EVM.Types
import EVM.Concrete (createAddress)
import qualified EVM.FeeSchedule as FeeSchedule
import Data.SBV.Trans.Control
import Data.SBV.Trans hiding (distinct, Word)
import Data.SBV hiding (runSMT, newArray_, addAxiom, distinct, sWord8s, Word)
import Data.Vector (toList, fromList)
import Data.Tree

import Data.ByteString (ByteString, pack)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as BS
import Data.Text (Text, splitOn, unpack)
import Control.Monad.State.Strict (runState, get, put, zipWithM)
import qualified Control.Monad.State.Class as State
import Control.Applicative

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
symAbiArg :: AbiType -> Query ([SWord 8], SWord 256)
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
symCalldata :: Text -> [AbiType] -> [String] -> Query ([SWord 8], SWord 256)
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
    SymbolicS -> Symbolic [] <$> freshArray_ Nothing
    InitialS -> Symbolic [] <$> freshArray_ (Just 0)
    ConcreteS -> return $ Concrete mempty
  c <- SAddr <$> freshVar_
  value' <- sw256 <$> freshVar_
  return $ loadSymVM (RuntimeCode x) symstore storagemodel c value' (SymbolicBuffer cd', cdlen) & over constraints ((<>) [cdconstraint])

loadSymVM :: ContractCode -> Storage -> StorageModel -> SAddr -> SymWord -> (Buffer, SWord 256) -> VM
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

data BranchInfo = BranchInfo
  { _vm                 :: VM,
    _branchCondition    :: Maybe Whiff
  }

doInterpret :: Fetch.Fetcher -> Maybe Integer -> VM -> Query (Tree BranchInfo)
doInterpret fetcher maxIter vm = let
      f (vm', cs) = Node (BranchInfo (if length cs == 0 then vm' else vm) Nothing) cs
    in f <$> interpret' fetcher maxIter vm

interpret' :: Fetch.Fetcher -> Maybe Integer -> VM -> Query (VM, [(Tree BranchInfo)])
interpret' fetcher maxIter vm = let
  cont s = interpret' fetcher maxIter $ execState s vm
  in case view EVM.result vm of

    Nothing -> cont exec1

    Just (VMFailure (EVM.Query q@(PleaseAskSMT _ _ continue))) -> let
      codelocation = getCodeLocation vm
      location = view (iterations . at codelocation) vm
      in case location of
        Nothing -> cont $ continue EVM.Unknown
        Just _ -> io (fetcher q) >>= cont

    Just (VMFailure (EVM.Query q)) -> io (fetcher q) >>= cont

    Just (VMFailure (Choose (EVM.PleaseChoosePath whiff continue)))
      -> case maxIterationsReached vm maxIter of
        Nothing -> let
          lvm = execState (continue True) vm
          rvm = execState (continue False) vm
          in do
            push 1
            (leftvm, left) <- interpret' fetcher maxIter lvm
            pop 1
            push 1
            (rightvm, right) <- interpret' fetcher maxIter rvm
            pop 1
            return (vm, [Node (BranchInfo leftvm (Just whiff)) left, Node (BranchInfo rightvm (Just whiff)) right])
        Just n -> cont $ continue (not n)

    Just _
      -> return (vm, [])

-- | Interpreter which explores all paths at
-- | branching points.
-- | returns a list of possible final evm states
interpret
  :: Fetch.Fetcher
  -> Maybe Integer --max iterations
  -> Stepper a
  -> StateT VM Query [a]
interpret fetcher maxIter =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> StateT VM Query [a]

    eval (Operational.Return x) =
      pure [x]

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          exec >>= interpret fetcher maxIter . k
        Stepper.Run ->
          run >>= interpret fetcher maxIter . k
        Stepper.Ask (EVM.PleaseChoosePath _ continue) -> do
          vm <- get
          case maxIterationsReached vm maxIter of
            Nothing -> do push 1
                          a <- interpret fetcher maxIter (Stepper.evm (continue True) >>= k)
                          put vm
                          pop 1
                          push 1
                          b <- interpret fetcher maxIter (Stepper.evm (continue False) >>= k)
                          pop 1
                          return $ a <> b
            Just n -> interpret fetcher maxIter (Stepper.evm (continue (not n)) >>= k)
        Stepper.Wait q -> do
          let performQuery =
                do m <- liftIO (fetcher q)
                   interpret fetcher maxIter (Stepper.evm m >>= k)

          case q of
            PleaseAskSMT _ _ continue -> do
              codelocation <- getCodeLocation <$> get
              iters <- use (iterations . at codelocation)
              case iters of
                -- if this is the first time we are branching at this point,
                -- explore both branches without consulting SMT.
                -- Exploring too many branches is a lot cheaper than
                -- consulting our SMT solver.
                Nothing -> interpret fetcher maxIter (Stepper.evm (continue EVM.Unknown) >>= k)
                _ -> performQuery
            _ -> performQuery

        Stepper.EVM m ->
          State.state (runState m) >>= interpret fetcher maxIter . k

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

checkAssert :: ByteString -> Maybe (Text, [AbiType]) -> [String] -> Query (Either (Tree BranchInfo) (Tree BranchInfo), VM)
checkAssert c signature' concreteArgs = verifyContract c signature' concreteArgs SymbolicS (const sTrue) (Just checkAssertions)

checkAssertions :: Postcondition
checkAssertions (_, out) = case view result out of
  Just (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) -> sFalse
  _ -> sTrue

verifyContract :: ByteString -> Maybe (Text, [AbiType]) -> [String] -> StorageModel -> Precondition -> Maybe Postcondition -> Query (Either (Tree BranchInfo) (Tree BranchInfo), VM)
verifyContract theCode signature' concreteArgs storagemodel pre maybepost = do
    preStateRaw <- abstractVM signature' concreteArgs theCode  storagemodel
    -- add the pre condition to the pathconditions to ensure that we are only exploring valid paths
    let preState = over constraints ((++) [(pre preStateRaw, Dull)]) preStateRaw
    v <- verify preState Nothing Nothing maybepost
    return (v, preState)

pruneDeadPaths :: [VM] -> [VM]
pruneDeadPaths =
  filter $ \vm -> case view result vm of
    Just (VMFailure DeadPath) -> False
    _ -> True

consistentPath :: VM -> Query (Maybe VM)
consistentPath vm = do
  resetAssertions
  constrain $ sAnd $ fst <$> view constraints vm
  checkSat >>= \case
    Sat -> return $ Just vm
    Unk -> return $ Just vm -- the path may still be consistent
    Unsat -> return Nothing

consistentTree :: Tree BranchInfo -> Query (Maybe (Tree BranchInfo))
consistentTree (Node (BranchInfo vm w) []) = do
  consistentPath vm >>= \case
    Nothing  -> return Nothing
    Just vm' -> return $ Just $ Node (BranchInfo vm' w) []
consistentTree (Node b xs) = do
  consistentChildren <- catMaybes <$> forM xs consistentTree
  if null consistentChildren then
    return Nothing
  else
    return $ Just (Node b consistentChildren)


leaves :: Tree BranchInfo -> [VM]
leaves (Node x []) = [_vm x]
leaves (Node _ xs) = concatMap leaves xs

-- | Symbolically execute the VM and check all endstates against the postcondition, if available.
-- Returns `Right (Tree BranchInfo)` if the postcondition can be violated, or
-- or `Left (Tree BranchInfo)`, if the postcondition holds for all endstates.
verify :: VM -> Maybe Integer -> Maybe (Fetch.BlockNumber, Text) -> Maybe Postcondition -> Query (Either (Tree BranchInfo) (Tree BranchInfo))
verify preState maxIter rpcinfo maybepost = do
  smtState <- queryState
  tree <- doInterpret (Fetch.oracle (Just smtState) rpcinfo False) maxIter preState
  case maybepost of
    (Just post) -> do
      let livePaths = pruneDeadPaths $ leaves tree
      -- can also do these queries individually (even concurrently!). Could save time and report multiple violations
          postC = sOr $ fmap (\postState -> (sAnd (fst <$> view constraints postState)) .&& sNot (post (preState, postState))) livePaths
      -- is there any path which can possibly violate
      -- the postcondition?
      resetAssertions
      constrain postC
      io $ putStrLn "checking postcondition..."
      checkSat >>= \case
        Unk -> do io $ putStrLn "postcondition query timed out"
                  return $ Left tree
        Unsat -> do io $ putStrLn "Q.E.D."
                    return $ Left tree
        Sat -> return $ Right tree

    Nothing -> do io $ putStrLn "Nothing to check"
                  return $ Left tree

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
  aVMs <- doInterpret (Fetch.oracle (Just smtState) Nothing False) maxiter preStateA
  pop 1
  push 1
  bVMs <- doInterpret (Fetch.oracle (Just smtState) Nothing False) maxiter preStateB
  pop 1
  -- Check each pair of endstates for equality:
  let differingEndStates = uncurry distinct <$> [(a,b) | a <- pruneDeadPaths (leaves aVMs), b <- pruneDeadPaths (leaves bVMs)]
      distinct a b =
        let (aPath, bPath) = both' (view constraints) (a, b)
            (aSelf, bSelf) = both' (view (state . contract)) (a, b)
            (aEnv, bEnv) = both' (view (env . contracts)) (a, b)
            (aResult, bResult) = both' (view result) (a, b)
            (Symbolic _ aStorage, Symbolic _ bStorage) = (view storage (aEnv ^?! ix aSelf), view storage (bEnv ^?! ix bSelf))
            differingResults = case (aResult, bResult) of

              (Just (VMSuccess aOut), Just (VMSuccess bOut)) ->
                aOut ./= bOut .|| aStorage ./= bStorage .|| fromBool (aSelf /= bSelf)

              (Just (VMFailure UnexpectedSymbolicArg), _) ->
                error $ "Unexpected symbolic argument at opcode: " <> maybe "??" show (vmOp a) <> ". Not supported (yet!)"

              (_, Just (VMFailure UnexpectedSymbolicArg)) ->
                error $ "Unexpected symbolic argument at opcode: " <> maybe "??" show (vmOp a) <> ". Not supported (yet!)"

              (Just (VMFailure _), Just (VMFailure _)) -> sFalse

              (Just _, Just _) -> sTrue

              errormsg -> error $ show errormsg

        in sAnd (fst <$> aPath) .&& sAnd (fst <$> bPath) .&& differingResults
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
