{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}

module EVM.SymExec where

import Control.Lens hiding (pre)
import EVM hiding (Query, push)
import qualified EVM as EVM
import EVM.Exec
import Options.Generic as Options
import qualified EVM.Fetch as Fetch
import EVM.ABI
import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational
import EVM.Types
import EVM.Solidity
import EVM.Symbolic (litBytes)
import EVM.Concrete (createAddress)
import qualified EVM.FeeSchedule as FeeSchedule
import Data.SBV.Trans.Control
import Data.SBV.Trans hiding (distinct)
import Data.SBV hiding (runSMT, newArray_, addAxiom, distinct)
import Data.Vector (toList, fromList)

import Control.Monad.IO.Class
import qualified Control.Monad.State.Class as State
import Data.ByteString (ByteString, pack)
import Data.Text (Text)
import Control.Monad.State.Strict (runStateT, runState, StateT, get, put, zipWithM)
import Control.Applicative
import Data.Maybe (fromJust)

-- | Convenience functions for generating large symbolic byte strings
sbytes32, sbytes64, sbytes128, sbytes256, sbytes512, sbytes1024 :: Query ([SWord 8])
sbytes32 =  toBytes <$> freshVar_ @ (WordN 256)
sbytes64 = liftA2 (<>) sbytes32 sbytes32
sbytes128 = liftA2 (<>) sbytes64 sbytes64
sbytes256 = liftA2 (<>) sbytes128 sbytes128
sbytes512 = liftA2 (<>) sbytes256 sbytes256
sbytes1024 = liftA2 (<>) sbytes512 sbytes512

-- | Abstract calldata argument generation
-- We don't assume input types are restricted to their proper range here;
-- such assumptions should instead be given as preconditions.
-- This could catch some interesting calldata mismanagement errors.
symAbiArg :: AbiType -> Query ([SWord 8], SWord 32)
symAbiArg (AbiUIntType n) | n `mod` 8 == 0 && n <= 256 = do x <- freshVar_ @ (WordN 256)
                                                            return (toBytes x, 32)
                          | otherwise = error "bad type"

symAbiArg (AbiIntType n)  | n `mod` 8 == 0 && n <= 256 = do x <- freshVar_ @ (WordN 256)
                                                            return (toBytes x, 32)
                          | otherwise = error "bad type"

symAbiArg AbiAddressType = do x <- freshVar_ @ (WordN 256)
                              return (toBytes x, 32)

symAbiArg (AbiBytesType n) | n <= 32 = do x <- freshVar_ @ (WordN 256)
                                          return (toBytes x, 32)
                           | otherwise = error "bad type"

-- TODO: is this encoding correct?
symAbiArg (AbiArrayType len typ) = do args <- mapM symAbiArg (replicate len typ)
                                      return (litBytes (encodeAbiValue (AbiUInt 256 (fromIntegral len))) <> (concat $ fst <$> args), 32 + (sum $ snd <$> args))

symAbiArg (AbiTupleType tuple) = do args <- mapM symAbiArg (toList tuple)
                                    return (concat $ fst <$> args, sum $ snd <$> args)
symAbiArg n = error $ "Unsupported symbolic abiencoding for" <> show n <> ". Please file an issue at https://github.com/dapphub/dapptools if you really need this."

contractWithStore :: ContractCode -> Storage -> Contract
contractWithStore theContractCode store = initialContract theContractCode & set storage store

-- | Generates calldata matching given type signature, optionally specialized
-- with concrete arguments.
-- Any argument given as "<symbolic>" or omitted at the tail of the list are
-- kept symbolic.
symCalldata :: Text -> [AbiType] -> [String] -> Query ([SWord 8], SWord 32)
symCalldata sig' typesignature concreteArgs =
  let args = concreteArgs <> replicate (length typesignature - length concreteArgs)  "<symbolic>"
      mkArg typ "<symbolic>" = symAbiArg typ
      mkArg typ arg = let n = litBytes . encodeAbiValue $ makeAbiValue typ arg
                      in return (n, num (length n))
      selector = litBytes $ sig sig'
  in do calldatas <- zipWithM mkArg typesignature args
        return (selector <> concat (fst <$> calldatas), 4 + (sum $ snd <$> calldatas))

abstractVM :: Maybe (Text, [AbiType]) -> [String] -> ByteString -> StorageModel -> Query VM
abstractVM typesignature concreteArgs x storagemodel = do
  (cd', cdlen, cdconstraint) <-
    case typesignature of
      Nothing -> do cd <- sbytes256
                    len <- freshVar_
                    return (cd, len, len .<= 1024)
      Just (name, typs) -> do (cd, cdlen) <- symCalldata name typs concreteArgs
                              return (cd, cdlen, sTrue)
  symstore <- case storagemodel of
    SymbolicS -> Symbolic <$> freshArray_ Nothing
    InitialS -> Symbolic <$> freshArray_ (Just 0)
    ConcreteS -> return $ Concrete mempty
  c <- SAddr <$> freshVar_
  return $ loadSymVM (RuntimeCode x) symstore c (cd', cdlen) & over pathConditions ((<>) [cdconstraint])

loadSymVM :: ContractCode -> Storage -> SAddr -> ([SWord 8], SWord 32) -> VM
loadSymVM x initStore addr calldata' =
    (makeVm $ VMOpts
    { vmoptContract = contractWithStore x initStore
    , vmoptCalldata = calldata'
    , vmoptValue = 0
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
    , vmoptCreate = False
    }) & set (env . contracts . at (createAddress ethrunAddress 1))
             (Just (contractWithStore x initStore))

-- Interpreter which explores all paths at
-- branching points.
-- returns a list of possible final evm states
interpret
  :: (EVM.Query -> IO (EVM ()))
  -> Maybe Integer --max iterations
  -> Stepper a
  -> StateT VM IO (Either Stepper.Failure [a])
interpret fetcher maxIter =
  eval . Operational.view

  where
    eval
      :: Operational.ProgramView Stepper.Action a
      -> StateT VM IO (Either Stepper.Failure [a])

    eval (Operational.Return x) =
      pure (Right [x])

    eval (action Operational.:>>= k) =
      case action of
        Stepper.Exec ->
          exec >>= interpret fetcher maxIter . k
        Stepper.Run ->
          run >>= interpret fetcher maxIter . k
        Stepper.Option (EVM.PleaseChoosePath continue) ->
          do vm <- get
             case maxIter of
               Just maxiter ->
                 let pc' = view (state . pc) vm
                     addr = view (state . contract) vm
                     iters = view (iterations . at (addr, pc') . non 0) vm
                 in if num maxiter <= iters then
                      vm ^?! (cache . path . ix ((addr, pc'), iters - 1)) & \case
                        -- When we have reached maxIterations, we take the choice that will hopefully
                        -- lead us out of here.
                        Known 0 -> State.state (runState (continue 1)) >> interpret fetcher maxIter (k ())
                        Known 1 -> State.state (runState (continue 0)) >> interpret fetcher maxIter (k ())
                        n -> error ("I don't see how this could have happened: " <> show n)
                    else do a <- State.state (runState (continue 0)) >> interpret fetcher maxIter (k ())
                            put vm
                            b <- State.state (runState (continue 1)) >> interpret fetcher maxIter (k ())
                            return $ liftA2 (<>) a b
               Nothing -> do a <- State.state (runState (continue 0)) >> interpret fetcher maxIter (k ())
                             put vm
                             b <- State.state (runState (continue 1)) >> interpret fetcher maxIter (k ())
                             return $ liftA2 (<>) a b
        Stepper.Wait q ->
          do m <- liftIO (fetcher q)
             State.state (runState m) >> interpret fetcher maxIter (k ())
        Stepper.Note _ ->
          -- simply ignore the note here
          interpret fetcher maxIter (k ())
        Stepper.Fail e ->
          pure (Left e)
        Stepper.EVM m ->
          State.state (runState m) >>= interpret fetcher maxIter . k

type Precondition = VM -> SBool
type Postcondition = (VM, VM) -> SBool

checkAssert :: ContractCode -> Maybe Integer -> Maybe (Text, [AbiType]) -> [String] -> Query (Either (VM, [VM]) ByteString)
checkAssert c maxIter signature' concreteArgs = verifyContract c maxIter signature' concreteArgs SymbolicS (const sTrue) (Just checkAssertions)

checkAssertions :: Postcondition
checkAssertions (_, out) = case view result out of
  Just (EVM.VMFailure (EVM.UnrecognizedOpcode 254)) -> sFalse
  _ -> sTrue

data StorageModel = ConcreteS | SymbolicS | InitialS
  deriving (Read, Show)

instance ParseField StorageModel

verifyContract :: ContractCode -> Maybe Integer -> Maybe (Text, [AbiType]) -> [String] -> StorageModel -> Precondition -> Maybe Postcondition -> Query (Either (VM, [VM]) ByteString)
verifyContract code' maxIter signature' concreteArgs storagemodel pre maybepost = do
    preStateRaw <- abstractVM signature' concreteArgs theCode  storagemodel
    -- add the pre condition to the pathconditions to ensure that we are only exploring valid paths
    let preState = over pathConditions ((++) [pre preStateRaw]) preStateRaw
    verify preState maxIter maybepost
  where theCode = case code' of
          InitCode b    -> b
          RuntimeCode b -> b

verify :: VM -> Maybe Integer -> Maybe Postcondition -> Query (Either (VM, [VM]) ByteString)
verify preState maxIter maybepost = do
  smtState <- queryState
  results <- io $ fst <$> runStateT (interpret (Fetch.oracle smtState Nothing) maxIter Stepper.runFully) preState
  case (maybepost, results) of
    (Just post, Right res) -> do
      let postC = sOr $ fmap (\postState -> (sAnd (view pathConditions postState)) .&& sNot (post (preState, postState))) res
                  -- is there any path which can possibly violate
                  -- the postcondition?
      resetAssertions
      constrain postC
      io $ putStrLn "checking postcondition..."
      checkSat >>= \case
        Unk -> do io $ putStrLn "postcondition query timed out"
                  return $ Left (preState, res)
        Unsat -> do io $ putStrLn "Q.E.D"
                    return $ Left (preState, res)
        Sat -> do io $ putStrLn "post condition violated:"
                  let (calldata', cdlen') = view (state . calldata) preState
                  cdlen <- num <$> getValue cdlen'
                  model <- mapM (getValue.fromSized) (take cdlen calldata')
                  return $ Right (pack model)

    (Nothing, Right res) -> do io $ putStrLn "Q.E.D"
                               return $ Left (preState, res)

    (_, Left _) -> error "unexpected error during symbolic execution"

equivalenceCheck :: ByteString -> ByteString -> Maybe Integer -> Maybe (Text, [AbiType]) -> Query (Either ([VM], [VM]) ByteString)
equivalenceCheck bytecodeA bytecodeB maxiter signature' = do
  preStateA <- abstractVM signature' [] bytecodeA SymbolicS
           
  let preself = preStateA ^. state . contract
      precaller = preStateA ^. state . caller
      prestorage = preStateA ^?! env . contracts . ix preself . storage
      (calldata', cdlen) = view (state . calldata) preStateA
      preStateB = loadSymVM (RuntimeCode bytecodeB) prestorage precaller (calldata', cdlen)

  smtState <- queryState
  (aRes, bRes) <- both (\x -> io $ fst <$> runStateT (interpret (Fetch.oracle smtState Nothing) maxiter Stepper.runFully) x)
    (preStateA, preStateB)
  case (aRes, bRes) of
    (Left errA, Right _) -> error $ "A Failed: " <> show errA
    (Right _, Left errB) -> error $ "B Failed: " <> show errB
    (Left errA, Left errB) -> error $ "A Failed: " <> show errA <> "\nand B Failed:" <> show errB
    (Right aVMs, Right bVMs) -> do
      let differingEndStates = zipWith distinct aVMs bVMs
          distinct a b = let (aPath, bPath) = both' (view pathConditions) (a, b)
                             (aSelf, bSelf) = both' (view (state . contract)) (a, b)
                             (aEnv, bEnv) = both' (view (env . contracts)) (a, b)
                             (aResult, bResult) = both' (view result) (a, b)
                             (Symbolic aStorage, Symbolic bStorage) = (view storage (aEnv ^?! ix aSelf), view storage (bEnv ^?! ix bSelf))
                             differingResults = case (aResult, bResult) of
                               (Just (VMSuccess aOut), Just (VMSuccess bOut)) -> aOut ./= bOut .|| aStorage ./= bStorage .|| fromBool (aSelf /= bSelf)
                               (Just (VMFailure _), Just (VMFailure _)) -> sFalse
                               (Just _, Just _) -> sTrue
                               _ -> error "Internal error during symbolic execution (should not be possible)"
                         in sAnd aPath .&& sAnd bPath .&& differingResults
      -- If there exists a pair of endstates where this is not the case,
      -- the following constraint is satisfiable
      resetAssertions
      constrain $ sOr differingEndStates

      checkSat >>= \case
         Unk -> error "solver said unknown!"
         Sat -> do cdlen' <- num <$> getValue cdlen
                   model <- mapM (getValue.fromSized) (take cdlen' calldata')
                   return (Right (pack model))
         Unsat -> return $ Left (aVMs, bVMs)

both' :: (a -> b) -> (a, a) -> (b, b)
both' f (x, y) = (f x, f y)
