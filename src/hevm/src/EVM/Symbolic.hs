{-# Language GADTs #-}
{-# Language NamedFieldPuns #-}
{-# Language DataKinds #-}

module EVM.Symbolic where

import Control.Lens
import EVM hiding (Query)
import EVM.Exec
import EVM.Op
import EVM.ABI
import EVM.Types
import EVM.Format
import EVM.Keccak
import EVM.Solidity
import EVM.Concrete (createAddress, SymWord(..))
import qualified EVM.FeeSchedule as FeeSchedule
import Data.SBV.Trans.Control hiding (sat)
import Data.SBV.Trans
import Data.SBV hiding (runSMT, newArray_)

import Control.Monad.IO.Class
import GHC.TypeNats
import Data.Vector        (Vector, toList)
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as State
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.ByteString (ByteString, pack, null)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Binary.Put    (runPut)
import Data.Binary.Get    (runGet)
import Control.Monad.State.Strict (runState, execState, unless)
import Data.Maybe (isJust, fromJust)

mkSymbolicContract :: ContractCode -> SArray (WordN 256) (WordN 256) -> Contract
mkSymbolicContract theContractCode store = Contract
  { _contractcode = theContractCode
  , _codehash =
    if BS.null theCode then 0 else
      keccak (stripBytecodeMetadata theCode)
  , _storage  = Symbolic store
  , _balance  = 0
  , _nonce    = 0
  , _opIxMap  = mkOpIxMap theCode
  , _codeOps  = mkCodeOps theCode
  , _external = False
  , _origStorage = mempty
  } where theCode = case theContractCode of
            InitCode b    -> b
            RuntimeCode b -> b

loadSymVM :: ByteString -> SArray (WordN 256) (WordN 256) -> [SWord 8] -> VM
loadSymVM x initStore calldata =
    (makeVm $ VMOpts
    { vmoptContract = mkSymbolicContract (RuntimeCode x) initStore
    , vmoptCalldata = calldata
    , vmoptValue = 0
    , vmoptAddress = createAddress ethrunAddress 1
    , vmoptCaller = ethrunAddress
    , vmoptOrigin = ethrunAddress
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
             (Just (mkSymbolicContract (RuntimeCode x) initStore))


loadVM :: ByteString -> Maybe VM
loadVM x =
    case runState exec (vmForEthrunCreation x) of
       (VMSuccess targetCode, vm1) -> do
         let target = view (state . contract) vm1
             vm2 = execState (replaceCodeOfSelf (RuntimeCode (forceLitBytes targetCode))) vm1
         return $ snd $ flip runState vm2
                (do resetState
                    assign (state . gas) 0xffffffffffffffff -- kludge
                    loadContract target)
       _ -> Nothing

symAbiArg :: AbiType -> Query [SWord 8]
--Gotta split these to ensure (SWord n) is well typed
symAbiArg (AbiUIntType 8) = do x <- freshVar_ :: Query (SWord 8)
                               return $ truncpad 32 $ toBytes x
symAbiArg (AbiUIntType 16) = do x <- freshVar_ :: Query (SWord 16)
                                return $ truncpad 32 $ toBytes x
symAbiArg (AbiUIntType 32) = do x <- freshVar_ :: Query (SWord 32)
                                return $ truncpad 32 $ toBytes x
symAbiArg (AbiUIntType 256) = do x <- freshVar_ :: Query (SWord 256)
                                 return $ truncpad 32 $ toBytes x
symAbiArg (AbiUIntType n) | n `mod` 8 == 0  = error "todo"
symAbiArg (AbiIntType n) | otherwise        = error "bad type"

symAbiArg (AbiIntType 8) = do x <- freshVar_ :: Query (SWord 8)
                              return $ truncpad 32 $ toBytes x
symAbiArg (AbiIntType 16) = do x <- freshVar_ :: Query (SWord 16)
                               return $ truncpad 32 $ toBytes x
symAbiArg (AbiIntType 32) = do x <- freshVar_ :: Query (SWord 32)
                               return $ truncpad 32 $ toBytes x
symAbiArg (AbiIntType 256) = do x <- freshVar_ :: Query (SWord 256)
                                return $ truncpad 32 $ toBytes x
symAbiArg (AbiIntType n) | n `mod` 8 == 0 = error "todo"
symAbiArg (AbiIntType n) | otherwise      = error "bad type (although)"

symAbiArg AbiAddressType = do x <- freshVar_ :: Query (SWord 128)
                              y <- freshVar_ :: Query (SWord 32)
                              return $ truncpad 32 $ toBytes x <> toBytes y

symAbiArg (AbiBytesType 1)  = do x <- freshVar_ :: Query (SWord 8)
                                 return $ truncpad 32 $ toBytes x
symAbiArg (AbiBytesType 4)  = do x <- freshVar_ :: Query (SWord 32)
                                 return $ truncpad 32 $ toBytes x
symAbiArg (AbiBytesType 16) = do x <- freshVar_ :: Query (SWord 64)
                                 return $ truncpad 32 $ toBytes x
symAbiArg (AbiBytesType 32) = do x <- freshVar_ :: Query (SWord 256)
                                 return $ truncpad 32 $ toBytes x

symAbiArg (AbiBytesType n) = error "todo"

symAbiArg (AbiArrayType len typ) = do args <- mapM symAbiArg (replicate len typ)
                                      return $ litBytes (encodeAbiValue (AbiUInt 256 (fromIntegral len))) <> (concat args)
symAbiArg (AbiTupleType tuple) = mapM symAbiArg (toList tuple) >>= return . concat
symAbiArg _ = error "todo"

symExec :: VM -> [SBool] -> Query [(VM, [SBool])]
symExec vm pathconds = do
  resetAssertions
  let branch = execState (execWhile isNotJump) vm
  case view result branch of
    Just x -> return [(branch, pathconds)]
    Nothing -> do
      io $ print $ "possible branching point at pc: " <> show (view (state.pc) branch)
      let Just cond = branch^.state.stack ^? ix 1
      constrain ((cond ./= 0) .&& sAnd pathconds)
      noJump <- checkSat --Assuming ((cond ./= 0):pathconds)
      case noJump of
        Unk   -> error "Solver said unknown!"
        Unsat -> -- the jump condition must be 0.
                 -- we change it to 0 on the stack;
                 -- step once and then recurse.
                 let vm1 = branch & state.stack.(ix 1) .~ 0
                 in do io $ print $ "but smt says jump condition is false"
                       symExec vm1 pathconds

        Sat   -> do
                 -- it's possible for the jump condition
                 -- to be nonzero. Can it also be zero?
                     resetAssertions
                     constrain ((cond .== 0) .&& sAnd pathconds)
                     jump <- checkSat
                     case jump of
                        Unk   -> error "Solver said unknown!"
                        Unsat -> -- no. The we must jump.
                                 let vm1 = branch & state.stack.(ix 1) .~ 1
                                 in do io $ print $ "but smt says jump condition is true"
                                       symExec vm1 pathconds
                        Sat -> -- We can either jump or not jump.
                               -- Explore both paths
                          let aVm = branch & state.stack.(ix 1) .~ 0
                              bVm = branch & state.stack.(ix 1) .~ 1
                          in do io $ print $ "and smt says both cases are possible"
                                aResult <- symExec aVm ((cond .== 0):pathconds)
                                bResult <- symExec bVm ((cond .== 1):pathconds)
                                return $ aResult <> bResult

type Precondition = [SWord 8] -> SBool
type Postcondition = (VM, VM) -> SBool

verify :: ContractCode -> Text -> Precondition -> Maybe Postcondition -> Query (Either () AbiValue)
verify (RuntimeCode runtimecode) signature' pre maybepost = do
  let Just types = (parseFunArgs signature')
  input <- symAbiArg types
  let calldata' = litBytes (sig signature') <> input
  symstore <- freshArray_ Nothing
  let preState = loadSymVM runtimecode symstore calldata'
  results <- symExec preState [pre input]
  case maybepost of
    Just post -> do let postC = sOr $ fmap (\(postState,pathc) -> (sAnd pathc) .&& sNot (post (preState, postState))) results
                    constrain postC
                    -- is it possible for any of these pathcondition => postcondition
                    -- implications to be false?
                    io $ print "checking postcondition..."
                    sat <- checkSat
                    case sat of
                      Unsat -> do io $ print "Q.E.D"
                                  return $ Left ()
                      Sat -> do io $ print "post condition violated:"
                                model <- mapM (getValue.fromSized) input
                                let inputArgs = decodeAbiValue types $ fromStrict (pack model)
                                return $ Right inputArgs
    Nothing -> do io $ print "Q.E.D"
                  return $ Left ()

isNotJump :: VM -> Bool
isNotJump vm = vmOp vm /= Just OpJumpi ||
  case vm^.state.stack ^? ix 1 of
    Just (S _ c) -> isConcrete c
    Nothing -> error "malformed stack"
