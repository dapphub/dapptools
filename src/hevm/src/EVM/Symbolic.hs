{-# Language GADTs #-}
{-# Language NamedFieldPuns #-}
{-# Language DataKinds #-}
{-# Language OverloadedStrings #-}

module EVM.Symbolic where

import Control.Lens
import EVM hiding (Query)
import qualified EVM as EVM
import EVM.Exec
import EVM.Op
import qualified EVM.Fetch as Fetch
import EVM.ABI
import EVM.Stepper (Stepper)
import qualified EVM.Stepper as Stepper
import qualified Control.Monad.Operational as Operational
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
import Control.Monad.State.Strict (runStateT, runState, execState, unless, StateT, forM, put)
import Control.Applicative
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
symAbiArg (AbiUIntType 8) = do x <- freshVar_
                               return $ truncpad 32 $ toBytes (x :: SWord 8)
symAbiArg (AbiUIntType 16) = do x <- freshVar_
                                return $ truncpad 32 $ toBytes (x :: SWord 16)
symAbiArg (AbiUIntType 32) = do x <- freshVar_
                                return $ truncpad 32 $ toBytes (x :: SWord 32)
symAbiArg (AbiUIntType 256) = do x <- freshVar_
                                 return $ truncpad 32 $ toBytes (x :: SWord 256)
symAbiArg (AbiUIntType n) | n `mod` 8 == 0  = error "todo"
symAbiArg (AbiIntType n) | otherwise        = error "bad type"

symAbiArg (AbiIntType 8) = do x <- freshVar_
                              return $ truncpad 32 $ toBytes (x :: SWord 8)
symAbiArg (AbiIntType 16) = do x <- freshVar_
                               return $ truncpad 32 $ toBytes (x :: SWord 16)
symAbiArg (AbiIntType 32) = do x <- freshVar_
                               return $ truncpad 32 $ toBytes (x :: SWord 32)
symAbiArg (AbiIntType 256) = do x <- freshVar_
                                return $ truncpad 32 $ toBytes (x :: SWord 256)
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

-- Interpreter which explores all paths at
-- branching points.
-- returns a list of possible final evm states
interpret
  :: (EVM.Query -> IO (EVM ()))
  -> Stepper a
  -> StateT VM IO (Either Stepper.Failure [a])
interpret fetcher =
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
          exec >>= interpret fetcher . k
        Stepper.Run ->
          run >>= interpret fetcher . k
        Stepper.Option (EVM.PleaseChoosePath continue) ->
          do vm <- State.get
             a <- State.state (runState (continue 0)) >> interpret fetcher (k ())
             put vm
             b <- State.state (runState (continue 1)) >> interpret fetcher (k ())
             return $ liftA2 (<>) a b
        Stepper.Wait q ->
          do
             m <- liftIO (fetcher q)
             State.state (runState m) >> interpret fetcher (k ())
        Stepper.Note _ ->
          -- simply ignore the note here
          interpret fetcher (k ())
        Stepper.Fail e ->
          pure (Left e)
        Stepper.EVM m ->
          State.state (runState m) >>= interpret fetcher . k

type Precondition = [SWord 8] -> SBool
type Postcondition = (VM, VM) -> SBool

verify :: ContractCode -> Text -> Precondition -> Maybe Postcondition -> Query (Either () AbiValue)
verify (RuntimeCode runtimecode) signature' pre maybepost = do
  let Just types = (parseFunArgs signature')
  input <- symAbiArg types
  let calldata' = litBytes (sig signature') <> input
  symstore <- freshArray_ Nothing
  let preState = (loadSymVM runtimecode symstore calldata') & set pathConditions [pre input]
  smtState <- queryState
  results <- io $ fst <$> runStateT (interpret (Fetch.oracle smtState) Stepper.runFully) preState
  case (maybepost, results) of
    (Just post, Right res) -> do let postC = sOr $ fmap (\postState -> (sAnd (view pathConditions postState)) .&& sNot (post (preState, postState))) res
                                 resetAssertions
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
    (Nothing, Right _) -> do io $ print "Q.E.D"
                             return $ Left ()
    (Nothing, Left _) -> error "unexpected error during symbolic execution"
