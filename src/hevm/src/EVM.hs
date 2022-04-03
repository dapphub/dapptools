{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleInstances #-}
{-# Language DataKinds #-}
{-# Language GADTs #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

module EVM where

import Prelude hiding (log, exponent, GT, LT)

import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import EVM.ABI
import EVM.Types
import EVM.Solidity
import EVM.Concrete (createAddress, create2Address)
import EVM.Op
import EVM.Expr (readStorage, writeStorage, readByte, readWord, writeWord, writeByte, bufLength, indexWord, litAddr, readBytes, word256At, copySlice, minLength)
import EVM.FeeSchedule (FeeSchedule (..))
import Options.Generic as Options
import qualified EVM.Precompiled
import qualified EVM.Expr as Expr

import Control.Lens hiding (op, (:<), (|>), (.>))
import Control.Monad.State.Strict hiding (state)

import Data.ByteString              (ByteString)
import Data.ByteString.Lazy         (fromStrict)
import Data.Map.Strict              (Map)
import Data.Set                     (Set, insert, member, fromList)
import Data.Maybe                   (fromMaybe)
import Data.Sequence                (Seq)
import Data.Vector.Storable         (Vector)
import Data.Foldable                (toList)
import Data.Word                    (Word8, Word32)
import Data.Bits                    (FiniteBits, countLeadingZeros, finiteBitSize)

import Data.Tree
import Data.Tuple.Curry
import Data.List (find)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteArray       as BA
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Data.Tree.Zipper     as Zipper
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as Vector

import qualified Data.Vector as RegularVector

import Crypto.Number.ModArithmetic (expFast)
import qualified Crypto.Hash as Crypto
import Crypto.Hash (Digest, SHA256, RIPEMD160, digestFromByteString)
import Crypto.PubKey.ECC.ECDSA (signDigestWith, PrivateKey(..), Signature(..))
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(..), Point(..))
import Crypto.PubKey.ECC.Generate (generateQ)

-- * Data types

-- | EVM failure modes
data Error
  = BalanceTooLow W256 W256
  | UnrecognizedOpcode Word8
  | SelfDestruction
  | StackUnderrun
  | BadJumpDestination
  | Revert (Expr Buf)
  | OutOfGas W256 W256
  | BadCheatCode (Maybe Word32)
  | StackLimitExceeded
  | IllegalOverflow
  | Query Query
  | Choose Choose
  | StateChangeWhileStatic
  | InvalidMemoryAccess
  | CallDepthLimitReached
  | MaxCodeSizeExceeded W256 W256
  | InvalidFormat
  | PrecompileFailure
  | UnexpectedSymbolicArg Int String
  | DeadPath
  | NotUnique (Expr EWord)
  | SMTTimeout
  | FFI [AbiValue]
deriving instance Show Error

-- | The possible result states of a VM
data VMResult
  = VMFailure Error -- ^ An operation failed
  | VMSuccess (Expr Buf) -- ^ Reached STOP, RETURN, or end-of-code

deriving instance Show VMResult

-- | The state of a stepwise EVM execution
data VM = VM
  { _result         :: Maybe VMResult
  , _state          :: FrameState
  , _frames         :: [Frame]
  , _env            :: Env
  , _block          :: Block
  , _tx             :: TxState
  , _logs           :: Expr Logs
  , _traces         :: Zipper.TreePos Zipper.Empty Trace
  , _cache          :: Cache
  , _burned         :: W256
  , _iterations     :: Map CodeLocation Int
  , _allowFFI       :: Bool
  }
  deriving (Show)

data Trace = Trace
  { _traceOpIx     :: Int
  , _traceContract :: Contract
  , _traceData     :: TraceData
  }
  deriving (Show)

data TraceData
  = EventTrace (Expr EWord) (Expr Buf) [Expr EWord]
  | FrameTrace FrameContext
  | QueryTrace Query
  | ErrorTrace Error
  | EntryTrace Text
  | ReturnTrace (Expr Buf) FrameContext
  deriving (Show)

-- | Queries halt execution until resolved through RPC calls or SMT queries
data Query where
  PleaseFetchContract :: Addr -> StorageModel -> (Contract -> EVM ()) -> Query
  --PleaseMakeUnique    :: SBV a -> [SBool] -> (IsUnique a -> EVM ()) -> Query
  PleaseFetchSlot     :: Addr -> W256 -> (W256 -> EVM ()) -> Query
  --PleaseAskSMT        :: SBool -> [SBool] -> (BranchCondition -> EVM ()) -> Query
  PleaseDoFFI         :: [String] -> (ByteString -> EVM ()) -> Query

data Choose where
  PleaseChoosePath    :: Expr EWord -> (Bool -> EVM ()) -> Choose

instance Show Query where
  showsPrec _ = \case
    PleaseFetchContract addr _ _ ->
      (("<EVM.Query: fetch contract " ++ show addr ++ ">") ++)
    PleaseFetchSlot addr slot _ ->
      (("<EVM.Query: fetch slot "
        ++ show slot ++ " for "
        ++ show addr ++ ">") ++)
--     PleaseAskSMT condition constraints _ ->
--       (("<EVM.Query: ask SMT about "
--         ++ show condition ++ " in context "
--         ++ show constraints ++ ">") ++)
--     PleaseMakeUnique val constraints _ ->
--       (("<EVM.Query: make value "
--         ++ show val ++ " unique in context "
--         ++ show constraints ++ ">") ++)
    PleaseDoFFI cmd _ ->
      (("<EVM.Query: do ffi: " ++ (show cmd)) ++)

instance Show Choose where
  showsPrec _ = \case
    PleaseChoosePath _ _ ->
      (("<EVM.Choice: waiting for user to select path (0,1)") ++)

-- | Alias for the type of e.g. @exec1@.
type EVM a = State VM a

type CodeLocation = (Addr, Int)

-- | The possible return values of a SMT query
data BranchCondition = Case Bool | Unknown | Inconsistent
  deriving Show

-- | The possible return values of a `is unique` SMT query
data IsUnique a = Unique a | Multiple | InconsistentU | TimeoutU
  deriving Show

-- | The cache is data that can be persisted for efficiency:
-- any expensive query that is constant at least within a block.
data Cache = Cache
  { _fetched :: Map Addr Contract,
    _path :: Map (CodeLocation, Int) Bool
  } deriving Show

-- | A way to specify an initial VM state
data VMOpts = VMOpts
  { vmoptContract :: Contract
  , vmoptCalldata :: Expr Buf
  , vmoptValue :: Expr EWord
  , vmoptPriorityFee :: W256
  , vmoptAddress :: Addr
  , vmoptCaller :: Expr EWord
  , vmoptOrigin :: Addr
  , vmoptGas :: W256
  , vmoptGaslimit :: W256
  , vmoptNumber :: W256
  , vmoptTimestamp :: Expr EWord
  , vmoptCoinbase :: Addr
  , vmoptDifficulty :: W256
  , vmoptMaxCodeSize :: W256
  , vmoptBlockGaslimit :: W256
  , vmoptGasprice :: W256
  , vmoptBaseFee :: W256
  , vmoptSchedule :: FeeSchedule Integer
  , vmoptChainId :: W256
  , vmoptCreate :: Bool
  , vmoptStorageModel :: StorageModel
  , vmoptTxAccessList :: Map Addr [W256]
  , vmoptAllowFFI :: Bool
  } deriving Show

-- | An entry in the VM's "call/create stack"
data Frame = Frame
  { _frameContext   :: FrameContext
  , _frameState     :: FrameState
  }
  deriving (Show)

-- | Call/create info
data FrameContext
  = CreationContext
    { creationContextAddress   :: Addr
    , creationContextCodehash  :: W256
    , creationContextReversion :: Map Addr Contract
    , creationContextSubstate  :: SubState
    }
  | CallContext
    { callContextTarget    :: Addr
    , callContextContext   :: Addr
    , callContextOffset    :: W256
    , callContextSize      :: W256
    , callContextCodehash  :: W256
    , callContextAbi       :: Maybe W256
    , callContextData      :: Expr Buf
    , callContextReversion :: Map Addr Contract
    , callContextSubState  :: SubState
    }
  deriving (Show)

-- | The "registers" of the VM along with memory and data stack
data FrameState = FrameState
  { _contract     :: Addr
  , _codeContract :: Addr
  , _code         :: Expr Buf
  , _pc           :: Int
  , _stack        :: [Expr EWord]
  , _memory       :: Expr Buf
  , _memorySize   :: Int
  , _calldata     :: Expr Buf
  , _callvalue    :: Expr EWord
  , _caller       :: Expr EWord
  , _gas          :: W256
  , _returndata   :: Expr Buf
  , _static       :: Bool
  }
  deriving (Show)

-- | The state that spans a whole transaction
data TxState = TxState
  { _gasprice            :: W256
  , _txgaslimit          :: W256
  , _txPriorityFee       :: W256
  , _origin              :: Addr
  , _toAddr              :: Addr
  , _value               :: Expr EWord
  , _substate            :: SubState
  , _isCreate            :: Bool
  , _txReversion         :: Map Addr Contract
  }
  deriving (Show)

-- | The "accrued substate" across a transaction
data SubState = SubState
  { _selfdestructs   :: [Addr]
  , _touchedAccounts :: [Addr]
  , _accessedAddresses :: Set Addr
  , _accessedStorageKeys :: Set (Addr, W256)
  , _refunds         :: [(Addr, Integer)]
  -- in principle we should include logs here, but do not for now
  }
  deriving (Show)

-- | A contract is either in creation (running its "constructor") or
-- post-creation, and code in these two modes is treated differently
-- by instructions like @EXTCODEHASH@, so we distinguish these two
-- code types.
data ContractCode
  = InitCode (Expr Buf)  -- ^ "Constructor" code, during contract creation
  | RuntimeCode (Expr Buf)           -- ^ "Instance" code, after contract creation
  deriving (Show)

  {-
      TODO: this is what Contractcode should look like actually. This reflects
      the structure of code output by solc where:

        - initcode has concrete code, followed by an abstract data "section"
        - runtimecode has a fixed length, but may contain fixed size symbolic regions (due to immutable)

      hopefully we do not have to deal with dynamic immutable before we get a real data section...

      data ContractCode
        = InitCode (BytesString, Expr Buf)  -- ^ "Constructor" code, during contract creation
        | RuntimeCode [Expr Byte]           -- ^ "Instance" code, after contract creation
        deriving (Show)

  -}

-- runtime err when used for symbolic code
instance Eq ContractCode where
  (InitCode x) == (InitCode y) = x == y
  (RuntimeCode x) == (RuntimeCode y) = x == y
  _ == _ = False

-- runtime err when used for symbolic code
instance Ord ContractCode where
  compare x y = compare (buf x) (buf y)
    where buf (InitCode z) = z
          buf (RuntimeCode z) = z

-- | A contract can either have concrete or symbolic storage
-- depending on what type of execution we are doing
-- data Storage
--   = Concrete (Map Word Expr EWord)
--   | Symbolic [(Expr EWord, Expr EWord)] (SArray (WordN 256) (WordN 256))
--   deriving (Show)

-- to allow for Eq Contract (which useful for debugging vmtests)
-- we mock an instance of Eq for symbolic storage.
-- It should not (cannot) be used though.
-- instance Eq Storage where
--   (==) (Concrete a) (Concrete b) = fmap forceLit a == fmap forceLit b
--   (==) (Symbolic _ _) (Concrete _) = False
--   (==) (Concrete _) (Symbolic _ _) = False
--   (==) _ _ = error "do not compare two symbolic arrays like this!"

-- | The state of a contract
data Contract = Contract
  { _contractcode :: ContractCode
  , _storage      :: Expr Storage
  , _balance      :: W256
  , _nonce        :: W256
  , _codehash     :: W256
  , _opIxMap      :: Vector Int
  , _codeOps      :: RegularVector.Vector (Int, Op)
  , _external     :: Bool
  , _origStorage  :: Map W256 W256
  }

deriving instance Show Contract

-- | When doing symbolic execution, we have three different
-- ways to model the storage of contracts. This determines
-- not only the initial contract storage model but also how
-- RPC or state fetched contracts will be modeled.
data StorageModel
  = ConcreteS    -- ^ Uses `Concrete` Storage. Reading / Writing from abstract
                 -- locations causes a runtime failure. Can be nicely combined with RPC.

  | SymbolicS    -- ^ Uses `Symbolic` Storage. Reading / Writing never reaches RPC,
                 -- but always done using an SMT array with no default value.

  | InitialS     -- ^ Uses `Symbolic` Storage. Reading / Writing never reaches RPC,
                 -- but always done using an SMT array with 0 as the default value.

  deriving (Read, Show)

instance ParseField StorageModel

-- | Various environmental data
data Env = Env
  { _contracts    :: Map Addr Contract
  , _chainId      :: W256
  , _storageModel :: StorageModel
  , _sha3Crack    :: Map W256 ByteString
  --, _keccakUsed   :: [([SWord 8], SWord 256)]
  }
  deriving (Show)


-- | Data about the block
data Block = Block
  { _coinbase    :: Addr
  , _timestamp   :: Expr EWord
  , _number      :: W256
  , _difficulty  :: W256
  , _gaslimit    :: W256
  , _baseFee     :: W256
  , _maxCodeSize :: W256
  , _schedule    :: FeeSchedule Integer
  } deriving Show

blankState :: FrameState
blankState = FrameState
  { _contract     = 0
  , _codeContract = 0
  , _code         = mempty
  , _pc           = 0
  , _stack        = mempty
  , _memory       = mempty
  , _memorySize   = 0
  , _calldata     = mempty
  , _callvalue    = 0
  , _caller       = 0
  , _gas          = 0
  , _returndata   = mempty
  , _static       = False
  }

makeLenses ''FrameState
makeLenses ''Frame
makeLenses ''Block
makeLenses ''TxState
makeLenses ''SubState
makeLenses ''Contract
makeLenses ''Env
makeLenses ''Cache
makeLenses ''Trace
makeLenses ''VM

-- | An "external" view of a contract's bytecode, appropriate for
-- e.g. @EXTCODEHASH@.
bytecode :: Getter Contract (Expr Buf)
bytecode = contractcode . to f
  where f (InitCode _)    = mempty
        f (RuntimeCode b) = b

instance Semigroup Cache where
  a <> b = Cache
    { _fetched = Map.unionWith unifyCachedContract (view fetched a) (view fetched b)
    , _path = mappend (view path a) (view path b)
    }

-- only intended for use in Cache merges, where we expect
-- everything to be Concrete
unifyCachedContract :: Contract -> Contract -> Contract
unifyCachedContract a b = a & set storage merged
  where merged = case (view storage a, view storage b) of
                   (ConcreteStore sa, ConcreteStore sb) ->
                     ConcreteStore (mappend sa sb)
                   _ ->
                     view storage a

instance Monoid Cache where
  mempty = Cache { _fetched = mempty,
                   _path = mempty
                 }

-- * Data accessors

currentContract :: VM -> Maybe Contract
currentContract vm =
  view (env . contracts . at (view (state . codeContract) vm)) vm

-- * Data constructors

makeVm :: VMOpts -> VM
makeVm o =
  let txaccessList = vmoptTxAccessList o
      txorigin = vmoptOrigin o
      txtoAddr = vmoptAddress o
      initialAccessedAddrs = fromList $ [txorigin, txtoAddr] ++ [1..9] ++ (Map.keys txaccessList)
      initialAccessedStorageKeys = fromList $ foldMap (uncurry (map . (,))) (Map.toList txaccessList)
      touched = if vmoptCreate o then [txorigin] else [txorigin, txtoAddr]
  in
  VM
  { _result = Nothing
  , _frames = mempty
  , _tx = TxState
    { _gasprice = vmoptGasprice o
    , _txgaslimit = vmoptGaslimit o
    , _txPriorityFee = vmoptPriorityFee o
    , _origin = txorigin
    , _toAddr = txtoAddr
    , _value = vmoptValue o
    , _substate = SubState mempty touched initialAccessedAddrs initialAccessedStorageKeys mempty
    --, _accessList = txaccessList
    , _isCreate = vmoptCreate o
    , _txReversion = Map.fromList
      [(vmoptAddress o, vmoptContract o)]
    }
  , _logs = mempty
  , _traces = Zipper.fromForest []
  , _block = Block
    { _coinbase = vmoptCoinbase o
    , _timestamp = vmoptTimestamp o
    , _number = vmoptNumber o
    , _difficulty = vmoptDifficulty o
    , _maxCodeSize = vmoptMaxCodeSize o
    , _gaslimit = vmoptBlockGaslimit o
    , _baseFee = vmoptBaseFee o
    , _schedule = vmoptSchedule o
    }
  , _state = FrameState
    { _pc = 0
    , _stack = mempty
    , _memory = mempty
    , _memorySize = 0
    , _code = theCode
    , _contract = vmoptAddress o
    , _codeContract = vmoptAddress o
    , _calldata = vmoptCalldata o
    , _callvalue = vmoptValue o
    , _caller = vmoptCaller o
    , _gas = vmoptGas o
    , _returndata = mempty
    , _static = False
    }
  , _env = Env
    { _sha3Crack = mempty
    , _chainId = vmoptChainId o
    , _contracts = Map.fromList
      [(vmoptAddress o, vmoptContract o)]
    --, _keccakUsed = mempty
    , _storageModel = vmoptStorageModel o
    }
  , _cache = Cache mempty mempty
  , _burned = 0
  --, _constraints = []
  , _iterations = mempty
  , _allowFFI = vmoptAllowFFI o
  } where theCode = case _contractcode (vmoptContract o) of
            InitCode b    -> b
            RuntimeCode b -> b

-- | Initialize empty contract with given code
initialContract :: ContractCode -> Contract
initialContract theContractCode = Contract
  { _contractcode = theContractCode
  , _codehash =
    case theCode of
      ConcreteBuf b -> keccak (stripBytecodeMetadata b)
      _ -> 0

  , _storage  = EmptyStore
  , _balance  = 0
  , _nonce    = if creation then 1 else 0
  , _opIxMap  = mkOpIxMap theCode
  , _codeOps  = mkCodeOps theCode
  , _external = False
  , _origStorage = mempty
  } where
      (creation, theCode) = case theContractCode of
            InitCode b    -> (True, b)
            RuntimeCode b -> (False, b)

contractWithStore :: ContractCode -> Expr Storage -> Contract
contractWithStore theContractCode store =
  initialContract theContractCode & set storage store

-- * Opcode dispatch (exec1)

-- | Update program counter
next :: (?op :: Word8) => EVM ()
next = modifying (state . pc) (+ (opSize ?op))

-- | Executes the EVM one step
exec1 :: EVM ()
exec1 = do
  vm <- get

  let
    -- Convenience function to access parts of the current VM state.
    -- Arcane type signature needed to avoid monomorphism restriction.
    the :: (b -> VM -> Const a VM) -> ((a -> Const a a) -> b) -> a
    the f g = view (f . g) vm

    -- Convenient aliases
    mem  = the state memory
    stk  = the state stack
    self = the state contract
    this = fromMaybe (error "internal error: state contract") (preview (ix self) (the env contracts))

    fees@FeeSchedule {..} = the block schedule

    doStop = finishFrame (FrameReturned mempty)

  if self > 0x0 && self <= 0x9 then do
    -- call to precompile
    let ?op = 0x00 -- dummy value
    case bufLength (the state calldata) of
      (Lit calldatasize) -> do
          copyBytesToMemory (the state calldata) (num calldatasize) 0 0
          executePrecompile self (num $ the state gas) 0 (num calldatasize) 0 0 []
          vmx <- get
          case view (state.stack) vmx of
            (x:_) -> case x of
              Lit (num -> x') -> case x' of
                0 -> do
                  fetchAccount self $ \_ -> do
                    touchAccount self
                    vmError PrecompileFailure
                _ -> fetchAccount self $ \_ -> do
                    touchAccount self
                    out <- use (state . returndata)
                    finishFrame (FrameReturned out)
              _ -> vmError $
                UnexpectedSymbolicArg (view (state . pc) vmx) "precompile returned a symbolic value"
            _ ->
              underrun
      _ -> vmError $ UnexpectedSymbolicArg (the state pc) "cannot call precompiles with symbolic data"

  -- This is a bit of a hack, but since we only use symbolic code to represent
  -- ctor args (appended to the end of ctor bytecode) it should be fine. It may
  -- be worth reworking the code representation to make this concrete code +
  -- optional symbolic data restriction more visible in the type system.
  -- TODO: is this actually safe? come up with a better representation. Does this work for immutable constants??
  else if (maybe True (\l -> the state pc >= l) (Expr.minLength (the state code)))
    then doStop

    else do
      let ?op = fromMaybe (error "could not analyze symbolic code") $ unlitByte $ Expr.readByte (Lit . num $ the state pc) (the state code)

      case ?op of

        -- op: PUSH
        x | x >= 0x60 && x <= 0x7f -> do
          let !n = num x - 0x60 + 1
              !xs = case the state code of
                -- TODO: pad right or left here?
                ConcreteBuf b -> Lit $ word $ padRight n $ BS.take n (BS.drop (1 + the state pc) b)
                b -> Expr.readBytes n (Lit . num $ the state pc + 1) b
          limitStack 1 $
            burn g_verylow $ do
              next
              pushSym xs

        -- op: DUP
        x | x >= 0x80 && x <= 0x8f -> do
          let !i = x - 0x80 + 1
          case preview (ix (num i - 1)) stk of
            Nothing -> underrun
            Just y ->
              limitStack 1 $
                burn g_verylow $ do
                  next
                  pushSym y

        -- op: SWAP
        x | x >= 0x90 && x <= 0x9f -> do
          let i = num (x - 0x90 + 1)
          if length stk < i + 1
            then underrun
            else
              burn g_verylow $ do
                next
                zoom (state . stack) $ do
                  assign (ix 0) (stk ^?! ix i)
                  assign (ix i) (stk ^?! ix 0)

        -- op: LOG
        x | x >= 0xa0 && x <= 0xa4 ->
          notStatic $
          let n = (num x - 0xa0) in
          case stk of
            (xOffset':xSize':xs) ->
              if length xs < n
              then underrun
              else
                forceConcrete2 (xOffset', xSize') "LOG" $ \(xOffset, xSize) -> do
                    let (topics, xs') = splitAt n xs
                        bytes         = readMemory (num xOffset) (num xSize) vm
                        logs'         = Log (litAddr self) bytes topics (view logs vm)
                    burn (g_log + g_logdata * (num xSize) + num n * g_logtopic) $
                      accessMemoryRange fees xOffset xSize $ do
                        traceTopLog logs'
                        next
                        assign (state . stack) xs'
                        assign logs logs'
            _ ->
              underrun

        -- op: STOP
        0x00 -> doStop

        -- op: ADD
        0x01 -> stackOp2 (const g_verylow) (uncurry Expr.add)
        -- op: MUL
        0x02 -> stackOp2 (const g_low) (uncurry Expr.mul)
        -- op: SUB
        0x03 -> stackOp2 (const g_verylow) (uncurry Expr.sub)

        -- op: DIV
        0x04 -> stackOp2 (const g_low) (uncurry Expr.div)

        -- op: SDIV
        0x05 -> stackOp2 (const g_low) (uncurry Expr.sdiv)

        -- op: MOD
        0x06 -> stackOp2 (const g_low) (uncurry Expr.mod)

        -- op: SMOD
        0x07 -> stackOp2 (const g_low) (uncurry Expr.smod)
        -- op: ADDMOD
        0x08 -> stackOp3 (const g_mid) (uncurryN Expr.addmod)
        -- op: MULMOD
        0x09 -> stackOp3 (const g_mid) (uncurryN Expr.mulmod)

        -- op: LT
        0x10 -> stackOp2 (const g_verylow) (uncurry Expr.lt)
        -- op: GT
        0x11 -> stackOp2 (const g_verylow) (uncurry Expr.gt)
        -- op: SLT
        0x12 -> stackOp2 (const g_verylow) (uncurry Expr.slt)
        -- op: SGT
        0x13 -> stackOp2 (const g_verylow) (uncurry Expr.sgt)

        -- op: EQ
        0x14 -> stackOp2 (const g_verylow) (uncurry Expr.eq)
        -- op: ISZERO
        0x15 -> stackOp1 (const g_verylow) (uncurryN Expr.iszero)

        -- op: AND
        0x16 -> stackOp2 (const g_verylow) (uncurry Expr.and)
        -- op: OR
        0x17 -> stackOp2 (const g_verylow) (uncurry Expr.or)
        -- op: XOR
        0x18 -> stackOp2 (const g_verylow) (uncurry Expr.xor)
        -- op: NOT
        0x19 -> stackOp1 (const g_verylow) (uncurryN Expr.not)

        -- op: BYTE
        0x1a -> stackOp2 (const g_verylow) (\(i, w) -> Expr.padByte $ Expr.indexWord i w)

        -- op: SHL
        0x1b -> stackOp2 (const g_verylow) (uncurry Expr.shl)
        -- op: SHR
        0x1c -> stackOp2 (const g_verylow) (uncurry Expr.shr)
        -- op: SAR
        0x1d -> stackOp2 (const g_verylow) (uncurry Expr.sar)

        -- op: SHA3
        -- more accurately refered to as KECCAK
        0x20 ->
          case stk of
            (xOffset' : xSize' : xs) ->
              forceConcrete xOffset' "sha3 offset must be concrete" $
                \xOffset -> forceConcrete xSize' "sha3 size must be concrete" $ \xSize ->
                  burn (g_sha3 + g_sha3word * ceilDiv (num xSize) 32) $
                    accessMemoryRange fees xOffset xSize $ do
                      (hash, invMap) <- case readMemory xOffset' xSize' vm of
                                          ConcreteBuf bs -> do
                                            let hash' = keccak bs
                                            pure (Lit hash', Map.singleton hash' bs)
                                          buf -> pure (Keccak buf, mempty)
                      next
                      assign (state . stack) (hash : xs)
                      (env . sha3Crack) <>= invMap
            _ -> underrun

        -- op: ADDRESS
        0x30 ->
          limitStack 1 $
            burn g_base (next >> push (num self))

        -- op: BALANCE
        0x31 ->
          case stk of
            (x':xs) -> forceConcrete x' "BALANCE" $ \x ->
              accessAndBurn (num x) $
                fetchAccount (num x) $ \c -> do
                  next
                  assign (state . stack) xs
                  push (view balance c)
            [] ->
              underrun

        -- op: ORIGIN
        0x32 ->
          limitStack 1 . burn g_base $
            next >> push (num (the tx origin))

        -- op: CALLER
        0x33 ->
          limitStack 1 . burn g_base $
            next >> pushSym (the state caller)

        -- op: CALLVALUE
        0x34 ->
          limitStack 1 . burn g_base $
            next >> pushSym (the state callvalue)

        -- op: CALLDATALOAD
        0x35 -> stackOp1 (const g_verylow) $
          \ind -> Expr.readWord ind (the state calldata)

        -- op: CALLDATASIZE
        0x36 ->
          limitStack 1 . burn g_base $
            next >> pushSym (bufLength (the state calldata))

        -- op: CALLDATACOPY
        0x37 ->
          case stk of
            (xTo' : xFrom : xSize' : xs) ->
              forceConcrete2 (xTo', xSize') "CALLDATACOPY" $
                \(xTo, xSize) ->
                  burn (g_verylow + g_copy * ceilDiv (num xSize) 32) $
                    accessUnboundedMemoryRange fees xTo xSize $ do
                      next
                      assign (state . stack) xs
                      copyBytesToMemory (the state calldata) xSize' xFrom xTo'
            _ -> underrun

        -- op: CODESIZE
        0x38 ->
          limitStack 1 . burn g_base $
            next >> pushSym (bufLength (the state code))

        -- op: CODECOPY
        0x39 ->
          case stk of
            (memOffset' : codeOffset : n' : xs) ->
              forceConcrete2 (memOffset', n') "CODECOPY" $
                \(memOffset,n) -> do
                  burn (g_verylow + g_copy * ceilDiv (num n) 32) $
                    accessUnboundedMemoryRange fees memOffset n $ do
                      next
                      assign (state . stack) xs
                      copyBytesToMemory (the state code) n' codeOffset memOffset'
            _ -> underrun

        -- op: GASPRICE
        0x3a ->
          limitStack 1 . burn g_base $
            next >> push (the tx gasprice)

        -- op: EXTCODESIZE
        0x3b ->
          case stk of
            (x':xs) -> case x' of
              (Lit x) -> if x == num cheatCode
                then do
                  next
                  assign (state . stack) xs
                  pushSym (Lit 1)
                else
                  accessAndBurn (num x) $
                    fetchAccount (num x) $ \c -> do
                      next
                      assign (state . stack) xs
                      pushSym (bufLength (view bytecode c))
              _ -> do
                assign (state . stack) xs
                pushSym (CodeSize x')
            [] ->
              underrun

        -- op: EXTCODECOPY
        0x3c ->
          case stk of
            ( extAccount'
              : memOffset'
              : codeOffset
              : codeSize'
              : xs ) ->
              forceConcrete3 (extAccount', memOffset', codeSize') "EXTCODECOPY" $
                \(extAccount, memOffset, codeSize) -> do
                  acc <- accessAccountForGas (num extAccount)
                  let cost = if acc then g_warm_storage_read else g_cold_account_access
                  burn (cost + g_copy * ceilDiv (num codeSize) 32) $
                    accessUnboundedMemoryRange fees memOffset codeSize $
                      fetchAccount (num extAccount) $ \c -> do
                        next
                        assign (state . stack) xs
                        copyBytesToMemory (view bytecode c) codeSize' codeOffset memOffset'
            _ -> underrun

        -- op: RETURNDATASIZE
        0x3d ->
          limitStack 1 . burn g_base $
            next >> pushSym (bufLength (the state returndata))

        -- op: RETURNDATACOPY
        0x3e ->
          case stk of
            (xTo' : xFrom : xSize' :xs) -> forceConcrete2 (xTo', xSize') "RETURNDATACOPY" $
              \(xTo, xSize) ->
                burn (g_verylow + g_copy * ceilDiv (num xSize) 32) $
                  accessUnboundedMemoryRange fees xTo xSize $ do
                    next
                    assign (state . stack) xs

                    let oob = Expr.lt (bufLength $ the state returndata) (Expr.add xFrom xSize')
                        overflow = Expr.lt (Expr.add xFrom xSize') (xFrom)
                        jump True = vmError InvalidMemoryAccess
                        jump False = copyBytesToMemory (the state returndata) xSize' xFrom xTo'
                    branch (Expr.or oob overflow) jump
            _ -> underrun

        -- op: EXTCODEHASH
        0x3f ->
          case stk of
            (x':xs) -> forceConcrete x' "EXTCODEHASH" $ \x ->
              accessAndBurn (num x) $ do
                next
                assign (state . stack) xs
                fetchAccount (num x) $ \c ->
                   if accountEmpty c
                     then push (num (0 :: Int))
                     else case view bytecode c of
                           ConcreteBuf b -> push (num (keccak b))
                           b -> pushSym (Keccak b)
            [] ->
              underrun

        -- op: BLOCKHASH
        0x40 -> do
          -- We adopt the fake block hash scheme of the VMTests,
          -- so that blockhash(i) is the hash of i as decimal ASCII.
          stackOp1 (const g_blockhash) $ \case
            (Lit i) -> if i + 256 < the block number || i >= the block number
                       then 0
                       else (num i :: Integer) & show & Char8.pack & keccak & num
            i -> BlockHash i

        -- op: COINBASE
        0x41 ->
          limitStack 1 . burn g_base $
            next >> push (num (the block coinbase))

        -- op: TIMESTAMP
        0x42 ->
          limitStack 1 . burn g_base $
            next >> pushSym (the block timestamp)

        -- op: NUMBER
        0x43 ->
          limitStack 1 . burn g_base $
            next >> push (the block number)

        -- op: DIFFICULTY
        0x44 ->
          limitStack 1 . burn g_base $
            next >> push (the block difficulty)

        -- op: GASLIMIT
        0x45 ->
          limitStack 1 . burn g_base $
            next >> push (the block gaslimit)

        -- op: CHAINID
        0x46 ->
          limitStack 1 . burn g_base $
            next >> push (the env chainId)

        -- op: SELFBALANCE
        0x47 ->
          limitStack 1 . burn g_low $
            next >> push (view balance this)

        -- op: BASEFEE
        0x48 ->
          limitStack 1 . burn g_base $
            next >> push (the block baseFee)

        -- op: POP
        0x50 ->
          case stk of
            (_:xs) -> burn g_base (next >> assign (state . stack) xs)
            _      -> underrun

        -- op: MLOAD
        0x51 ->
          case stk of
            (x':xs) -> forceConcrete x' "MLOAD" $ \x ->
              burn g_verylow $
                accessMemoryWord fees x $ do
                  next
                  assign (state . stack) (readWord (Lit x) mem : xs)
            _ -> underrun

        -- op: MSTORE
        0x52 ->
          case stk of
            (x':y:xs) -> forceConcrete x' "MSTORE" $ \x ->
              burn g_verylow $
                accessMemoryWord fees x $ do
                  next
                  assign (state . memory) (writeWord (Lit x) y mem)
                  assign (state . stack) xs
            _ -> underrun

        -- op: MSTORE8
        0x53 ->
          case stk of
            (x':y:xs) -> forceConcrete x' "MSTORE8" $ \x ->
              burn g_verylow $
                accessMemoryRange fees x 1 $ do
                  let yByte = indexWord 31 y
                  next
                  modifying (state . memory) (writeByte (Lit x) yByte)
                  assign (state . stack) xs
            _ -> underrun

        -- op: SLOAD
        0x54 ->
          case stk of
            (x:xs) -> do
              acc <- accessStorageForGas self x
              let cost = if acc then g_warm_storage_read else g_cold_sload
              burn cost $
                accessStorage self x $ \y -> do
                  next
                  assign (state . stack) (y:xs)
            _ -> underrun

        -- op: SSTORE
        0x55 ->
          notStatic $
          case stk of
            (x:new:xs) ->
              accessStorage self x $ \current -> do
                availableGas <- use (state . gas)

                if num availableGas <= g_callstipend
                  then finishFrame (FrameErrored (OutOfGas availableGas (num g_callstipend)))
                  else do
                    let original = case readStorage (ConcreteStore $ view origStorage this) x of
                                     Just (Lit v) -> v
                                     _ -> 0
                    let storage_cost = case (maybeLitWord current, maybeLitWord new) of
                                 (Just current', Just new') ->
                                    if (current' == new') then g_sload
                                    else if (current' == original) && (original == 0) then g_sset
                                    else if (current' == original) then g_sreset
                                    else g_sload

                                 -- if any of the arguments are symbolic,
                                 -- assume worst case scenario
                                 _ -> g_sset

                    acc <- accessStorageForGas self x
                    let cold_storage_cost = if acc then 0 else g_cold_sload
                    burn (storage_cost + cold_storage_cost) $ do
                      next
                      assign (state . stack) xs
                      modifying (env . contracts . ix self . storage)
                        (writeStorage x new)

                      case (maybeLitWord current, maybeLitWord new) of
                         (Just current', Just new') ->
                            unless (current' == new') $
                              if current' == original
                              then when (original /= 0 && new' == 0) $
                                      refund (g_sreset + g_access_list_storage_key)
                              else do
                                      when (original /= 0) $
                                        if new' == 0
                                        then refund (g_sreset + g_access_list_storage_key)
                                        else unRefund (g_sreset + g_access_list_storage_key)
                                      when (original == new') $
                                        if original == 0
                                        then refund (g_sset - g_sload)
                                        else refund (g_sreset - g_sload)
                         -- if any of the arguments are symbolic,
                         -- don't change the refund counter
                         _ -> noop
            _ -> underrun

        -- op: JUMP
        0x56 ->
          case stk of
            (x:xs) ->
              burn g_mid $ forceConcrete x "JUMP: symbolic jumpdest" $ \x' ->
                checkJump x' xs
            _ -> underrun

        -- op: JUMPI
        0x57 -> do
          case stk of
            (x:y:xs) -> forceConcrete x "JUMPI: symbolic jumpdest" $ \x' ->
                burn g_high $
                  let jump :: Bool -> EVM ()
                      jump True = assign (state . stack) xs >> next
                      jump _    = checkJump x' xs
                  in case maybeLitWord y of
                      Just y' -> jump (0 == y')
                      -- if the jump condition is symbolic, we explore both sides
                      Nothing -> branch y jump
            _ -> underrun

        -- op: PC
        0x58 ->
          limitStack 1 . burn g_base $
            next >> push (num (the state pc))

        -- op: MSIZE
        0x59 ->
          limitStack 1 . burn g_base $
            next >> push (num (the state memorySize))

        -- op: GAS
        0x5a ->
          limitStack 1 . burn g_base $
            next >> push (the state gas - num g_base)

        -- op: JUMPDEST
        0x5b -> burn g_jumpdest next

        -- op: EXP
        0x0a ->
          case stk of
            (base:exponent':xs) -> forceConcrete exponent' "EXP: symbolic exponent" $ \exponent ->
              let cost = if exponent == 0
                         then g_exp
                         else g_exp + g_expbyte * num (ceilDiv (1 + log2 exponent) 8)
              in burn cost $ do
                next
                state . stack .= Expr.exp base exponent' : xs
            _ -> underrun

        -- op: SIGNEXTEND
        0x0b -> stackOp2 (const g_low) (uncurry Expr.sex)

        -- op: CREATE
        0xf0 ->
          notStatic $
          case stk of
            (xValue' : xOffset' : xSize' : xs) -> forceConcrete3 (xValue', xOffset', xSize') "CREATE" $
              \(xValue, xOffset, xSize) -> do
                accessMemoryRange fees xOffset xSize $ do
                  availableGas <- use (state . gas)
                  let
                    newAddr = createAddress self (view nonce this)
                    (cost, gas') = costOfCreate fees availableGas 0
                  _ <- accessAccountForGas newAddr
                  burn (cost - gas') $
                    let initCode = readMemory (num xOffset) (num xSize) vm
                    in create self this (num gas') xValue xs newAddr initCode
            _ -> underrun

        -- op: CALL
        0xf1 ->
          case stk of
            ( xGas'
              : xTo
              : xValue'
              : xInOffset'
              : xInSize'
              : xOutOffset'
              : xOutSize'
              : xs
             ) -> forceConcrete6 (xGas', xValue', xInOffset', xInSize', xOutOffset', xOutSize') "CALL" $
              \(xGas, xValue, xInOffset, xInSize, xOutOffset, xOutSize) ->
                (if xValue > 0 then notStatic else id) $
                  delegateCall this xGas xTo xTo xValue xInOffset xInSize xOutOffset xOutSize xs $ \callee -> do
                    zoom state $ do
                      assign callvalue (Lit xValue)
                      assign caller (litAddr self)
                      assign contract callee
                    transfer self callee xValue
                    touchAccount self
                    touchAccount callee
            _ ->
              underrun

        -- op: CALLCODE
        0xf2 ->
          case stk of
            ( xGas'
              : xTo
              : xValue'
              : xInOffset'
              : xInSize'
              : xOutOffset'
              : xOutSize'
              : xs
              ) -> forceConcrete6 (xGas', xValue', xInOffset', xInSize', xOutOffset', xOutSize') "CALLCODE" $
                \(xGas, xValue, xInOffset, xInSize, xOutOffset, xOutSize) ->
                  delegateCall this xGas xTo (litAddr self) xValue xInOffset xInSize xOutOffset xOutSize xs $ \_ -> do
                    zoom state $ do
                      assign callvalue (Lit xValue)
                      assign caller (litAddr self)
                    touchAccount self
            _ ->
              underrun

        -- op: RETURN
        0xf3 ->
          case stk of
            (xOffset' : xSize' :_) -> forceConcrete2 (xOffset', xSize') "RETURN" $ \(xOffset, xSize) ->
              accessMemoryRange fees xOffset xSize $ do
                let
                  output = readMemory xOffset' xSize' vm
                  codesize = num (bufLength output)
                  maxsize = the block maxCodeSize
                  creation = case view frames vm of
                    [] -> the tx isCreate
                    frame:_ -> case view frameContext frame of
                       CreationContext {} -> True
                       CallContext {} -> False
                if creation
                then
                  if codesize > maxsize
                  then
                    finishFrame (FrameErrored (MaxCodeSizeExceeded maxsize codesize))
                  else
                    branch (Expr.eqByte (readByte 0 output) (LitByte 0xef)) $ \case
                      True -> finishFrame $ FrameErrored InvalidFormat
                      False -> do
                        burn (g_codedeposit * num codesize) $
                          finishFrame (FrameReturned output)
                else
                   finishFrame (FrameReturned output)
            _ -> underrun

        -- op: DELEGATECALL
        0xf4 ->
          case stk of
            (xGas'
             :xTo
             :xInOffset'
             :xInSize'
             :xOutOffset'
             :xOutSize'
             :xs) -> forceConcrete5 (xGas', xInOffset', xInSize', xOutOffset', xOutSize') "DELEGATECALL" $
              \(xGas, xInOffset, xInSize, xOutOffset, xOutSize) ->
                delegateCall this xGas xTo (litAddr self) 0 xInOffset xInSize xOutOffset xOutSize xs $ \_ -> do
                  touchAccount self
            _ -> underrun

        -- op: CREATE2
        0xf5 -> notStatic $
          case stk of
            (xValue'
             :xOffset'
             :xSize'
             :xSalt'
             :xs) -> forceConcrete4 (xValue', xOffset', xSize', xSalt') "CREATE2" $
              \(xValue, xOffset, xSize, xSalt) ->
                accessMemoryRange fees xOffset xSize $ do
                  availableGas <- use (state . gas)

                  forceConcreteBuf (readMemory (num xOffset) (num xSize) vm) "CREATE2" $
                    \initCode -> do
                      let
                        newAddr  = create2Address self (num xSalt) initCode
                        (cost, gas') = costOfCreate fees availableGas xSize
                      _ <- accessAccountForGas newAddr
                      burn (cost - gas') $
                       create self this (num gas') xValue xs newAddr (ConcreteBuf initCode)
            _ -> underrun

        -- op: STATICCALL
        0xfa ->
          case stk of
            (xGas'
             :xTo
             :xInOffset'
             :xInSize'
             :xOutOffset'
             :xOutSize'
             :xs) -> forceConcrete5 (xGas', xInOffset', xInSize', xOutOffset', xOutSize') "STATICCALL" $
              \(xGas, xInOffset, xInSize, xOutOffset, xOutSize) -> do
                delegateCall this xGas xTo xTo 0 xInOffset xInSize xOutOffset xOutSize xs $ \callee -> do
                  zoom state $ do
                    assign callvalue 0
                    assign caller (litAddr self)
                    assign contract callee
                    assign static True
                  touchAccount self
                  touchAccount callee
            _ ->
              underrun

        -- op: SELFDESTRUCT
        0xff ->
          notStatic $
          case stk of
            [] -> underrun
            (xTo':_) -> forceConcrete xTo' "SELFDESTRUCT" $ \(num -> xTo) -> do
              acc <- accessAccountForGas (num xTo)
              let cost = if acc then 0 else g_cold_account_access
                  funds = view balance this
                  recipientExists = accountExists xTo vm
                  c_new = if not recipientExists && funds /= 0
                          then num g_selfdestruct_newaccount
                          else 0
              burn (g_selfdestruct + c_new + cost) $ do
                   selfdestruct self
                   touchAccount xTo

                   if funds /= 0
                   then fetchAccount xTo $ \_ -> do
                          env . contracts . ix xTo . balance += funds
                          assign (env . contracts . ix self . balance) 0
                          doStop
                   else doStop

        -- op: REVERT
        0xfd ->
          case stk of
            (xOffset':xSize':_) -> forceConcrete2 (xOffset', xSize') "REVERT" $ \(xOffset, xSize) ->
              accessMemoryRange fees xOffset xSize $ do
                let output = readMemory xOffset' xSize' vm
                finishFrame (FrameReverted output)
            _ -> underrun

        xxx ->
          vmError (UnrecognizedOpcode xxx)

transfer :: Addr -> Addr -> W256 -> EVM ()
transfer xFrom xTo xValue =
  zoom (env . contracts) $ do
    ix xFrom . balance -= xValue
    ix xTo  . balance += xValue

-- | Checks a *CALL for failure; OOG, too many callframes, memory access etc.
callChecks
  :: (?op :: Word8)
  => Contract -> W256 -> Addr -> Addr -> W256 -> W256 -> W256 -> W256 -> W256 -> [Expr EWord]
   -- continuation with gas available for call
  -> (Integer -> EVM ())
  -> EVM ()
callChecks this xGas xContext xTo xValue xInOffset xInSize xOutOffset xOutSize xs continue = do
  vm <- get
  let fees = view (block . schedule) vm
  accessMemoryRange fees xInOffset xInSize $
    accessMemoryRange fees xOutOffset xOutSize $ do
      availableGas <- use (state . gas)
      let recipientExists = accountExists xContext vm
      (cost, gas') <- costOfCall fees recipientExists xValue availableGas xGas xTo
      burn (cost - gas') $ do
        if xValue > view balance this
        then do
          assign (state . stack) (0 : xs)
          assign (state . returndata) mempty
          pushTrace $ ErrorTrace $ BalanceTooLow xValue (view balance this)
          next
        else if length (view frames vm) >= 1024
             then do
               assign (state . stack) (0 : xs)
               assign (state . returndata) mempty
               pushTrace $ ErrorTrace CallDepthLimitReached
               next
             else continue gas'

precompiledContract
  :: (?op :: Word8)
  => Contract
  -> W256
  -> Addr
  -> Addr
  -> W256
  -> W256 -> W256 -> W256 -> W256
  -> [Expr EWord]
  -> EVM ()
precompiledContract this xGas precompileAddr recipient xValue inOffset inSize outOffset outSize xs =
  callChecks this xGas recipient precompileAddr xValue inOffset inSize outOffset outSize xs $ \gas' ->
  do
    executePrecompile precompileAddr gas' inOffset inSize outOffset outSize xs
    self <- use (state . contract)
    stk <- use (state . stack)
    pc' <- use (state . pc)
    case stk of
      (x:_) -> case maybeLitWord x of
        Just 0 ->
          return ()
        Just 1 ->
          fetchAccount recipient $ \_ -> do

          transfer self recipient xValue
          touchAccount self
          touchAccount recipient
        _ -> vmError $ UnexpectedSymbolicArg pc' "symbolic return value from precompile"
      _ -> underrun

executePrecompile
  :: (?op :: Word8)
  => Addr
  -> Integer -> W256 -> W256 -> W256 -> W256 -> [Expr EWord]
  -> EVM ()
executePrecompile preCompileAddr gasCap inOffset inSize outOffset outSize xs  = do
  vm <- get
  let input = readMemory (num inOffset) (num inSize) vm
      fees = view (block . schedule) vm
      cost = costOfPrecompile fees preCompileAddr input
      notImplemented = error $ "precompile at address " <> show preCompileAddr <> " not yet implemented"
      precompileFail = burn (num gasCap - cost) $ do
                         assign (state . stack) (0 : xs)
                         pushTrace $ ErrorTrace PrecompileFailure
                         next
  if cost > num gasCap then
    burn (num gasCap) $ do
      assign (state . stack) (0 : xs)
      next
  else
    burn cost $
      case preCompileAddr of
        -- ECRECOVER
        0x1 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "ECRECOVER" $ \input' ->
          case EVM.Precompiled.execute 0x1 (truncpadlit 128 input') 32 of
            Nothing -> do
              -- return no output for invalid signature
              assign (state . stack) (1 : xs)
              assign (state . returndata) mempty
              next
            Just output -> do
              assign (state . stack) (1 : xs)
              assign (state . returndata) (ConcreteBuf output)
              copyBytesToMemory (ConcreteBuf output) (Lit outSize) (Lit 0) (Lit outOffset)
              next

        -- SHA2-256
        0x2 ->
          let
            hash = case input of
                     ConcreteBuf input' -> ConcreteBuf $ BS.pack $ BA.unpack (Crypto.hash input' :: Digest SHA256)
                     _ -> WriteWord (Lit 0) (SHA256 input) EmptyBuf
          in do
            assign (state . stack) (1 : xs)
            assign (state . returndata) hash
            copyBytesToMemory hash (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- RIPEMD-160
        0x3 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "RIPEMD160" $ \input' ->

          let
            padding = BS.pack $ replicate 12 0
            hash' = BS.pack $ BA.unpack (Crypto.hash input' :: Digest RIPEMD160)
            hash  = ConcreteBuf $ padding <> hash'
          in do
            assign (state . stack) (1 : xs)
            assign (state . returndata) hash
            copyBytesToMemory hash (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- IDENTITY
        0x4 -> do
            assign (state . stack) (1 : xs)
            assign (state . returndata) input
            copyCallBytesToMemory input (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- MODEXP
        0x5 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "MODEXP" $ \input' ->

          let
            (lenb, lene, lenm) = parseModexpLength input'

            output = ConcreteBuf $
              if isZero (96 + lenb + lene) lenm input'
              then truncpadlit (num lenm) (asBE (0 :: Int))
              else
                let
                  b = asInteger $ lazySlice 96 lenb input'
                  e = asInteger $ lazySlice (96 + lenb) lene input'
                  m = asInteger $ lazySlice (96 + lenb + lene) lenm input'
                in
                  padLeft (num lenm) (asBE (expFast b e m))
          in do
            assign (state . stack) (1 : xs)
            assign (state . returndata) output
            copyBytesToMemory output (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- ECADD
        0x6 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "ECADD" $ \input' ->
           case EVM.Precompiled.execute 0x6 (truncpadlit 128 input') 64 of
          Nothing -> precompileFail
          Just output -> do
            let truncpaddedOutput = ConcreteBuf $ truncpadlit 64 output
            assign (state . stack) (1 : xs)
            assign (state . returndata) truncpaddedOutput
            copyBytesToMemory truncpaddedOutput (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- ECMUL
        0x7 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "ECMUL" $ \input' ->

          case EVM.Precompiled.execute 0x7 (truncpadlit 96 input') 64 of
          Nothing -> precompileFail
          Just output -> do
            let truncpaddedOutput = ConcreteBuf $ truncpadlit 64 output
            assign (state . stack) (1 : xs)
            assign (state . returndata) truncpaddedOutput
            copyBytesToMemory truncpaddedOutput (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- ECPAIRING
        0x8 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "ECPAIR" $ \input' ->

          case EVM.Precompiled.execute 0x8 input' 32 of
          Nothing -> precompileFail
          Just output -> do
            let truncpaddedOutput = ConcreteBuf $ truncpadlit 32 output
            assign (state . stack) (1 : xs)
            assign (state . returndata) truncpaddedOutput
            copyBytesToMemory truncpaddedOutput (Lit outSize) (Lit 0) (Lit outOffset)
            next

        -- BLAKE2
        0x9 ->
         -- TODO: support symbolic variant
         forceConcreteBuf input "BLAKE2" $ \input' -> do

          case (BS.length input', 1 >= BS.last input') of
            (213, True) -> case EVM.Precompiled.execute 0x9 input' 64 of
              Just output -> do
                let truncpaddedOutput = ConcreteBuf $ truncpadlit 64 output
                assign (state . stack) (1 : xs)
                assign (state . returndata) truncpaddedOutput
                copyBytesToMemory truncpaddedOutput (Lit outSize) (Lit 0) (Lit outOffset)
                next
              Nothing -> precompileFail
            _ -> precompileFail


        _   -> notImplemented

truncpadlit :: Int -> ByteString -> ByteString
truncpadlit n xs = if m > n then BS.take n xs
                   else BS.append xs (BS.replicate (n - m) 0)
  where m = BS.length xs

lazySlice :: W256 -> W256 -> ByteString -> LS.ByteString
lazySlice offset size bs =
  let bs' = LS.take (num size) (LS.drop (num offset) (fromStrict bs))
  in bs' <> LS.replicate ((num size) - LS.length bs') 0

parseModexpLength :: ByteString -> (W256, W256, W256)
parseModexpLength input =
  let lenb = word $ LS.toStrict $ lazySlice  0 32 input
      lene = word $ LS.toStrict $ lazySlice 32 64 input
      lenm = word $ LS.toStrict $ lazySlice 64 96 input
  in (lenb, lene, lenm)

--- checks if a range of ByteString bs starting at offset and length size is all zeros.
isZero :: W256 -> W256 -> ByteString -> Bool
isZero offset size bs =
  LS.all (== 0) $
    LS.take (num size) $
      LS.drop (num offset) $
        fromStrict bs

asInteger :: LS.ByteString -> Integer
asInteger xs = if xs == mempty then 0
  else 256 * asInteger (LS.init xs)
      + num (LS.last xs)

-- * Opcode helper actions

noop :: Monad m => m ()
noop = pure ()

pushTo :: MonadState s m => ASetter s s [a] [a] -> a -> m ()
pushTo f x = f %= (x :)

pushToSequence :: MonadState s m => ASetter s s (Seq a) (Seq a) -> a -> m ()
pushToSequence f x = f %= (Seq.|> x)

getCodeLocation :: VM -> CodeLocation
getCodeLocation vm = (view (state . contract) vm, view (state . pc) vm)

-- | Ask the SMT solver to provide a concrete model for val iff a unique model exists
--makeUnique :: Expr EWord -> (Word -> EVM ()) -> EVM ()
--makeUnique sw@(S w val) cont = case maybeLitWord sw of
  --Nothing -> do
    --conditions <- use constraints
    --assign result . Just . VMFailure . Query $ PleaseMakeUnique val (fst <$> conditions) $ \case
      --Unique a -> do
        --assign result Nothing
        --cont (C w $ fromSizzle a)
      --InconsistentU -> vmError DeadPath
      --TimeoutU -> vmError SMTTimeout
      --Multiple -> vmError $ NotUnique w
  --Just a -> cont a

---- | Construct SMT Query and halt execution until resolved
--askSMT :: CodeLocation -> (SBool, Whiff) -> (Bool -> EVM ()) -> EVM ()
--askSMT codeloc (condition, whiff) continue = do
  ---- We keep track of how many times we have come across this particular
  ---- (contract, pc) combination in the `iteration` mapping.
  --iteration <- use (iterations . at codeloc . non 0)

  ---- If we are backstepping, the result of this query should be cached
  ---- already. So we first check the cache to see if the result is known
  --use (cache . path . at (codeloc, iteration)) >>= \case
     ---- If the query has been done already, select path or select the only available
     --Just w -> choosePath (Case w)
     ---- If this is a new query, run the query, cache the result
     ---- increment the iterations and select appropriate path
     --Nothing -> do pathconds <- use constraints
                   --assign result . Just . VMFailure . Query $ PleaseAskSMT
                     --condition' (fst <$> pathconds) choosePath

   --where condition' = simplifyCondition condition whiff

         --choosePath :: BranchCondition -> EVM ()
         ---- Only one path is possible
         --choosePath (Case v) = do assign result Nothing
                                  --pushTo constraints $ if v then (condition', whiff) else (sNot condition', IsZero whiff)
                                  --iteration <- use (iterations . at codeloc . non 0)
                                  --assign (cache . path . at (codeloc, iteration)) (Just v)
                                  --assign (iterations . at codeloc) (Just (iteration + 1))
                                  --continue v
         ---- Both paths are possible; we ask for more input
         --choosePath Unknown = assign result . Just . VMFailure . Choose . PleaseChoosePath whiff $ choosePath . Case
         ---- None of the paths are possible; fail this branch
         --choosePath Inconsistent = vmError DeadPath

branch :: Expr EWord -> (Bool -> EVM ()) -> EVM ()
branch cond jump = assign result . Just . VMFailure . Choose $ PleaseChoosePath cond jump

-- | Construct RPC Query and halt execution until resolved
fetchAccount :: Addr -> (Contract -> EVM ()) -> EVM ()
fetchAccount addr continue =
  use (env . contracts . at addr) >>= \case
    Just c -> continue c
    Nothing ->
      use (cache . fetched . at addr) >>= \case
        Just c -> do
          assign (env . contracts . at addr) (Just c)
          continue c
        Nothing -> do
          model <- use (env . storageModel)
          assign result . Just . VMFailure $ Query $
            PleaseFetchContract addr model
              (\c -> do assign (cache . fetched . at addr) (Just c)
                        assign (env . contracts . at addr) (Just c)
                        assign result Nothing
                        continue c)

accessStorage
  :: Addr                   -- ^ Contract address
  -> Expr EWord             -- ^ Storage slot key
  -> (Expr EWord -> EVM ()) -- ^ Continuation
  -> EVM ()
accessStorage addr slot continue =
  use (env . contracts . at addr) >>= \case
    Just c ->
      case readStorage (view storage c) slot of
        -- Notice that if storage is symbolic, we always continue straight away
        Just x ->
          continue x
        Nothing ->
          if view external c
          then
            -- check if the slot is cached
            use (cache . fetched . at addr) >>= \case
              Nothing -> forceConcrete slot "cannot read symbolic slots via RPC" mkQuery
              Just cachedContract -> forceConcrete slot "cannot read symbolic slots via rpc" $
                \s -> maybe (mkQuery s) continue (readStorage (view storage cachedContract) slot)
          else do
            modifying (env . contracts . ix addr . storage) (writeStorage slot 0)
            continue 0
    Nothing ->
      fetchAccount addr $ \_ ->
        accessStorage addr slot continue
  where
      mkQuery s = assign result . Just . VMFailure . Query $
                    PleaseFetchSlot addr s
                      (\(Lit -> x) -> do
                          modifying (cache . fetched . ix addr . storage) (writeStorage slot x)
                          modifying (env . contracts . ix addr . storage) (writeStorage slot x)
                          assign result Nothing
                          continue x)

accountExists :: Addr -> VM -> Bool
accountExists addr vm =
  case view (env . contracts . at addr) vm of
    Just c -> not (accountEmpty c)
    Nothing -> False

-- EIP 161
accountEmpty :: Contract -> Bool
accountEmpty c =
  case view contractcode c of
    RuntimeCode b -> bufLength b == Lit 0
    _ -> False
  && (view nonce c == 0)
  && (view balance c == 0)

-- * How to finalize a transaction
finalize :: EVM ()
finalize = do
  let
    revertContracts  = use (tx . txReversion) >>= assign (env . contracts)
    revertSubstate   = assign (tx . substate) (SubState mempty mempty mempty mempty mempty)

  use result >>= \case
    Nothing ->
      error "Finalising an unfinished tx."
    Just (VMFailure (EVM.Revert _)) -> do
      revertContracts
      revertSubstate
    Just (VMFailure _) -> do
      -- burn remaining gas
      assign (state . gas) 0
      revertContracts
      revertSubstate
    Just (VMSuccess output) -> do
      -- deposit the code from a creation tx
      creation <- use (tx . isCreate)
      createe  <- use (state . contract)
      createeExists <- (Map.member createe) <$> use (env . contracts)

      when (creation && createeExists) $ replaceCode createe (RuntimeCode output)

  -- compute and pay the refund to the caller and the
  -- corresponding payment to the miner
  txOrigin     <- use (tx . origin)
  sumRefunds   <- (sum . (snd <$>)) <$> (use (tx . substate . refunds))
  miner        <- use (block . coinbase)
  blockReward  <- num . r_block <$> (use (block . schedule))
  gasPrice     <- use (tx . gasprice)
  priorityFee  <- use (tx . txPriorityFee)
  gasLimit     <- use (tx . txgaslimit)
  gasRemaining <- use (state . gas)

  let
    gasUsed      = gasLimit - gasRemaining
    cappedRefund = min (quot gasUsed 5) (num sumRefunds)
    originPay    = (gasRemaining + cappedRefund) * gasPrice

    minerPay     = priorityFee * gasUsed

  modifying (env . contracts)
     (Map.adjust (over balance (+ originPay)) txOrigin)
  modifying (env . contracts)
     (Map.adjust (over balance (+ minerPay)) miner)
  touchAccount miner

  -- pay out the block reward, recreating the miner if necessary
  preuse (env . contracts . ix miner) >>= \case
    Nothing -> modifying (env . contracts)
      (Map.insert miner (initialContract (EVM.RuntimeCode mempty)))
    Just _  -> noop
  modifying (env . contracts)
    (Map.adjust (over balance (+ blockReward)) miner)

  -- perform state trie clearing (EIP 161), of selfdestructs
  -- and touched accounts. addresses are cleared if they have
  --    a) selfdestructed, or
  --    b) been touched and
  --    c) are empty.
  -- (see Yellow Paper "Accrued Substate")
  --
  -- remove any destructed addresses
  destroyedAddresses <- use (tx . substate . selfdestructs)
  modifying (env . contracts)
    (Map.filterWithKey (\k _ -> (k `notElem` destroyedAddresses)))
  -- then, clear any remaining empty and touched addresses
  touchedAddresses <- use (tx . substate . touchedAccounts)
  modifying (env . contracts)
    (Map.filterWithKey
      (\k a -> not ((k `elem` touchedAddresses) && accountEmpty a)))

-- | Loads the selected contract as the current contract to execute
loadContract :: Addr -> EVM ()
loadContract target =
  preuse (env . contracts . ix target . contractcode) >>=
    \case
      Nothing ->
        error "Call target doesn't exist"
      Just (InitCode targetCode) -> do
        assign (state . contract) target
        assign (state . code)     targetCode
        assign (state . codeContract) target
      Just (RuntimeCode targetCode) -> do
        assign (state . contract) target
        assign (state . code)     targetCode
        assign (state . codeContract) target

limitStack :: Int -> EVM () -> EVM ()
limitStack n continue = do
  stk <- use (state . stack)
  if length stk + n > 1024
    then vmError StackLimitExceeded
    else continue

notStatic :: EVM () -> EVM ()
notStatic continue = do
  bad <- use (state . static)
  if bad
    then vmError StateChangeWhileStatic
    else continue

-- | Burn gas, failing if insufficient gas is available
-- We use the `Integer` type to avoid overflows in intermediate
-- calculations and throw if the value won't fit into a uint64
burn :: Integer -> EVM () -> EVM ()
burn n' continue =
  if n' > (2 :: Integer) ^ (64 :: Integer) - 1
  then vmError IllegalOverflow
  else do
    let n = num n'
    available <- use (state . gas)
    if n <= available
      then do
        state . gas -= n
        burned += n
        continue
      else
        vmError (OutOfGas available n)

--forceConcreteAddr :: SAddr -> (Addr -> EVM ()) -> EVM ()
--forceConcreteAddr n continue = case maybeLitAddr n of
  --Nothing -> vmError UnexpectedSymbolicArg
  --Just c -> continue c

forceConcrete :: Expr EWord -> String -> (W256 -> EVM ()) -> EVM ()
forceConcrete n msg continue = case maybeLitWord n of
  Nothing -> do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg
  Just c -> continue c

forceConcrete2 :: (Expr EWord, Expr EWord) -> String -> ((W256, W256) -> EVM ()) -> EVM ()
forceConcrete2 (n,m) msg continue = case (maybeLitWord n, maybeLitWord m) of
  (Just c, Just d) -> continue (c, d)
  _ -> do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg

forceConcrete3 :: (Expr EWord, Expr EWord, Expr EWord) -> String -> ((W256, W256, W256) -> EVM ()) -> EVM ()
forceConcrete3 (k,n,m) msg continue = case (maybeLitWord k, maybeLitWord n, maybeLitWord m) of
  (Just c, Just d, Just f) -> continue (c, d, f)
  _ -> do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg

forceConcrete4 :: (Expr EWord, Expr EWord, Expr EWord, Expr EWord) -> String -> ((W256, W256, W256, W256) -> EVM ()) -> EVM ()
forceConcrete4 (k,l,n,m) msg continue = case (maybeLitWord k, maybeLitWord l, maybeLitWord n, maybeLitWord m) of
  (Just b, Just c, Just d, Just f) -> continue (b, c, d, f)
  _ -> do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg

forceConcrete5 :: (Expr EWord, Expr EWord, Expr EWord, Expr EWord, Expr EWord) -> String -> ((W256, W256, W256, W256, W256) -> EVM ()) -> EVM ()
forceConcrete5 (k,l,m,n,o) msg continue = case (maybeLitWord k, maybeLitWord l, maybeLitWord m, maybeLitWord n, maybeLitWord o) of
  (Just a, Just b, Just c, Just d, Just e) -> continue (a, b, c, d, e)
  _ -> do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg

forceConcrete6 :: (Expr EWord, Expr EWord, Expr EWord, Expr EWord, Expr EWord, Expr EWord) -> String -> ((W256, W256, W256, W256, W256, W256) -> EVM ()) -> EVM ()
forceConcrete6 (k,l,m,n,o,p) msg continue = case (maybeLitWord k, maybeLitWord l, maybeLitWord m, maybeLitWord n, maybeLitWord o, maybeLitWord p) of
  (Just a, Just b, Just c, Just d, Just e, Just f) -> continue (a, b, c, d, e, f)
  _ -> do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg

forceConcreteBuf :: Expr Buf -> String -> (ByteString -> EVM ()) -> EVM ()
forceConcreteBuf (ConcreteBuf b) _ continue = continue b
forceConcreteBuf _ msg _ = do
    vm <- get
    vmError $ UnexpectedSymbolicArg (view (state . pc) vm) msg

-- * Substate manipulation
refund :: Integer -> EVM ()
refund n = do
  self <- use (state . contract)
  pushTo (tx . substate . refunds) (self, n)

unRefund :: Integer -> EVM ()
unRefund n = do
  self <- use (state . contract)
  refs <- use (tx . substate . refunds)
  assign (tx . substate . refunds)
    (filter (\(a,b) -> not (a == self && b == n)) refs)

touchAccount :: Addr -> EVM()
touchAccount = pushTo ((tx . substate) . touchedAccounts)

selfdestruct :: Addr -> EVM()
selfdestruct = pushTo ((tx . substate) . selfdestructs)

accessAndBurn :: Addr -> EVM () -> EVM ()
accessAndBurn x cont = do
  FeeSchedule {..} <- use ( block . schedule )
  acc <- accessAccountForGas x
  let cost = if acc then g_warm_storage_read else g_cold_account_access
  burn cost cont

-- | returns a wrapped boolean- if true, this address has been touched before in the txn (warm gas cost as in EIP 2929)
-- otherwise cold
accessAccountForGas :: Addr -> EVM Bool
accessAccountForGas addr = do
  accessedAddrs <- use (tx . substate . accessedAddresses)
  let accessed = member addr accessedAddrs
  assign (tx . substate . accessedAddresses) (insert addr accessedAddrs)
  return accessed

-- | returns a wrapped boolean- if true, this slot has been touched before in the txn (warm gas cost as in EIP 2929)
-- otherwise cold
accessStorageForGas :: Addr -> Expr EWord -> EVM Bool
accessStorageForGas addr key = do
  accessedStrkeys <- use (tx . substate . accessedStorageKeys)
  case maybeLitWord key of
    Just litword -> do
      let accessed = member (addr, litword) accessedStrkeys
      assign (tx . substate . accessedStorageKeys) (insert (addr, litword) accessedStrkeys)
      return accessed
    _ -> return False

-- * Cheat codes

-- The cheat code is 7109709ecfa91a80626ff3989d68f67f5b1dd12d.
-- Call this address using one of the cheatActions below to do
-- special things, e.g. changing the block timestamp. Beware that
-- these are necessarily hevm specific.
cheatCode :: Addr
cheatCode = num (keccak "hevm cheat code")

cheat
  :: (?op :: Word8)
  => (W256, W256) -> (W256, W256)
  -> EVM ()
cheat (inOffset, inSize) (outOffset, outSize) = do
  mem <- use (state . memory)
  vm <- get
  let
    abi = readBytes 4 (Lit inOffset) mem
    input = readMemory (Lit $ inOffset + 4) (Lit $ inSize - 4) vm
  case maybeLitWord abi of
    Nothing -> vmError $ UnexpectedSymbolicArg (view (state . pc) vm) "symbolic cheatcode selector"
    Just (fromIntegral -> abi') ->
      case Map.lookup abi' cheatActions of
        Nothing ->
          vmError (BadCheatCode (Just abi'))
        Just action -> do
            action (Lit outOffset) (Lit outSize) input
            next
            push 1

type CheatAction = Expr EWord -> Expr EWord -> Expr Buf -> EVM ()

cheatActions :: Map Word32 CheatAction
cheatActions =
  Map.fromList
    [ action "ffi(string[])" $
        \sig outOffset outSize input -> do
          vm <- get
          if view EVM.allowFFI vm then
            case decodeBuf [AbiArrayDynamicType AbiStringType] input of
              CAbi valsArr -> case valsArr of
                [AbiArrayDynamic AbiStringType strsV] ->
                  let
                    cmd = fmap
                            (\case
                              (AbiString a) -> unpack $ decodeUtf8 a
                              _ -> "")
                            (V.toList strsV)
                    cont bs = do
                      let encoded = ConcreteBuf bs
                      assign (state . returndata) encoded
                      copyBytesToMemory encoded outSize 0 outOffset
                      assign result Nothing
                  in assign result (Just . VMFailure . Query $ (PleaseDoFFI cmd cont))
                _ -> vmError (BadCheatCode sig)
              _ -> vmError (BadCheatCode sig)
          else
            let msg = encodeUtf8 "ffi disabled: run again with --ffi if you want to allow tests to call external scripts"
            in vmError . EVM.Revert . ConcreteBuf $
              abiMethod "Error(string)" (AbiTuple . V.fromList $ [AbiString msg]),

      action "warp(uint256)" $
        \sig _ _ input -> case decodeStaticArgs 1 input of
          [x]  -> assign (block . timestamp) x
          _ -> vmError (BadCheatCode sig),

      action "roll(uint256)" $
        \sig _ _ input -> case decodeStaticArgs 1 input of
          [x] -> forceConcrete x "cannot roll to a symbolic block number" (assign (block . number))
          _ -> vmError (BadCheatCode sig),

      action "store(address,bytes32,bytes32)" $
        \sig _ _ input -> case decodeStaticArgs 3 input of
          [a, slot, new] ->
            forceConcrete a "cannot store at a symbolic address" $ \(num -> a') ->
              fetchAccount a' $ \_ -> do
                modifying (env . contracts . ix a' . storage) (writeStorage slot new)
          _ -> vmError (BadCheatCode sig),

      action "load(address,bytes32)" $
        \sig outOffset _ input -> case decodeStaticArgs 2 input of
          [a, slot] ->
            forceConcrete a "cannot load from a symbolic address" $ \(num -> a') ->
              accessStorage a' slot $ \res -> do
                assign (state . returndata . word256At 0) res
                assign (state . memory . word256At outOffset) res
          _ -> vmError (BadCheatCode sig),

      action "sign(uint256,bytes32)" $
        \sig outOffset _ input -> case decodeStaticArgs 2 input of
          [sk, hash] ->
            forceConcrete2 (sk, hash) "cannot sign symbolic data" $ \(sk', hash') -> let
              curve = getCurveByName SEC_p256k1
              priv = PrivateKey curve (num sk')
              digest = digestFromByteString (word256Bytes hash')
            in do
              case digest of
                Nothing -> vmError (BadCheatCode sig)
                Just digest' -> do
                  let s = ethsign priv digest'
                      v = if even (sign_s s) then 27 else 28
                      encoded = encodeAbiValue $
                        AbiTuple (RegularVector.fromList
                          [ AbiUInt 8 v
                          , AbiBytes 32 (word256Bytes . fromInteger $ sign_r s)
                          , AbiBytes 32 (word256Bytes . fromInteger $ sign_s s)
                          ])
                  assign (state . returndata) (ConcreteBuf encoded)
                  copyBytesToMemory (ConcreteBuf encoded) (num . BS.length $ encoded) 0 outOffset
          _ -> vmError (BadCheatCode sig),

      action "addr(uint256)" $
        \sig outOffset _ input -> case decodeStaticArgs 1 input of
          [sk] -> forceConcrete sk "cannot derive address for a symbolic key" $ \sk' -> let
                curve = getCurveByName SEC_p256k1
                pubPoint = generateQ curve (num sk')
                encodeInt = encodeAbiValue . AbiUInt 256 . fromInteger
              in do
                case pubPoint of
                  PointO -> do vmError (BadCheatCode sig)
                  Point x y -> do
                    -- See yellow paper #286
                    let
                      pub = BS.concat [ encodeInt x, encodeInt y ]
                      addr = num . word256 . BS.drop 12 . BS.take 32 . keccakBytes $ pub
                    assign (state . returndata . word256At 0) addr
                    assign (state . memory . word256At outOffset) addr
          _ -> vmError (BadCheatCode sig)

    ]
  where
    action s f = (abiKeccak s, f (Just $ abiKeccak s))

-- | Hack deterministic signing, totally insecure...
ethsign :: PrivateKey -> Digest Crypto.Keccak_256 -> Signature
ethsign sk digest = go 420
  where
    go k = case signDigestWith k sk digest of
       Nothing  -> go (k + 1)
       Just sig -> sig

-- * General call implementation ("delegateCall")
-- note that the continuation is ignored in the precompile case
delegateCall
  :: (?op :: Word8)
  => Contract -> W256 -> Expr EWord -> Expr EWord -> W256 -> W256 -> W256 -> W256 -> W256
  -> [Expr EWord]
  -> (Addr -> EVM ())
  -> EVM ()
delegateCall this gasGiven xTo xContext xValue xInOffset xInSize xOutOffset xOutSize xs continue =
  forceConcrete2 (fromIntegral xTo, fromIntegral xContext) "cannot delegateCall with symbolic target or context" $
    \((num -> xTo'), (num -> xContext')) ->
      if xTo' > 0 && xTo' <= 9
      then precompiledContract this gasGiven xTo' xContext' xValue xInOffset xInSize xOutOffset xOutSize xs
      else if num xTo' == cheatCode then
        do
          assign (state . stack) xs
          cheat (xInOffset, xInSize) (xOutOffset, xOutSize)
      else
        callChecks this gasGiven xContext' xTo' xValue xInOffset xInSize xOutOffset xOutSize xs $
        \xGas -> do
          vm0 <- get
          fetchAccount xTo' $ \target ->
                burn xGas $ do
                  let newContext = CallContext
                                    { callContextTarget    = xTo'
                                    , callContextContext   = xContext'
                                    , callContextOffset    = xOutOffset
                                    , callContextSize      = xOutSize
                                    , callContextCodehash  = view codehash target
                                    , callContextReversion = view (env . contracts) vm0
                                    , callContextSubState  = view (tx . substate) vm0
                                    , callContextAbi =
                                        if xInSize >= 4
                                        then case unlit $ readBytes 4 (Lit xInOffset) (view (state . memory) vm0)
                                             of Nothing -> Nothing
                                                Just abi -> Just $ num abi
                                        else Nothing
                                    , callContextData = (readMemory (num xInOffset) (num xInSize) vm0)
                                    }

                  pushTrace (FrameTrace newContext)
                  next
                  vm1 <- get

                  pushTo frames $ Frame
                    { _frameState = (set stack xs) (view state vm1)
                    , _frameContext = newContext
                    }

                  zoom state $ do
                    assign gas (num xGas)
                    assign pc 0
                    assign code (view bytecode target)
                    assign codeContract xTo'
                    assign stack mempty
                    assign memory mempty
                    assign memorySize 0
                    assign returndata mempty
                    assign calldata (copySlice (Lit xInOffset) (Lit 0) (Lit xInSize) (view (state . memory) vm0) EmptyBuf)

                  continue xTo'

-- -- * Contract creation

-- EIP 684
collision :: Maybe Contract -> Bool
collision c' = case c' of
  Just c -> (view nonce c /= 0) || case view contractcode c of
    -- TODO: is this correct???
    RuntimeCode b -> bufLength b /= (Lit 0)
    _ -> True
  Nothing -> False

create :: (?op :: Word8)
  => Addr -> Contract
  -> W256 -> W256 -> [Expr EWord] -> Addr -> Expr Buf -> EVM ()
create self this xGas' xValue xs newAddr initCode = do
  vm0 <- get
  let xGas = num xGas'
  if xValue > view balance this
  then do
    assign (state . stack) (0 : xs)
    assign (state . returndata) mempty
    pushTrace $ ErrorTrace $ BalanceTooLow xValue (view balance this)
    next
  else if length (view frames vm0) >= 1024
  then do
    assign (state . stack) (0 : xs)
    assign (state . returndata) mempty
    pushTrace $ ErrorTrace CallDepthLimitReached
    next
  else if collision $ view (env . contracts . at newAddr) vm0
  then burn xGas $ do
    assign (state . stack) (0 : xs)
    modifying (env . contracts . ix self . nonce) succ
    next
  else burn xGas $ do
    touchAccount self
    touchAccount newAddr
    let
      newContract =
        initialContract (InitCode initCode) & set storage EmptyStore
      newContext  =
        CreationContext { creationContextAddress   = newAddr
                        , creationContextCodehash  = view codehash newContract
                        , creationContextReversion = view (env . contracts) vm0
                        , creationContextSubstate  = view (tx . substate) vm0
                        }

    zoom (env . contracts) $ do
      oldAcc <- use (at newAddr)
      let oldBal = maybe 0 (view balance) oldAcc

      assign (at newAddr) (Just (newContract & balance .~ oldBal))
      modifying (ix self . nonce) succ

    transfer self newAddr xValue

    pushTrace (FrameTrace newContext)
    next
    vm1 <- get
    pushTo frames $ Frame
      { _frameContext = newContext
      , _frameState   = (set stack xs) (view state vm1)
      }

    assign state $
      blankState
        & set contract   newAddr
        & set codeContract newAddr
        & set code       initCode
        & set callvalue  (Lit xValue)
        & set caller     (litAddr self)
        & set gas        xGas'

-- | Replace a contract's code, like when CREATE returns
-- from the constructor code.
replaceCode :: Addr -> ContractCode -> EVM ()
replaceCode target newCode =
  zoom (env . contracts . at target) $
    get >>= \case
      Just now -> case (view contractcode now) of
        InitCode _ ->
          put . Just $
          initialContract newCode
          & set storage (view storage now)
          & set balance (view balance now)
          & set nonce   (view nonce now)
        RuntimeCode _ ->
          error ("internal error: can't replace code of deployed contract " <> show target)
      Nothing ->
        error "internal error: can't replace code of nonexistent contract"

replaceCodeOfSelf :: ContractCode -> EVM ()
replaceCodeOfSelf newCode = do
  vm <- get
  replaceCode (view (state . contract) vm) newCode

resetState :: EVM ()
resetState = do
  assign result Nothing
  assign frames []
  assign state  blankState


-- * VM error implementation

vmError :: Error -> EVM ()
vmError e = finishFrame (FrameErrored e)

underrun :: EVM ()
underrun = vmError StackUnderrun

-- | A stack frame can be popped in three ways.
data FrameResult
  = FrameReturned (Expr Buf) -- ^ STOP, RETURN, or no more code
  | FrameReverted (Expr Buf) -- ^ REVERT
  | FrameErrored Error -- ^ Any other error
  deriving Show

-- | This function defines how to pop the current stack frame in either of
-- the ways specified by 'FrameResult'.
--
-- It also handles the case when the current stack frame is the only one;
-- in this case, we set the final '_result' of the VM execution.
finishFrame :: FrameResult -> EVM ()
finishFrame how = do
  oldVm <- get

  case view frames oldVm of
    -- Is the current frame the only one?
    [] -> do
      case how of
          FrameReturned output -> assign result . Just $ VMSuccess output
          FrameReverted buffer -> assign result . Just $ VMFailure (EVM.Revert buffer)
          FrameErrored e       -> assign result . Just $ VMFailure e
      finalize

    -- Are there some remaining frames?
    nextFrame : remainingFrames -> do

      -- Insert a debug trace.
      insertTrace $
        case how of
          FrameErrored e ->
            ErrorTrace e
          FrameReverted e ->
            ErrorTrace (EVM.Revert e)
          FrameReturned output ->
            ReturnTrace output (view frameContext nextFrame)
      -- Pop to the previous level of the debug trace stack.
      popTrace

      -- Pop the top frame.
      assign frames remainingFrames
      -- Install the state of the frame to which we shall return.
      assign state (view frameState nextFrame)

      -- When entering a call, the gas allowance is counted as burned
      -- in advance; this unburns the remainder and adds it to the
      -- parent frame.
      let remainingGas = view (state . gas) oldVm
          reclaimRemainingGasAllowance = do
            modifying burned (subtract remainingGas)
            modifying (state . gas) (+ remainingGas)

      -- Now dispatch on whether we were creating or calling,
      -- and whether we shall return, revert, or error (six cases).
      case view frameContext nextFrame of

        -- Were we calling?
        CallContext _ _ (num -> outOffset) (num -> outSize) _ _ _ reversion substate' -> do

          -- Excerpt K.1. from the yellow paper:
          -- K.1. Deletion of an Account Despite Out-of-gas.
          -- At block 2675119, in the transaction 0xcf416c536ec1a19ed1fb89e4ec7ffb3cf73aa413b3aa9b77d60e4fd81a4296ba,
          -- an account at address 0x03 was called and an out-of-gas occurred during the call.
          -- Against the equation (197), this added 0x03 in the set of touched addresses, and this transaction turned σ[0x03] into ∅.

          -- In other words, we special case address 0x03 and keep it in the set of touched accounts during revert
          touched <- use (tx . substate . touchedAccounts)

          let
            substate'' = over touchedAccounts (maybe id cons (find (3 ==) touched)) substate'
            revertContracts = assign (env . contracts) reversion
            revertSubstate  = assign (tx . substate) substate''

          case how of
            -- Case 1: Returning from a call?
            FrameReturned output -> do
              assign (state . returndata) output
              copyCallBytesToMemory output outSize 0 outOffset
              reclaimRemainingGasAllowance
              push 1

            -- Case 2: Reverting during a call?
            FrameReverted output -> do
              revertContracts
              revertSubstate
              assign (state . returndata) output
              copyCallBytesToMemory output outSize 0 outOffset
              reclaimRemainingGasAllowance
              push 0

            -- Case 3: Error during a call?
            FrameErrored _ -> do
              revertContracts
              revertSubstate
              assign (state . returndata) mempty
              push 0
        -- Or were we creating?
        CreationContext _ _ reversion substate' -> do
          creator <- use (state . contract)
          let
            createe = view (state . contract) oldVm
            revertContracts = assign (env . contracts) reversion'
            revertSubstate  = assign (tx . substate) substate'

            -- persist the nonce through the reversion
            reversion' = (Map.adjust (over nonce (+ 1)) creator) reversion

          case how of
            -- Case 4: Returning during a creation?
            FrameReturned output -> do
                replaceCode createe (RuntimeCode output)
                assign (state . returndata) mempty
                reclaimRemainingGasAllowance
                push (num createe)

            -- Case 5: Reverting during a creation?
            FrameReverted output -> do
              revertContracts
              revertSubstate
              assign (state . returndata) output
              reclaimRemainingGasAllowance
              push 0

            -- Case 6: Error during a creation?
            FrameErrored _ -> do
              revertContracts
              revertSubstate
              assign (state . returndata) mempty
              push 0


-- * Memory helpers

accessUnboundedMemoryRange
  :: FeeSchedule Integer
  -> W256
  -> W256
  -> EVM ()
  -> EVM ()
accessUnboundedMemoryRange _ _ 0 continue = continue
accessUnboundedMemoryRange fees f l continue = do
  m0 <- num <$> use (state . memorySize)
  do
    let m1 = 32 * ceilDiv (max m0 (num f + num l)) 32
    burn (memoryCost fees m1 - memoryCost fees m0) $ do
      assign (state . memorySize) (num m1)
      continue

accessMemoryRange
  :: FeeSchedule Integer
  -> W256
  -> W256
  -> EVM ()
  -> EVM ()
accessMemoryRange _ _ 0 continue = continue
accessMemoryRange fees f l continue =
  if f + l < l
    then vmError IllegalOverflow
    else accessUnboundedMemoryRange fees f l continue

accessMemoryWord
  :: FeeSchedule Integer -> W256 -> EVM () -> EVM ()
accessMemoryWord fees x = accessMemoryRange fees x 32

copyBytesToMemory
  :: Expr Buf -> Expr EWord -> Expr EWord -> Expr EWord -> EVM ()
copyBytesToMemory bs size xOffset yOffset =
  if size == 0 then noop
  else do
    mem <- use (state . memory)
    assign (state . memory) $
      copySlice xOffset yOffset size bs mem

copyCallBytesToMemory
  :: Expr Buf -> Expr EWord -> Expr EWord -> Expr EWord -> EVM ()
copyCallBytesToMemory bs size xOffset yOffset =
  if size == 0 then noop
  else do
    mem <- use (state . memory)
    assign (state . memory) $
      copySlice xOffset yOffset (Expr.min size (bufLength bs)) bs mem

readMemory :: Expr EWord -> Expr EWord -> VM -> Expr Buf
readMemory offset size vm = copySlice offset (Lit 0) size (view (state . memory) vm) EmptyBuf

-- * Tracing

withTraceLocation
  :: (MonadState VM m) => TraceData -> m Trace
withTraceLocation x = do
  vm <- get
  let
    Just this =
      currentContract vm
  pure Trace
    { _traceData = x
    , _traceContract = this
    , _traceOpIx = fromMaybe 0 $ (view opIxMap this) Vector.!? (view (state . pc) vm)
    }

pushTrace :: TraceData -> EVM ()
pushTrace x = do
  trace <- withTraceLocation x
  modifying traces $
    \t -> Zipper.children $ Zipper.insert (Node trace []) t

insertTrace :: TraceData -> EVM ()
insertTrace x = do
  trace <- withTraceLocation x
  modifying traces $
    \t -> Zipper.nextSpace $ Zipper.insert (Node trace []) t

popTrace :: EVM ()
popTrace =
  modifying traces $
    \t -> case Zipper.parent t of
            Nothing -> error "internal error (trace root)"
            Just t' -> Zipper.nextSpace t'

zipperRootForest :: Zipper.TreePos Zipper.Empty a -> Forest a
zipperRootForest z =
  case Zipper.parent z of
    Nothing -> Zipper.toForest z
    Just z' -> zipperRootForest (Zipper.nextSpace z')

traceForest :: VM -> Forest Trace
traceForest = view (traces . to zipperRootForest)

traceTopLog :: (MonadState VM m) => Expr Logs -> m ()
traceTopLog (Log addr bytes topics _) = do
  trace <- withTraceLocation (EventTrace addr bytes topics)
  modifying traces $
    \t -> Zipper.nextSpace (Zipper.insert (Node trace []) t)

-- * Stack manipulation

push :: W256 -> EVM ()
push = pushSym . Lit . num

pushSym :: Expr EWord -> EVM ()
pushSym x = state . stack %= (x :)


stackOp1
  :: (?op :: Word8)
  => ((Expr EWord) -> Integer)
  -> ((Expr EWord) -> (Expr EWord))
  -> EVM ()
stackOp1 cost f =
  use (state . stack) >>= \case
    (x:xs) ->
      burn (cost x) $ do
        next
        let !y = f x
        state . stack .= y : xs
    _ ->
      underrun

stackOp2
  :: (?op :: Word8)
  => (((Expr EWord), (Expr EWord)) -> Integer)
  -> (((Expr EWord), (Expr EWord)) -> (Expr EWord))
  -> EVM ()
stackOp2 cost f =
  use (state . stack) >>= \case
    (x:y:xs) ->
      burn (cost (x, y)) $ do
        next
        state . stack .= f (x, y) : xs
    _ ->
      underrun

stackOp3
  :: (?op :: Word8)
  => (((Expr EWord), (Expr EWord), (Expr EWord)) -> Integer)
  -> (((Expr EWord), (Expr EWord), (Expr EWord)) -> (Expr EWord))
  -> EVM ()
stackOp3 cost f =
  use (state . stack) >>= \case
    (x:y:z:xs) ->
      burn (cost (x, y, z)) $ do
        next
        state . stack .= f (x, y, z) : xs
    _ ->
      underrun

-- * Bytecode data functions

checkJump :: (Integral n) => n -> [Expr EWord] -> EVM ()
checkJump x xs = do
  theCode <- use (state . code)
  branch (Expr.and
           (Expr.lt (Lit . num $ x) (bufLength theCode))
           (Expr.eqByte (LitByte 0x5b) (Expr.readByte (Lit . num $ x) theCode)))
         (\case
           True -> do
             state . stack .= xs
             state . pc .= num x
           False -> vmError BadJumpDestination)

opSize :: Word8 -> Int
opSize x | x >= 0x60 && x <= 0x7f = num x - 0x60 + 2
opSize _                          = 1

--  i of the resulting vector contains the operation index for
-- the program counter value i.  This is needed because source map
-- entries are per operation, not per byte.
--
-- Note that in some cases we have to deal with bytecode that may have an
-- abstract length (e.g. constructors that have dynamic data), in this case we
-- construct the opIxMap only up to the bytecode index past which we can be
-- sure that all data is symbolic. Since we do not support execution of
-- symbolic opcodes a source mapping for these regions is not required.
-- In the future it may be desirable to lift this implied structure to the type
-- level by introducing a distinction between buffers of known and unknown
-- length, and representing InitCode as a pair of (Buf KnownLength, Buf UnknownLength).
-- We avoid for know for the sake of keeping the type level reprsentation simple.
mkOpIxMap :: Expr Buf -> Vector Int
mkOpIxMap xs = case minLength xs of
  Nothing -> error "cannot operate on fully abstracted bytecode"
  Just l -> Vector.create $ Vector.new l >>= \v ->
  -- Loop over the byte string accumulating a vector-mutating action.
  -- This is somewhat obfuscated, but should be fast.
    case xs of
      ConcreteBuf xs' ->
        let (_, _, _, m) =
              BS.foldl' (go v) (0 :: Word8, 0, 0, return ()) xs'
        in m >> return v
      xs' ->
        let (_, _, _, m) =
              Expr.foldBufTo (go' v) (0, 0, 0, return ()) (fromIntegral l) (stripBytecodeMetadataSym xs')
        in m >> return v

    where
      -- concrete case
      go v (0, !i, !j, !m) x | x >= 0x60 && x <= 0x7f =
        {- Start of PUSH op. -} (x - 0x60 + 1, i + 1, j,     m >> Vector.write v i j)
      go v (1, !i, !j, !m) _ =
        {- End of PUSH op. -}   (0,            i + 1, j + 1, m >> Vector.write v i j)
      go v (0, !i, !j, !m) _ =
        {- Other op. -}         (0,            i + 1, j + 1, m >> Vector.write v i j)
      go v (n, !i, !j, !m) _ =
        {- PUSH data. -}        (n - 1,        i + 1, j,     m >> Vector.write v i j)

      -- symbolic case
      go' v (0, !i, !j, !m) x = case unlitByte x of
        Just x' -> if x' >= 0x60 && x' <= 0x7f
          -- start of PUSH op --
                   then (x' - 0x60 + 1, i + 1, j,     m >> Vector.write v i j)
          -- other data --
                   else (0,             i + 1, j + 1, m >> Vector.write v i j)
        _ -> error "cannot analyze symbolic code"

        {- Start of PUSH op. -} (x - 0x60 + 1, i + 1, j,     m >> Vector.write v i j)
      go' v (1, !i, !j, !m) _ =
        {- End of PUSH op. -}   (0,            i + 1, j + 1, m >> Vector.write v i j)
      go' v (n, !i, !j, !m) _ =
        {- PUSH data. -}        (n - 1,        i + 1, j,     m >> Vector.write v i j)

vmOp :: VM -> Maybe Op
vmOp vm =
  let i  = vm ^. state . pc
      code' = vm ^. state . code
      data' = WriteWord 0 (readWord (Lit . num $ i) code') EmptyBuf
  in do
    l <- minLength code'
    when (l < i) Nothing
    case readByte (Lit . num $ i) code' of
      LitByte op -> Just $ readOp op data'
      _ -> Nothing

vmOpIx :: VM -> Maybe Int
vmOpIx vm =
  do self <- currentContract vm
     (view opIxMap self) Vector.!? (view (state . pc) vm)

opParams :: VM -> Map String (Expr EWord)
opParams vm =
  case vmOp vm of
    Just OpCreate ->
      params $ words "value offset size"
    Just OpCall ->
      params $ words "gas to value in-offset in-size out-offset out-size"
    Just OpSstore ->
      params $ words "index value"
    Just OpCodecopy ->
      params $ words "mem-offset code-offset code-size"
    Just OpSha3 ->
      params $ words "offset size"
    Just OpCalldatacopy ->
      params $ words "to from size"
    Just OpExtcodecopy ->
      params $ words "account mem-offset code-offset code-size"
    Just OpReturn ->
      params $ words "offset size"
    Just OpJumpi ->
      params $ words "destination condition"
    _ -> mempty
  where
    params xs =
      if length (vm ^. state . stack) >= length xs
      then Map.fromList (zip xs (vm ^. state . stack))
      else mempty

-- | Reads
readOp :: Word8 -> Expr Buf -> Op
readOp x _  | x >= 0x80 && x <= 0x8f = OpDup (x - 0x80 + 1)
readOp x _  | x >= 0x90 && x <= 0x9f = OpSwap (x - 0x90 + 1)
readOp x _  | x >= 0xa0 && x <= 0xa4 = OpLog (x - 0xa0)
readOp x xs | x >= 0x60 && x <= 0x7f =
  let n = num $ x - 0x60 + 1
  in OpPush (readBytes n (Lit . num $ x) xs)
readOp x _ = case x of
  0x00 -> OpStop
  0x01 -> OpAdd
  0x02 -> OpMul
  0x03 -> OpSub
  0x04 -> OpDiv
  0x05 -> OpSdiv
  0x06 -> OpMod
  0x07 -> OpSmod
  0x08 -> OpAddmod
  0x09 -> OpMulmod
  0x0a -> OpExp
  0x0b -> OpSignextend
  0x10 -> OpLt
  0x11 -> OpGt
  0x12 -> OpSlt
  0x13 -> OpSgt
  0x14 -> OpEq
  0x15 -> OpIszero
  0x16 -> OpAnd
  0x17 -> OpOr
  0x18 -> OpXor
  0x19 -> OpNot
  0x1a -> OpByte
  0x1b -> OpShl
  0x1c -> OpShr
  0x1d -> OpSar
  0x20 -> OpSha3
  0x30 -> OpAddress
  0x31 -> OpBalance
  0x32 -> OpOrigin
  0x33 -> OpCaller
  0x34 -> OpCallvalue
  0x35 -> OpCalldataload
  0x36 -> OpCalldatasize
  0x37 -> OpCalldatacopy
  0x38 -> OpCodesize
  0x39 -> OpCodecopy
  0x3a -> OpGasprice
  0x3b -> OpExtcodesize
  0x3c -> OpExtcodecopy
  0x3d -> OpReturndatasize
  0x3e -> OpReturndatacopy
  0x3f -> OpExtcodehash
  0x40 -> OpBlockhash
  0x41 -> OpCoinbase
  0x42 -> OpTimestamp
  0x43 -> OpNumber
  0x44 -> OpDifficulty
  0x45 -> OpGaslimit
  0x46 -> OpChainid
  0x47 -> OpSelfbalance
  0x50 -> OpPop
  0x51 -> OpMload
  0x52 -> OpMstore
  0x53 -> OpMstore8
  0x54 -> OpSload
  0x55 -> OpSstore
  0x56 -> OpJump
  0x57 -> OpJumpi
  0x58 -> OpPc
  0x59 -> OpMsize
  0x5a -> OpGas
  0x5b -> OpJumpdest
  0xf0 -> OpCreate
  0xf1 -> OpCall
  0xf2 -> OpCallcode
  0xf3 -> OpReturn
  0xf4 -> OpDelegatecall
  0xf5 -> OpCreate2
  0xfd -> OpRevert
  0xfa -> OpStaticcall
  0xff -> OpSelfdestruct
  _    -> OpUnknown x

-- Maps operation indicies into a pair of (bytecode index, operation)
mkCodeOps :: Expr Buf -> RegularVector.Vector (Int, Op)
mkCodeOps (ConcreteBuf bytes) = RegularVector.fromList . toList $ go 0 bytes
  where
    go !i !xs =
      case BS.uncons xs of
        Nothing ->
          mempty
        Just (x, xs') ->
          let j = opSize x
          in (i, readOp x (ConcreteBuf xs')) Seq.<| go (i + j) (BS.drop j xs)
mkCodeOps buf = RegularVector.fromList . toList $ go' 0 (stripBytecodeMetadataSym buf)
  where
    go' :: Int -> Expr Buf -> Seq (Int, Op)
    go' !i b | i > fromMaybe (error "cannot analyze fully abstract code") (minLength b) = mempty
    go' !i b = let
        op = fromMaybe (error "unexpected symbolic code argument") . unlitByte $ readByte (Lit . num $ i) b
        sz = opSize op
      in (i, readOp op buf) Seq.<| go' (i + sz) b

-- * Gas cost calculation helpers

-- Gas cost function for CALL, transliterated from the Yellow Paper.
costOfCall
  :: FeeSchedule Integer
  -> Bool -> W256 -> W256 -> W256 -> Addr
  -> EVM (Integer, Integer)
costOfCall (FeeSchedule {..}) recipientExists xValue availableGas' xGas' target = do
  acc <- accessAccountForGas target
  let call_base_gas = if acc then g_warm_storage_read else g_cold_account_access
      availableGas = num availableGas'
      xGas = num xGas'
      c_new = if not recipientExists && xValue /= 0
            then num g_newaccount
            else 0
      c_xfer = if xValue /= 0  then num g_callvalue else 0
      c_extra = num call_base_gas + c_xfer + c_new
      c_gascap =  if availableGas >= c_extra
                  then min xGas (allButOne64th (availableGas - c_extra))
                  else xGas
      c_callgas = if xValue /= 0  then c_gascap + num g_callstipend else c_gascap
  return (c_gascap + c_extra, c_callgas)

-- Gas cost of create, including hash cost if needed
costOfCreate
  :: FeeSchedule Integer
  -> W256 -> W256 -> (Integer, Integer)
costOfCreate (FeeSchedule {..}) availableGas' hashSize =
  (createCost + initGas, initGas)
  where
    availableGas = num availableGas'
    createCost = g_create + hashCost
    hashCost   = g_sha3word * ceilDiv (num hashSize) 32
    initGas    = allButOne64th (availableGas - createCost)

concreteModexpGasFee :: ByteString -> Integer
concreteModexpGasFee input = max 200 ((multiplicationComplexity * iterCount) `div` 3)
  where (lenb, lene, lenm) = parseModexpLength input
        ez = isZero (96 + lenb) lene input
        e' = word $ LS.toStrict $
          lazySlice (96 + lenb) (min 32 lene) input
        nwords :: Integer
        nwords = ceilDiv (num $ max lenb lenm) 8
        multiplicationComplexity = nwords * nwords
        iterCount' :: Integer
        iterCount' | lene <= 32 && ez = 0
                   | lene <= 32 = num (log2 e')
                   | e' == 0 = 8 * (num lene - 32)
                   | otherwise = num (log2 e') + 8 * (num lene - 32)
        iterCount = max iterCount' 1

-- Gas cost of precompiles
costOfPrecompile :: FeeSchedule Integer -> Addr -> Expr Buf -> Integer
costOfPrecompile (FeeSchedule {..}) precompileAddr input =
  case precompileAddr of
    -- ECRECOVER
    0x1 -> 3000
    -- SHA2-256
    0x2 -> num $ (((bufLength input + 31) `div` 32) * 12) + 60
    -- RIPEMD-160
    0x3 -> num $ (((bufLength input + 31) `div` 32) * 120) + 600
    -- IDENTITY
    0x4 -> num $ (((bufLength input + 31) `div` 32) * 3) + 15
    -- MODEXP
    0x5 -> case input of
             ConcreteBuf i -> concreteModexpGasFee i
             _ -> error "Unsupported symbolic modexp gas calc"
    -- ECADD
    0x6 -> g_ecadd
    -- ECMUL
    0x7 -> g_ecmul
    -- ECPAIRING
    0x8 -> num $ ((bufLength input) `div` 192) * (num g_pairing_point) + (num g_pairing_base)
    -- BLAKE2
    0x9 -> case input of
             ConcreteBuf i -> g_fround * (num $ asInteger $ lazySlice 0 4 i)
             _ -> error "Unsupported symbolic blake2 gas calc"
    _ -> error ("unimplemented precompiled contract " ++ show precompileAddr)

-- Gas cost of memory expansion
memoryCost :: FeeSchedule Integer -> Integer -> Integer
memoryCost FeeSchedule{..} byteCount =
  let
    wordCount = ceilDiv byteCount 32
    linearCost = g_memory * wordCount
    quadraticCost = div (wordCount * wordCount) 512
  in
    linearCost + quadraticCost

-- * Arithmetic

ceilDiv :: (Num a, Integral a) => a -> a -> a
ceilDiv m n = div (m + n - 1) n

allButOne64th :: (Num a, Integral a) => a -> a
allButOne64th n = n - div n 64

log2 :: FiniteBits b => b -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x


-- * Emacs setup

-- Local Variables:
-- outline-regexp: "-- \\*+\\|data \\|newtype \\|type \\| +-- op: "
-- outline-heading-alist:
--   (("-- *" . 1) ("data " . 2) ("newtype " . 2) ("type " . 2))
-- compile-command: "make"
-- End:
