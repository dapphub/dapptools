{-# Language ImplicitParams #-}
{-# Language ConstraintKinds #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

module EVM where

import Prelude hiding ((^), log, Word, exponent)

import EVM.ABI
import EVM.Types
import EVM.Solidity
import EVM.Keccak
import EVM.Concrete
import EVM.Op
import EVM.FeeSchedule (FeeSchedule (..))

import qualified EVM.Precompiled

import Data.Binary.Get (runGetOrFail)
import Data.Bits (bit, testBit, complement)
import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.), FiniteBits (..))
import Data.Text (Text)
import Data.Word (Word8, Word32)

import Control.Lens hiding (op, (:<), (|>))
import Control.Monad.State.Strict hiding (state)

import Data.ByteString              (ByteString)
import Data.ByteString.Lazy         (fromStrict)
import Data.Map.Strict              (Map)
import Data.Maybe                   (fromMaybe, isNothing)
import Data.Semigroup               (Semigroup (..))
import Data.Sequence                (Seq)
import Data.Vector.Storable         (Vector)
import Data.Foldable                (toList)

import Data.Tree

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Data.Tree.Zipper     as Zipper
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as Vector

import qualified Data.Vector as RegularVector

-- * Data types

data Error
  = BalanceTooLow Word Word
  | UnrecognizedOpcode Word8
  | SelfDestruction
  | StackUnderrun
  | BadJumpDestination
  | Revert ByteString
  | NoSuchContract Addr
  | OutOfGas Word Word
  | BadCheatCode Word32
  | StackLimitExceeded
  | IllegalOverflow
  | Query Query
  | PrecompiledContractError Int
  | StateChangeWhileStatic

deriving instance Show Error

-- | The possible result states of a VM
data VMResult
  = VMFailure Error -- ^ An operation failed
  | VMSuccess ByteString -- ^ Reached STOP, RETURN, or end-of-code

deriving instance Show VMResult

-- | The state of a stepwise EVM execution
data VM = VM
  { _result        :: Maybe VMResult
  , _state         :: FrameState
  , _frames        :: [Frame]
  , _env           :: Env
  , _block         :: Block
  , _tx            :: TxState
  , _logs          :: Seq Log
  , _traces        :: Zipper.TreePos Zipper.Empty Trace
  , _cache         :: Cache
  , _execMode      :: ExecMode
  , _burned        :: Word
  }

data Trace = Trace
  { _traceCodehash :: W256
  , _traceOpIx     :: Int
  , _traceData     :: TraceData
  }

data TraceData
  = EventTrace Log
  | FrameTrace FrameContext
  | QueryTrace Query
  | ErrorTrace Error
  | EntryTrace Text
  | ReturnTrace ByteString FrameContext

data ExecMode = ExecuteNormally | ExecuteAsVMTest

data Query where
  PleaseFetchContract :: Addr         -> (Contract -> EVM ()) -> Query
  PleaseFetchSlot     :: Addr -> Word -> (Word     -> EVM ()) -> Query

instance Show Query where
  showsPrec _ = \case
    PleaseFetchContract addr _ ->
      (("<EVM.Query: fetch contract " ++ show addr ++ ">") ++)
    PleaseFetchSlot addr slot _ ->
      (("<EVM.Query: fetch slot "
        ++ show slot ++ " for "
        ++ show addr ++ ">") ++)

-- | Alias for the type of e.g. @exec1@.
type EVM a = State VM a

-- | The cache is data that can be persisted for efficiency:
-- any expensive query that is constant at least within a block.
data Cache = Cache
  { _fetched :: Map Addr Contract
  }

-- | A way to specify an initial VM state
data VMOpts = VMOpts
  { vmoptCode :: ByteString
  , vmoptCalldata :: ByteString
  , vmoptValue :: W256
  , vmoptAddress :: Addr
  , vmoptCaller :: Addr
  , vmoptOrigin :: Addr
  , vmoptGas :: W256
  , vmoptNumber :: W256
  , vmoptTimestamp :: W256
  , vmoptCoinbase :: Addr
  , vmoptDifficulty :: W256
  , vmoptGaslimit :: W256
  , vmoptGasprice :: W256
  , vmoptSchedule :: FeeSchedule Word
  } deriving Show

-- | A log entry
data Log = Log Addr ByteString [Word]

-- | An entry in the VM's "call/create stack"
data Frame = Frame
  { _frameContext   :: FrameContext
  , _frameState     :: FrameState
  }

-- | Call/create info
data FrameContext
  = CreationContext
    { creationContextCodehash :: W256 }
  | CallContext
    { callContextOffset   :: Word
    , callContextSize     :: Word
    , callContextCodehash :: W256
    , callContextAbi      :: Maybe Word
    , callContextData     :: ByteString
    , callContextReversion :: Map Addr Contract
    }

-- | The "registers" of the VM along with memory and data stack
data FrameState = FrameState
  { _contract     :: Addr
  , _codeContract :: Addr
  , _code         :: ByteString
  , _pc           :: Int
  , _stack        :: [Word]
  , _memory       :: ByteString
  , _memorySize   :: Int
  , _calldata     :: ByteString
  , _callvalue    :: Word
  , _caller       :: Addr
  , _gas          :: Word
  , _returndata   :: ByteString
  , _static       :: Bool
  }

-- | The state that spans a whole transaction
data TxState = TxState
  { _selfdestructs :: [Addr]
  , _refunds       :: [(Addr, Word)]
  }

-- | A contract is either in creation (running its "constructor") or
-- post-creation, and code in these two modes is treated differently
-- by instructions like @EXTCODEHASH@, so we distinguish these two
-- code types.
data ContractCode
  = InitCode ByteString     -- ^ "Constructor" code, during contract creation
  | RuntimeCode ByteString  -- ^ "Instance" code, after contract creation
  deriving (Show, Eq)

-- | The state of a contract
data Contract = Contract
  { _bytecode :: ContractCode
  , _storage  :: Map Word Word
  , _balance  :: Word
  , _nonce    :: Word
  , _codehash :: W256
  , _opIxMap  :: Vector Int
  , _codeOps  :: RegularVector.Vector (Int, Op)
  , _external :: Bool
  }

deriving instance Show Contract
deriving instance Eq Contract

-- | Various environmental data
data Env = Env
  { _contracts :: Map Addr Contract
  , _sha3Crack :: Map Word ByteString
  , _origin    :: Addr
  }


-- | Data about the block
data Block = Block
  { _coinbase   :: Addr
  , _timestamp  :: Word
  , _number     :: Word
  , _difficulty :: Word
  , _gaslimit   :: Word
  , _gasprice   :: Word
  , _schedule   :: FeeSchedule Word
  }

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
makeLenses ''Contract
makeLenses ''Env
makeLenses ''Cache
makeLenses ''Trace
makeLenses ''VM

-- | An "external" view of a contract's bytecode, appropriate for
-- e.g. @EXTCODEHASH@.
bytecodeE :: Getter Contract ByteString
bytecodeE = bytecode . to f
  where f (InitCode _)    = BS.empty
        f (RuntimeCode b) = b

-- | An "internal" view of a contract's bytecode, appropriate for
-- e.g. @CODECOPY@.
bytecodeI :: Getter Contract ByteString
bytecodeI = bytecode . to f
  where f (InitCode b)    = b
        f (RuntimeCode b) = b

instance Semigroup Cache where
  a <> b = Cache
    { _fetched = mappend (view fetched a) (view fetched b) }

instance Monoid Cache where
  mempty = Cache { _fetched = mempty }

-- * Data accessors

currentContract :: VM -> Maybe Contract
currentContract vm =
  view (env . contracts . at (view (state . codeContract) vm)) vm

-- * Data constructors

makeVm :: VMOpts -> VM
makeVm o = VM
  { _result = Nothing
  , _frames = mempty
  , _tx = TxState
    { _selfdestructs = mempty
    , _refunds = mempty
    }
  , _logs = mempty
  , _traces = Zipper.fromForest []
  , _block = Block
    { _coinbase = vmoptCoinbase o
    , _timestamp = w256 $ vmoptTimestamp o
    , _number = w256 $ vmoptNumber o
    , _difficulty = w256 $ vmoptDifficulty o
    , _gaslimit = w256 $ vmoptGaslimit o
    , _gasprice = w256 $ vmoptGasprice o
    , _schedule = vmoptSchedule o
    }
  , _state = FrameState
    { _pc = 0
    , _stack = mempty
    , _memory = mempty
    , _memorySize = 0
    , _code = vmoptCode o
    , _contract = vmoptAddress o
    , _codeContract = vmoptAddress o
    , _calldata = vmoptCalldata o
    , _callvalue = w256 $ vmoptValue o
    , _caller = vmoptCaller o
    , _gas = w256 $ vmoptGas o
    , _returndata = mempty
    , _static = False
    }
  , _env = Env
    { _sha3Crack = mempty
    , _origin = vmoptOrigin o
    , _contracts = Map.fromList
      [(vmoptAddress o, initialContract (RuntimeCode (vmoptCode o)))]
    }
  , _cache = mempty
  , _execMode = ExecuteNormally
  , _burned = 0
  }

initialContract :: ContractCode -> Contract
initialContract theContractCode = Contract
  { _bytecode = theContractCode
  , _codehash =
    if BS.null theCode then 0 else
      keccak (stripBytecodeMetadata theCode)
  , _storage  = mempty
  , _balance  = 0
  , _nonce    = 0
  , _opIxMap  = mkOpIxMap theCode
  , _codeOps  = mkCodeOps theCode
  , _external = False
  } where theCode = case theContractCode of
            InitCode b    -> b
            RuntimeCode b -> b

-- * Opcode dispatch (exec1)

next :: (?op :: Word8) => EVM ()
next = modifying (state . pc) (+ (opSize ?op))

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
    this = fromMaybe (error "internal error: state contract") (preview (ix (the state contract)) (the env contracts))

    fees@(FeeSchedule {..}) = the block schedule

    doStop = finishFrame (FrameReturned "")

  if the state pc >= num (BS.length (the state code))
    then
      doStop

    else do
      let ?op = BS.index (the state code) (the state pc)

      case ?op of

        -- op: PUSH
        x | x >= 0x60 && x <= 0x7f -> do
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + the state pc)
                                       (the state code))
          limitStack 1 $
            burn g_verylow $ do
              next
              push (w256 (word xs))

        -- op: DUP
        x | x >= 0x80 && x <= 0x8f -> do
          let !i = x - 0x80 + 1
          case preview (ix (num i - 1)) stk of
            Nothing -> underrun
            Just y -> do
              limitStack 1 $
                burn g_verylow $ do
                  next
                  push y

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
            (xOffset:xSize:xs) ->
              if length xs < n
              then underrun
              else do
                let (topics, xs') = splitAt n xs
                    bytes         = readMemory (num xOffset) (num xSize) vm
                    log           = Log self bytes topics

                burn (g_log + g_logdata * xSize + num n * g_logtopic) $ do
                  accessMemoryRange fees xOffset xSize $ do
                    traceLog log
                    next
                    assign (state . stack) xs'
                    pushToSequence logs log
            _ ->
              underrun

        -- op: STOP
        0x00 -> doStop

        -- op: ADD
        0x01 -> stackOp2 (const g_verylow) (uncurry (+))
        -- op: MUL
        0x02 -> stackOp2 (const g_low) (uncurry (*))
        -- op: SUB
        0x03 -> stackOp2 (const g_verylow) (uncurry (-))

        -- op: DIV
        0x04 -> stackOp2 (const g_low) $
          \case (_, 0) -> 0
                (x, y) -> div x y

        -- op: SDIV
        0x05 ->
          stackOp2 (const g_low) (uncurry (sdiv))

        -- op: MOD
        0x06 -> stackOp2 (const g_low) $ \case
          (_, 0) -> 0
          (x, y) -> mod x y

        -- op: SMOD
        0x07 -> stackOp2 (const g_low) $ uncurry smod
        -- op: ADDMOD
        0x08 -> stackOp3 (const g_mid) $ (\(x, y, z) -> addmod x y z)
        -- op: MULMOD
        0x09 -> stackOp3 (const g_mid) $ (\(x, y, z) -> mulmod x y z)

        -- op: LT
        0x10 -> stackOp2 (const g_verylow) $ \(x, y) -> if x < y then 1 else 0
        -- op: GT
        0x11 -> stackOp2 (const g_verylow) $ \(x, y) -> if x > y then 1 else 0
        -- op: SLT
        0x12 -> stackOp2 (const g_verylow) $ uncurry slt
        -- op: SGT
        0x13 -> stackOp2 (const g_verylow) $ uncurry sgt

        -- op: EQ
        0x14 -> stackOp2 (const g_verylow) $ \(x, y) -> if x == y then 1 else 0
        -- op: ISZERO
        0x15 -> stackOp1 (const g_verylow) $ \case 0 -> 1; _ -> 0

        -- op: AND
        0x16 -> stackOp2 (const g_verylow) $ uncurry (.&.)
        -- op: OR
        0x17 -> stackOp2 (const g_verylow) $ uncurry (.|.)
        -- op: XOR
        0x18 -> stackOp2 (const g_verylow) $ uncurry xor
        -- op: NOT
        0x19 -> stackOp1 (const g_verylow) complement

        -- op: BYTE
        0x1a -> stackOp2 (const g_verylow) $ \case
          (n, _) | n >= 32 ->
            0
          (n, x) ->
            0xff .&. shiftR x (8 * (31 - num n))

        -- op: SHL
        0x1b -> stackOp2 (const g_verylow) $ \(n, x) -> shiftL x (num n)
        -- op: SHR
        0x1c -> stackOp2 (const g_verylow) $ \(n, x) -> shiftR x (num n)
        -- op: SAR
        0x1d -> stackOp2 (const g_verylow) $ uncurry sar

        -- op: SHA3
        0x20 ->
          case stk of
            ((num -> xOffset) : (num -> xSize) : xs) -> do
              let bytes = readMemory xOffset xSize vm
                  hash  = keccakBlob bytes
              burn (g_sha3 + g_sha3word * ceilDiv (num xSize) 32) $
                accessMemoryRange fees xOffset xSize $ do
                  next
                  assign (state . stack) (hash : xs)
                  assign (env . sha3Crack . at hash) (Just bytes)
            _ -> underrun

        -- op: ADDRESS
        0x30 ->
          limitStack 1 $
            burn g_base (next >> push (num (the state contract)))

        -- op: BALANCE
        0x31 ->
          case stk of
            (x:xs) -> do
              burn g_balance $ do
                touchAccount (num x) $ \c -> do
                  next
                  assign (state . stack) xs
                  push (view balance c)
            [] ->
              underrun

        -- op: ORIGIN
        0x32 ->
          limitStack 1 . burn g_base $
            next >> push (num (the env origin))

        -- op: CALLER
        0x33 ->
          limitStack 1 . burn g_base $
            next >> push (num (the state caller))

        -- op: CALLVALUE
        0x34 ->
          limitStack 1 . burn g_base $
            next >> push (the state callvalue)

        -- op: CALLDATALOAD
        0x35 -> stackOp1 (const g_verylow) $
          \x -> readBlobWord x (the state calldata)

        -- op: CALLDATASIZE
        0x36 ->
          limitStack 1 . burn g_base $
            next >> push (blobSize (the state calldata))

        -- op: CALLDATACOPY
        0x37 ->
          case stk of
            ((num -> xTo) : (num -> xFrom) : (num -> xSize) :xs) -> do
              burn (g_verylow + g_copy * ceilDiv xSize 32) $ do
                accessMemoryRange fees xTo xSize $ do
                  next
                  assign (state . stack) xs
                  copyBytesToMemory (the state calldata) xSize xFrom xTo
            _ -> underrun

        -- TODO: the asymmetry between CODESIZE and CODECOPY is a little irritating
        -- op: CODESIZE
        0x38 ->
          limitStack 1 . burn g_base $
            next >> push (num (BS.length (the state code)))

        -- op: CODECOPY
        0x39 ->
          case stk of
            ((num -> memOffset) : (num -> codeOffset) : (num -> n) : xs) -> do
              burn (g_verylow + g_copy * ceilDiv (num n) 32) $ do
                accessMemoryRange fees memOffset n $ do
                  next
                  assign (state . stack) xs
                  copyBytesToMemory (view bytecodeI this)
                    n codeOffset memOffset
            _ -> underrun

        -- op: GASPRICE
        0x3a ->
          limitStack 1 . burn g_base $
            next >> push (the block gasprice)

        -- op: EXTCODESIZE
        0x3b ->
          case stk of
            (x:xs) -> do
              if x == num cheatCode
                then do
                  next
                  assign (state . stack) xs
                  push (w256 1)
                else
                  burn g_extcode $ do
                    touchAccount (num x) $ \c -> do
                      next
                      assign (state . stack) xs
                      push (num (BS.length (view bytecodeE c)))
            [] ->
              underrun

        -- op: EXTCODECOPY
        0x3c ->
          case stk of
            ( extAccount
              : (num -> memOffset)
              : (num -> codeOffset)
              : (num -> codeSize)
              : xs ) -> do
              burn (g_extcode + g_copy * ceilDiv (num codeSize) 32) $
                accessMemoryRange fees memOffset codeSize $ do
                  touchAccount (num extAccount) $ \c -> do
                    next
                    assign (state . stack) xs
                    copyBytesToMemory (view bytecodeE c)
                      codeSize codeOffset memOffset
            _ -> underrun

        -- op: RETURNDATASIZE
        0x3d ->
          limitStack 1 . burn g_base $
            next >> push (blobSize (the state returndata))

        -- op: RETURNDATACOPY
        0x3e ->
          case stk of
            ((num -> xTo) : (num -> xFrom) : (num -> xSize) :xs) -> do
              burn (g_verylow + g_copy * ceilDiv xSize 32) $ do
                accessMemoryRange fees xTo xSize $ do
                  next
                  assign (state . stack) xs
                  copyBytesToMemory (the state returndata) xSize xFrom xTo
            _ -> underrun

        -- op: EXTCODEHASH
        0x3f ->
          case stk of
            (x:xs) -> do
              burn g_extcodehash $ do
                next
                assign (state . stack) xs
                preuse (env . contracts . ix (num x)) >>=
                  \case
                    Nothing -> push (num (0 :: Int))
                    Just c  -> push (num (keccak (view bytecodeE c)))
            [] ->
              underrun

        -- op: BLOCKHASH
        0x40 -> do
          -- We adopt the fake block hash scheme of the VMTests,
          -- so that blockhash(i) is the hash of i as decimal ASCII.
          stackOp1 (const g_blockhash) $
            \i ->
              if i + 256 < the block number || i >= the block number
              then 0
              else
                (num i :: Integer)
                  & show & Char8.pack & keccak & num

        -- op: COINBASE
        0x41 ->
          limitStack 1 . burn g_base $
            next >> push (num (the block coinbase))

        -- op: TIMESTAMP
        0x42 ->
          limitStack 1 . burn g_base $
            next >> push (the block timestamp)

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

        -- op: POP
        0x50 ->
          case stk of
            (_:xs) -> burn g_base (next >> assign (state . stack) xs)
            _      -> underrun

        -- op: MLOAD
        0x51 ->
          case stk of
            (x:xs) -> do
              burn g_verylow $
                accessMemoryWord fees x $ do
                  next
                  assign (state . stack) (view (word256At (num x)) mem : xs)
            _ -> underrun

        -- op: MSTORE
        0x52 ->
          case stk of
            (x:y:xs) -> do
              burn g_verylow $
                accessMemoryWord fees x $ do
                  next
                  assign (state . memory . word256At (num x)) y
                  assign (state . stack) xs
            _ -> underrun

        -- op: MSTORE8
        0x53 ->
          case stk of
            (x:y:xs) -> do
              burn g_verylow $
                accessMemoryRange fees x 1 $ do
                  next
                  modifying (state . memory) (setMemoryByte x (wordToByte y))
                  assign (state . stack) xs
            _ -> underrun

        -- op: SLOAD
        0x54 ->
          case stk of
            (x:xs) ->
              burn g_sload $
                accessStorage self x $ \y -> do
                  next
                  assign (state . stack) (y:xs)
            _ -> underrun

        -- op: SSTORE
        0x55 ->
          notStatic $
          case stk of
            (x:new:xs) -> do
              accessStorage self x $ \old -> do
                -- Gas cost is higher when changing from zero to nonzero.
                let cost = if old == 0 && new /= 0 then g_sset else g_sreset

                burn cost $ do
                  next
                  assign (state . stack) xs
                  assign (env . contracts . ix (the state contract) . storage . at x)
                    (Just new)

                  -- Give gas refund if clearing the storage slot.
                  if old /= 0 && new == 0 then refund r_sclear else noop

            _ -> underrun

        -- op: JUMP
        0x56 ->
          case stk of
            (x:xs) -> do
              burn g_mid $ do
                checkJump x xs
            _ -> underrun

        -- op: JUMPI
        0x57 -> do
          case stk of
            (x:y:xs) -> do
              burn g_high $ do
                if y == 0
                  then assign (state . stack) xs >> next
                  else checkJump x xs
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
            next >> push (the state gas - g_base)

        -- op: JUMPDEST
        0x5b -> burn g_jumpdest next

        -- op: EXP
        0x0a ->
          let cost (_, exponent) =
                if exponent == 0
                then g_exp
                else g_exp + g_expbyte * num (ceilDiv (1 + log2 exponent) 8)
          in stackOp2 cost (uncurry exponentiate)

        -- op: SIGNEXTEND
        0x0b ->
          stackOp2 (const g_low) $ \(bytes, x) ->
            if bytes >= 32 then x
            else let n = num bytes * 8 + 7 in
              if testBit x n
              then x .|. complement (bit n - 1)
              else x .&. (bit n - 1)

        -- op: CREATE
        0xf0 ->
          notStatic $
          case stk of
            (xValue:xOffset:xSize:xs) ->
              burn g_create $ let
              newAddr = newContractAddress self (wordValue (view nonce this)) in
              create vm self this fees xValue xOffset xSize xs newAddr
            _ -> underrun

        -- op: CALL
        0xf1 ->
          case stk of
            ( xGas
              : (num -> xTo)
              : xValue
              : xInOffset
              : xInSize
              : xOutOffset
              : xOutSize
              : xs
             ) ->
              case xTo of
                n | n > 0 && n <= 8 -> precompiledContract
                n | num n == cheatCode ->
                  do
                    assign (state . stack) xs
                    cheat (xInOffset, xInSize) (xOutOffset, xOutSize)
                _ ->
                  let
                    availableGas = the state gas
                    recipient    = view (env . contracts . at xTo) vm
                    (cost, gas') = costOfCall fees recipient xValue availableGas xGas
                  in burn (cost - gas') $
                    (if xValue > 0 then notStatic else id) $
                    if xValue > view balance this
                    then do
                      assign (state . stack) (0 : xs)
                      next
                    else
                      case view execMode vm of
                        ExecuteAsVMTest -> do
                          assign (state . stack) (1 : xs)
                          next
                        ExecuteNormally -> do
                          delegateCall fees gas' xTo xInOffset xInSize xOutOffset xOutSize xs $ do
                            zoom state $ do
                              assign callvalue xValue
                              assign caller (the state contract)
                              assign contract xTo
                              assign memorySize 0
                            zoom (env . contracts) $ do
                              ix self . balance -= xValue
                              ix xTo  . balance += xValue
            _ ->
              underrun

        -- op: CALLCODE
        0xf2 ->
          error "CALLCODE not supported (use DELEGATECALL)"

        -- op: RETURN
        0xf3 ->
          case stk of
            (xOffset:xSize:_) ->
              accessMemoryRange fees xOffset xSize $ do
                let output = readMemory (num xOffset) (num xSize) vm
                finishFrame (FrameReturned output)
            _ -> underrun

        -- op: DELEGATECALL
        0xf4 ->
          case stk of
            (xGas:xTo:xInOffset:xInSize:xOutOffset:xOutSize:xs) ->
              if num xTo == cheatCode
              then do
                assign (state . stack) xs
                cheat (xInOffset, xInSize) (xOutOffset, xOutSize)
              else
                burn (num g_call) $ do
                  delegateCall fees xGas (num xTo) xInOffset xInSize xOutOffset xOutSize xs
                    (return ())
            _ -> underrun

        -- op: CREATE2
        0xf5 -> notStatic $
          case stk of
            (xValue:xOffset:xSize:xSalt:xs) ->
              burn (g_create + g_sha3word * ceilDiv (num xSize) 32) $ let
                initcode = readMemory (num xOffset) (num xSize) vm
                newAddr  = newContractAddressCREATE2 self (num xSalt) initcode in
                  create vm self this fees xValue xOffset xSize xs newAddr
            _ -> underrun

        -- op: STATICCALL
        0xfa ->
          case stk of
            (xGas : (num -> xTo) : xInOffset : xInSize : xOutOffset : xOutSize : xs) ->
              case xTo of
                n | n > 0 && n <= 8 -> precompiledContract
                _ ->
                  let
                    availableGas = the state gas
                    recipient    = view (env . contracts . at xTo) vm
                    (cost, gas') = costOfCall fees recipient 0 availableGas xGas
                  in burn (cost - gas') $
                    case view execMode vm of
                      ExecuteAsVMTest -> do
                        assign (state . stack) (1 : xs)
                        next
                      ExecuteNormally -> do
                        delegateCall fees gas' xTo xInOffset xInSize xOutOffset xOutSize xs $ do
                          zoom state $ do
                            assign callvalue 0
                            assign caller (the state contract)
                            assign contract xTo
                            assign memorySize 0
                            assign static True
            _ ->
              underrun

        -- op: SELFDESTRUCT
        0xff ->
          notStatic $
          case stk of
            [] -> underrun
            (x:_) -> do
              touchAccount (num x) $ \_ -> do
                pushTo (tx . selfdestructs) self
                assign (env . contracts . ix self . balance) 0
                modifying
                  (env . contracts . ix (num x) . balance)
                  (+ (vm ^?! env . contracts . ix self . balance))
                doStop

        -- op: REVERT
        0xfd ->
          case stk of
            (xOffset:xSize:_) ->
              accessMemoryRange fees xOffset xSize $ do
                let output = readMemory (num xOffset) (num xSize) vm
                finishFrame (FrameReverted output)
            _ -> underrun

        xxx ->
          vmError (UnrecognizedOpcode xxx)

precompiledContract :: (?op :: Word8) => EVM ()
precompiledContract = do
  vm <- get
  fees <- use (block . schedule)
  stk <- use (state . stack)

  case (?op, stk) of
    -- CALL (includes value)
    (0xf1, (_:(num -> op):_:inOffset:inSize:outOffset:outSize:xs)) ->
      doIt vm fees op inOffset inSize outOffset outSize xs
    -- STATICCALL (does not include value)
    (0xfa, (_:(num -> op):inOffset:inSize:outOffset:outSize:xs)) ->
      doIt vm fees op inOffset inSize outOffset outSize xs
    _ ->
      underrun

  where
    doIt vm fees op inOffset inSize outOffset outSize xs =
      let
        input = readMemory (num inOffset) (num inSize) vm
      in
        case EVM.Precompiled.execute op input (num outSize) of
          Nothing -> do
            assign (state . stack) (0 : xs)
            vmError (PrecompiledContractError op)
          Just output -> do
            let
              cost =
                case op of
                  1 -> 3000
                  _ -> error ("unimplemented precompiled contract " ++ show op)
            accessMemoryRange fees inOffset inSize $
              accessMemoryRange fees outOffset outSize $
                burn cost $ do
                  assign (state . stack) (1 : xs)
                  modifying (state . memory)
                    (writeMemory output outSize 0 outOffset)
                  next

-- * Opcode helper actions

noop :: Monad m => m ()
noop = pure ()

pushTo :: MonadState s m => ASetter s s [a] [a] -> a -> m ()
pushTo f x = f %= (x :)

pushToSequence :: MonadState s m => ASetter s s (Seq a) (Seq a) -> a -> m ()
pushToSequence f x = f %= (Seq.|> x)

touchAccount :: Addr -> (Contract -> EVM ()) -> EVM ()
touchAccount addr continue = do
  use (env . contracts . at addr) >>= \case
    Just c -> continue c
    Nothing ->
      use (cache . fetched . at addr) >>= \case
        Just c -> do
          assign (env . contracts . at addr) (Just c)
          continue c
        Nothing ->
          assign result . Just . VMFailure . Query $
            PleaseFetchContract addr
              (\c -> do assign (cache . fetched . at addr) (Just c)
                        assign (env . contracts . at addr) (Just c)
                        assign result Nothing
                        continue c)

accessStorage
  :: Addr             -- ^ Contract address
  -> Word             -- ^ Storage slot key
  -> (Word -> EVM ()) -- ^ Continuation
  -> EVM ()
accessStorage addr slot continue =
  use (env . contracts . at addr) >>= \case
    Just c ->
      case view (storage . at slot) c of
        Just value ->
          continue value
        Nothing ->
          if view external c
          then
            assign result . Just . VMFailure . Query $
              PleaseFetchSlot addr slot
                (\x -> do
                    assign (cache . fetched . ix addr . storage . at slot) (Just x)
                    assign (env . contracts . ix addr . storage . at slot) (Just x)
                    assign result Nothing
                    continue x)
          else do
            assign (env . contracts . ix addr . storage . at slot) (Just 0)
            continue 0
    Nothing ->
      touchAccount addr $ \_ ->
        accessStorage addr slot continue

create :: (?op :: Word8)
  => VM -> Addr -> Contract
  -> FeeSchedule Word
  -> Word -> Word -> Word -> [Word] -> Addr -> EVM ()
create vm self this fees xValue xOffset xSize xs newAddr =
  accessMemoryRange fees xOffset xSize $ do
  if xValue > view balance this
    then do
      assign (state . stack) (0 : xs)
      next
    else do
      case view execMode vm of
        ExecuteAsVMTest -> do
          assign (state . stack) (num newAddr : xs)
          next

        ExecuteNormally -> do
          let
            initCode =
              readMemory (num xOffset) (num xSize) vm
            newContract =
              initialContract (InitCode initCode)
            newContext  =
              CreationContext (view codehash newContract)

          zoom (env . contracts) $ do
            assign (at newAddr) (Just newContract)
            assign (ix newAddr . balance) xValue
            modifying (ix self . balance) (flip (-) xValue)
            modifying (ix self . nonce) succ

          pushTrace (FrameTrace newContext)
          next
          vm' <- get
          pushTo frames $ Frame
            { _frameContext = newContext
            , _frameState   = (set stack xs) (view state vm')
            }

          assign state $
            blankState
              & set contract   newAddr
              & set codeContract newAddr
              & set code       initCode
              & set callvalue  xValue
              & set caller     self
              & set gas        (view (state . gas) vm')


-- | Replace a contract's code, like when CREATE returns
-- from the constructor code.
replaceCode :: Addr -> ContractCode -> EVM ()
replaceCode target newCode = do
  zoom (env . contracts . at target) $ do
    Just now <- get
    case (view bytecode now) of
      InitCode _ ->
        put . Just $
        initialContract newCode
        & set storage (view storage now)
        & set balance (view balance now)
        & set nonce   (view nonce now)
      RuntimeCode _ ->
        error "internal error: can't replace code of deployed contract"

replaceCodeOfSelf :: ContractCode -> EVM ()
replaceCodeOfSelf newCode = do
  vm <- get
  replaceCode (view (state . contract) vm) newCode

resetState :: EVM ()
resetState = do
  assign result     Nothing
  assign frames     []
  assign state      blankState

finalize :: EVM ()
finalize = do
  destroyedAddresses <- use (tx . selfdestructs)
  modifying (env . contracts)
    (Map.filterWithKey (\k _ -> not (elem k destroyedAddresses)))

loadContract :: Addr -> EVM ()
loadContract target =
  preuse (env . contracts . ix target . bytecodeE) >>=
    \case
      Nothing ->
        error "Call target doesn't exist"
      Just targetCode -> do
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

burn :: Word -> EVM () -> EVM ()
burn n continue = do
  available <- use (state . gas)
  if n <= available
    then do
      state . gas -= n
      burned += n
      continue
    else
      vmError (OutOfGas available n)

refund :: Word -> EVM ()
refund n = do
  self <- use (state . contract)
  pushTo (tx . refunds) (self, n)

-- * Cheat codes

-- The cheat code is 7109709ecfa91a80626ff3989d68f67f5b1dd12d.
-- Call this address using one of the cheatActions below to do
-- special things, e.g. changing the block timestamp. Beware that
-- these are necessarily hevm specific.
cheatCode :: Addr
cheatCode = num (keccak "hevm cheat code")

cheat
  :: (?op :: Word8)
  => (Word, Word) -> (Word, Word)
  -> EVM ()
cheat (inOffset, inSize) (outOffset, outSize) = do
  mem <- use (state . memory)
  let
    abi =
      num (wordValue (readMemoryWord32 inOffset mem))
    input =
      sliceMemory (inOffset + 4) (inSize - 4) mem
  case Map.lookup abi cheatActions of
    Nothing ->
      vmError (BadCheatCode abi)
    Just (argTypes, action) -> do
      case runGetOrFail
             (getAbiSeq (length argTypes) argTypes)
             (fromStrict input) of
        Right ("", _, args) -> do
          action (toList args) >>= \case
            Nothing -> do
              next
              push 1
            Just (encodeAbiValue -> bs) -> do
              next
              modifying (state . memory)
                (writeMemory bs outSize 0 outOffset)
              push 1
        Left _ ->
          vmError (BadCheatCode abi)
        Right _ ->
          vmError (BadCheatCode abi)

type CheatAction = ([AbiType], [AbiValue] -> EVM (Maybe AbiValue))

cheatActions :: Map Word32 CheatAction
cheatActions =
  Map.fromList
    [ action "warp(uint256)" [AbiUIntType 256] $
        \[AbiUInt 256 x] -> do
          assign (block . timestamp) (w256 (W256 x))
          return Nothing
    ]
  where
    action s ts f = (abiKeccak s, (ts, f))


-- * General call implementation ("delegateCall")

delegateCall
  :: (?op :: Word8)
  => FeeSchedule Word
  -> Word -> Addr -> Word -> Word -> Word -> Word -> [Word]
  -> EVM ()
  -> EVM ()
delegateCall fees xGas xTo xInOffset xInSize xOutOffset xOutSize xs continue =
  touchAccount xTo . const $
    preuse (env . contracts . ix xTo) >>=
      \case
        Nothing -> vmError (NoSuchContract xTo)
        Just target ->
          accessMemoryRange fees xInOffset xInSize $ do
            accessMemoryRange fees xOutOffset xOutSize $ do
              burn xGas $ do
                vm0 <- get

                let newContext = CallContext
                      { callContextOffset = xOutOffset
                      , callContextSize = xOutSize
                      , callContextCodehash = view codehash target
                      , callContextReversion = view (env . contracts) vm0
                      , callContextAbi =
                          if xInSize >= 4
                          then
                            let
                              w = wordValue
                                    (readMemoryWord32 xInOffset (view (state . memory) vm0))
                            in Just $! num w
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
                  assign gas xGas
                  assign pc 0
                  assign code (view bytecodeE target)
                  assign codeContract xTo
                  assign stack mempty
                  assign memory mempty
                  assign calldata (readMemory (num xInOffset) (num xInSize) vm0)

                continue


-- * VM error implementation

underrun :: EVM ()
underrun = vmError StackUnderrun

-- | A stack frame can be popped in three ways.
data FrameResult
  = FrameReturned ByteString -- ^ STOP, RETURN, or no more code
  | FrameReverted ByteString -- ^ REVERT
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
    [] ->
      assign result . Just $
        case how of
          FrameReturned output -> VMSuccess output
          FrameReverted output -> VMFailure (Revert output)
          FrameErrored e       -> VMFailure e

    -- Are there some remaining frames?
    nextFrame : remainingFrames -> do

      -- Pop the top frame.
      assign frames remainingFrames
      -- Install the state of the frame to which we shall return.
      assign state (view frameState nextFrame)
      -- Insert a debug trace.
      insertTrace $
        case how of
          FrameErrored e ->
            ErrorTrace e
          FrameReverted output ->
            ErrorTrace (Revert output)
          FrameReturned output ->
            ReturnTrace output (view frameContext nextFrame)
      -- Pop to the previous level of the debug trace stack.
      popTrace

      let remainingGas = view (state . gas) oldVm

      -- Now dispatch on whether we were creating or calling,
      -- and whether we shall return, revert, or error (six cases).
      case view frameContext nextFrame of

        -- Were we calling?
        CallContext (num -> outOffset) (num -> outSize) _ _ _ reversion -> do

          let
            -- When entering a call, the gas allowance is counted as burned
            -- in advance; this unburns the remainder and adds it to the
            -- parent frame.
            reclaimRemainingGasAllowance = do
              modifying burned (subtract remainingGas)
              modifying (state . gas) (+ remainingGas)

            revertContracts = assign (env . contracts) reversion

          case how of
            -- Case 1: Returning from a call?
            FrameReturned output -> do
              assign (state . returndata) output
              copyBytesToMemory output outSize 0 outOffset
              reclaimRemainingGasAllowance
              push 1

            -- Case 2: Reverting during a call?
            FrameReverted output -> do
              revertContracts
              assign (state . returndata) output
              reclaimRemainingGasAllowance
              push 0

            -- Case 3: Error during a call?
            FrameErrored _ -> do
              revertContracts
              push 0

        -- Or were we creating?
        CreationContext _ -> do
          let
            createe = view (state . contract) oldVm
            destroy = assign (env . contracts . at createe) Nothing

          case how of
            -- Case 4: Returning during a creation?
            FrameReturned output -> do
              replaceCode createe (RuntimeCode output)
              assign (state . gas) remainingGas
              push (num createe)

            -- Case 5: Reverting during a creation?
            FrameReverted output -> do
              destroy
              assign (state . returndata) output
              assign (state . gas) remainingGas
              push 0

            -- Case 6: Error during a creation?
            FrameErrored _ -> do
              destroy
              assign (state . gas) 0
              push 0


vmError :: Error -> EVM ()
vmError e = finishFrame (FrameErrored e)

-- * Memory helpers

accessMemoryRange
  :: FeeSchedule Word
  -> Word
  -> Word
  -> EVM ()
  -> EVM ()
accessMemoryRange _ _ 0 continue = continue
accessMemoryRange fees f l continue = do
  m0 <- num <$> use (state . memorySize)
  if f + l < l
    then vmError IllegalOverflow
    else do
      let m1 = 32 * ceilDiv (max m0 (f + l)) 32
      burn (memoryCost fees m1 - memoryCost fees m0) $ do
        assign (state . memorySize) (num m1)
        continue

accessMemoryWord
  :: FeeSchedule Word -> Word -> EVM () -> EVM ()
accessMemoryWord fees x continue = accessMemoryRange fees x 32 continue

copyBytesToMemory
  :: ByteString -> Word -> Word -> Word -> EVM ()
copyBytesToMemory bs size xOffset yOffset =
  if size == 0 then noop
  else do
    mem <- use (state . memory)
    assign (state . memory) $
      writeMemory bs size xOffset yOffset mem

readMemory :: Word -> Word -> VM -> ByteString
readMemory offset size vm = sliceMemory offset size (view (state . memory) vm)

word256At
  :: Functor f
  => Word -> (Word -> f Word)
  -> ByteString -> f ByteString
word256At i = lens getter setter where
  getter m = readMemoryWord i m
  setter m x = setMemoryWord i x m

-- * Tracing

withTraceLocation
  :: (MonadState VM m) => TraceData -> m Trace
withTraceLocation x = do
  vm <- get
  let
    Just this =
      preview (env . contracts . ix (view (state . codeContract) vm)) vm
  pure Trace
    { _traceData = x
    , _traceCodehash = view codehash this
    , _traceOpIx = (view opIxMap this) Vector.! (view (state . pc) vm)
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
traceForest vm =
  view (traces . to zipperRootForest) vm

traceLog :: (MonadState VM m) => Log -> m ()
traceLog log = do
  trace <- withTraceLocation (EventTrace log)
  modifying traces $
    \t -> Zipper.nextSpace (Zipper.insert (Node trace []) t)

-- * Stack manipulation

push :: Word -> EVM ()
push x = state . stack %= (x :)

stackOp1
  :: (?op :: Word8)
  => (Word -> Word)
  -> (Word -> Word)
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
  => ((Word, Word) -> Word)
  -> ((Word, Word) -> Word)
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
  => ((Word, Word, Word) -> Word)
  -> ((Word, Word, Word) -> Word)
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

checkJump :: (Integral n) => n -> [Word] -> EVM ()
checkJump x xs = do
  theCode <- use (state . code)
  if x < num (BS.length theCode) && BS.index theCode (num x) == 0x5b
    then
      insidePushData (num x) >>=
        \case
          True ->
            vmError BadJumpDestination
          _ -> do
            state . stack .= xs
            state . pc .= num x
    else vmError BadJumpDestination

insidePushData :: Int -> EVM Bool
insidePushData i =
  -- If the operation index for the code pointer is the same
  -- as for the previous code pointer, then it's inside push data.
  if i == 0
    then pure False
    else do
      self <- use (state . codeContract)
      Just x <- preuse (env . contracts . ix self . opIxMap)
      pure ((x Vector.! i) == (x Vector.! (i - 1)))

opSize :: Word8 -> Int
opSize x | x >= 0x60 && x <= 0x7f = num x - 0x60 + 2
opSize _                          = 1

-- Index i of the resulting vector contains the operation index for
-- the program counter value i.  This is needed because source map
-- entries are per operation, not per byte.
mkOpIxMap :: ByteString -> Vector Int
mkOpIxMap xs = Vector.create $ Vector.new (BS.length xs) >>= \v ->
  -- Loop over the byte string accumulating a vector-mutating action.
  -- This is somewhat obfuscated, but should be fast.
  let (_, _, _, m) =
        BS.foldl' (go v) (0 :: Word8, 0, 0, return ()) xs
  in m >> return v
  where
    go v (0, !i, !j, !m) x | x >= 0x60 && x <= 0x7f =
      {- Start of PUSH op. -} (x - 0x60 + 1, i + 1, j,     m >> Vector.write v i j)
    go v (1, !i, !j, !m) _ =
      {- End of PUSH op. -}   (0,            i + 1, j + 1, m >> Vector.write v i j)
    go v (0, !i, !j, !m) _ =
      {- Other op. -}         (0,            i + 1, j + 1, m >> Vector.write v i j)
    go v (n, !i, !j, !m) _ =
      {- PUSH data. -}        (n - 1,        i + 1, j,     m >> Vector.write v i j)

vmOp :: VM -> Maybe Op
vmOp vm =
  let i  = vm ^. state . pc
      xs = BS.drop i (vm ^. state . code)
      op = BS.index xs 0
  in if BS.null xs
     then Nothing
     else Just (readOp op (BS.drop 1 xs))

vmOpIx :: VM -> Maybe Int
vmOpIx vm =
  do self <- currentContract vm
     (view opIxMap self) Vector.!? (view (state . pc) vm)

opParams :: VM -> Map String Word
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

readOp :: Word8 -> ByteString -> Op
readOp x _  | x >= 0x80 && x <= 0x8f = OpDup (x - 0x80 + 1)
readOp x _  | x >= 0x90 && x <= 0x9f = OpSwap (x - 0x90 + 1)
readOp x _  | x >= 0xa0 && x <= 0xa4 = OpLog (x - 0xa0)
readOp x xs | x >= 0x60 && x <= 0x7f =
  let n   = x - 0x60 + 1
      xs' = BS.take (num n) xs
  in OpPush (word xs')
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
  _    -> (OpUnknown x)

mkCodeOps :: ByteString -> RegularVector.Vector (Int, Op)
mkCodeOps bytes = RegularVector.fromList . toList $ go 0 bytes
  where
    go !i !xs =
      case BS.uncons xs of
        Nothing ->
          mempty
        Just (x, xs') ->
          let j = opSize x
          in (i, readOp x xs') Seq.<| go (i + j) (BS.drop j xs)

-- * Gas cost calculation helpers

-- Gas cost function for CALL, transliterated from the Yellow Paper.
costOfCall
  :: FeeSchedule Word
  -> Maybe a -> Word -> Word -> Word
  -> (Word, Word)
costOfCall (FeeSchedule {..}) recipient xValue availableGas xGas =
  (c_gascap + c_extra, c_callgas)
  where
    c_extra =
      num g_call + c_xfer + c_new
    c_xfer =
      if xValue /= 0          then num g_callvalue              else 0
    c_new =
      if isNothing recipient  then num g_newaccount             else 0
    c_callgas =
      if xValue /= 0          then c_gascap + num g_callstipend else c_gascap
    c_gascap =
      if availableGas >= c_extra
      then min xGas (allButOne64th (availableGas - c_extra))
      else xGas

memoryCost :: FeeSchedule Word -> Word -> Word
memoryCost FeeSchedule{..} byteCount =
  let
    wordCount = ceilDiv byteCount 32
    linearCost = g_memory * wordCount
    quadraticCost = div (wordCount * wordCount) 512
  in
    if byteCount > exponentiate 2 32
    then maxBound
    else linearCost + quadraticCost

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
