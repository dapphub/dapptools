{-# Language ConstraintKinds #-}
{-# Language FlexibleInstances #-}
{-# Language RecordWildCards #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language StrictData #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

module EVM where

import Prelude hiding ((^), log, Word)

import EVM.Types
import EVM.Solidity
import EVM.Keccak
import EVM.Machine
import EVM.Concrete
import EVM.Op
import EVM.FeeSchedule (FeeSchedule (..))

import qualified EVM.FeeSchedule as FeeSchedule (metropolis)

import Control.Monad.State.Strict hiding (state)

import Data.Bits (xor, shiftR, (.&.), (.|.))
import Data.Bits (bit, testBit, complement, popCount)
import Data.Word (Word8)

import Control.Lens hiding (op, (:<), (|>))

import Data.ByteString              (ByteString)
import Data.Map.Strict              (Map)
import Data.Maybe                   (fromMaybe, fromJust, isNothing)
import Data.Monoid                  (Endo)
import Data.Sequence                (Seq)
import Data.Vector.Storable         (Vector)
import Data.Vector.Storable.Mutable (new, write)
import Data.Foldable                (toList)

import Data.Tree

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Data.Vector.Storable as Vector
import qualified Data.Tree.Zipper     as Zipper

import qualified Data.Vector as RegularVector

data Error e
  = BalanceTooLow (Word e) (Word e)
  | UnrecognizedOpcode Word8
  | SelfDestruction
  | StackUnderrun
  | BadJumpDestination
  | Revert
  | NoSuchContract Addr
  | OutOfGas

deriving instance Show (Error Concrete)

-- | The possible result states of a VM
data VMResult e
  = VMFailure (Error e)  -- ^ An operation failed
  | VMSuccess (Blob e)   -- ^ Reached STOP, RETURN, or end-of-code

deriving instance Show (VMResult Concrete)

-- | The state of a stepwise EVM execution
data VM e = VM
  { _result        :: Maybe (VMResult e)
  , _state         :: FrameState e
  , _frames        :: [Frame e]
  , _env           :: Env e
  , _block         :: Block e
  , _tx            :: TxState e
  , _logs          :: Seq (Log e)
  , _contextTrace  :: Zipper.TreePos Zipper.Empty (Either (Log e) (FrameContext e))
  }

-- | A log entry
data Log e = Log Addr (Blob e) [Word e]

-- | An entry in the VM's "call/create stack"
data Frame e = Frame
  { _frameContext   :: FrameContext e
  , _frameState     :: FrameState e
  }

-- | Call/create info
data FrameContext e
  = CreationContext
    { creationContextCodehash :: W256 }
  | CallContext
    { callContextOffset   :: Word e
    , callContextSize     :: Word e
    , callContextCodehash :: W256
    , callContextAbi      :: Maybe (Word e)
    , callContextReversion :: Map Addr (Contract e)
    }

-- | The "registers" of the VM along with memory and data stack
data FrameState e = FrameState
  { _contract    :: Addr
  , _codeContract :: Addr
  , _code        :: ByteString
  , _pc          :: Int
  , _stack       :: [Word e]
  , _memory      :: Memory e
  , _memorySize  :: Int
  , _calldata    :: Blob e
  , _callvalue   :: Word e
  , _caller      :: Addr
  , _gas         :: Word e
  }

-- | The state that spans a whole transaction
data TxState e = TxState
  { _selfdestructs :: [Addr]
  , _refunds       :: [(Addr, Word e)]
  }

-- | The state of a contract
data Contract e = Contract
  { _bytecode :: ByteString
  , _storage  :: Map (Word e) (Word e)
  , _balance  :: Word e
  , _nonce    :: Word e
  , _codehash :: W256
  , _codesize :: Int -- (redundant?)
  , _opIxMap  :: Vector Int
  , _codeOps  :: RegularVector.Vector Op
  }

deriving instance Show (Contract Concrete)
deriving instance Eq (Contract Concrete)

-- | Kind of a hodgepodge?
data Env e = Env
  { _contracts          :: Map Addr (Contract e)
  , _sha3Crack          :: Map (Word e) (Blob e)
  , _origin             :: Addr
  }

data Block e = Block
  { _coinbase   :: Addr
  , _timestamp  :: Word e
  , _number     :: Word e
  , _difficulty :: Word e
  , _gaslimit   :: Word e
  }

blankState :: Machine e => FrameState e
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
  }

makeLenses ''FrameState
makeLenses ''Frame
makeLenses ''Block
makeLenses ''TxState
makeLenses ''Contract
makeLenses ''Env
makeLenses ''VM

type EVM e a = State (VM e) a

currentContract :: Machine e => VM e -> Maybe (Contract e)
currentContract vm =
  view (env . contracts . at (view (state . codeContract) vm)) vm

zipperRootForest :: Zipper.TreePos Zipper.Empty a -> Forest a
zipperRootForest z =
  case Zipper.parent z of
    Nothing -> Zipper.toForest z
    Just z' -> zipperRootForest (Zipper.nextSpace z')

contextTraceForest :: Machine e => VM e -> Forest (Either (Log e) (FrameContext e))
contextTraceForest vm =
  view (contextTrace . to zipperRootForest) vm

traceLog :: MonadState (VM e) m => Log e -> m ()
traceLog log =
  modifying contextTrace $
    \t -> Zipper.nextSpace (Zipper.insert (Node (Left log) []) t)

initialContract :: Machine e => ByteString -> Contract e
initialContract theCode = Contract
  { _bytecode = theCode
  , _codesize = BS.length theCode
  , _codehash =
    if BS.null theCode then 0 else
      keccak (stripConstructorArguments theCode)
  , _storage  = mempty
  , _balance  = 0
  , _nonce    = 0
  , _opIxMap  = mkOpIxMap theCode
  , _codeOps  = mkCodeOps theCode
  }

performCreation :: Machine e => ByteString -> EVM e ()
performCreation createdCode = do
  self <- use (state . contract)
  zoom (env . contracts . at self) $ do
    if BS.null createdCode
      then put Nothing
      else do
        Just now <- get
        put . Just $
          initialContract createdCode
            & set storage (view storage now)
            & set balance (view balance now)

resetState :: Machine e => EVM e ()
resetState = do
  -- TODO: handle selfdestructs
  assign result     Nothing
  assign frames     []
  assign state      blankState

loadContract :: Machine e => Addr -> EVM e ()
loadContract target =
  preuse (env . contracts . ix target . bytecode) >>=
    \case
      Nothing ->
        error "Call target doesn't exist"
      Just targetCode -> do
        assign (state . contract) target
        assign (state . code)     targetCode
        assign (state . codeContract) target

burn :: Machine e => Word e -> EVM e () -> EVM e ()
burn n continue = do
  available <- use (state . gas)
  if n <= available
    then do
      state . gas -= n
      continue
    else
      vmError OutOfGas

refund :: Machine e => Word e -> EVM e ()
refund n = do
  self <- use (state . contract)
  pushTo (tx . refunds) (self, n)

{-# SPECIALIZE exec1 :: EVM Concrete () #-}
exec1 :: forall e. Machine e => EVM e ()
exec1 = do
  vm <- get

  let
    -- Convenience function to access parts of the current VM state.
    -- Arcane type signature needed to avoid monomorphism restriction.
    the :: (b -> VM e -> Const a (VM e)) -> ((a -> Const a a) -> b) -> a
    the f g = view (f . g) vm

    -- Simple for now
    fees@(FeeSchedule {..}) = FeeSchedule.metropolis :: FeeSchedule (Word e)

    -- Convenient aliases
    mem  = the state memory
    stk  = the state stack
    self = the state contract
    this = fromJust (preview (ix (the state contract)) (the env contracts))

  if the state pc >= num (BS.length (the state code))
    then
      case view frames vm of
        (nextFrame : remainingFrames) -> do
          assign frames remainingFrames
          assign state (view frameState nextFrame)
          push 1
        [] ->
          assign result (Just (VMSuccess (blob "")))

    else do
      let op = BS.index (the state code) (the state pc)
      state . pc += opSize op

      case op of

        -- op: PUSH
        x | x >= 0x60 && x <= 0x7f -> do
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + the state pc)
                                       (the state code))
          burn g_verylow $ do
            push (w256 (word xs))

        -- op: DUP
        x | x >= 0x80 && x <= 0x8f -> do
          let !i = x - 0x80 + 1
          case preview (ix (num i - 1)) stk of
            Nothing -> underrun
            Just y -> do
              burn g_verylow $ do
                push y

        -- op: SWAP
        x | x >= 0x90 && x <= 0x9f -> do
          let i = num (x - 0x90 + 1)
          if length stk < i + 1
            then underrun
            else
              burn g_verylow $ do
                zoom (state . stack) $ do
                  assign (ix 0) (stk ^?! ix i)
                  assign (ix i) (stk ^?! ix 0)

        -- op: LOG
        x | x >= 0xa0 && x <= 0xa4 ->
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
                  assign (state . stack) xs'
                  pushToSequence logs log
                  traceLog log
            _ ->
              underrun

        -- op: STOP
        0x00 ->
          case vm ^. frames of
            [] ->
              assign result (Just (VMSuccess ""))
            (nextFrame : remainingFrames) -> do
              modifying contextTrace $ \t ->
                case Zipper.parent t of
                  Nothing -> error "internal error (context trace root)"
                  Just t' -> Zipper.nextSpace t'
              assign frames remainingFrames
              assign state (view frameState nextFrame)
              case view frameContext nextFrame of
                CreationContext _ -> do
                  -- Move back the gas to the parent context
                  assign (state . gas) (the state gas)

                CallContext _ _ _ _ _ -> do
                  -- Take back the remaining gas allowance
                  modifying (state . gas) (+ the state gas)
              push 1

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

        -- op: SHA3
        0x20 ->
          case stk of
            ((num -> xOffset) : (num -> xSize) : xs) -> do
              let bytes = readMemory xOffset xSize vm
                  hash  = keccakBlob bytes
              burn (g_sha3 + g_sha3word * ceilDiv (num xSize) 32) $
                accessMemoryRange fees xOffset xSize $ do
                  assign (state . stack) (hash : xs)
                  assign (env . sha3Crack . at hash) (Just bytes)
            _ -> underrun

        -- op: ADDRESS
        0x30 -> burn g_base (push (num (the state contract)))

        -- op: BALANCE
        0x31 ->
          case stk of
            (x:xs) -> do
              burn g_balance $ do
                assign (state . stack) xs
                touchAccount (num x) >>= push . view balance
            [] ->
              underrun

        -- op: ORIGIN
        0x32 -> burn g_base (push (num (the env origin)))

        -- op: CALLER
        0x33 -> burn g_base (push (num (the state caller)))

        -- op: CALLVALUE
        0x34 -> burn g_base (push (the state callvalue))

        -- op: CALLDATALOAD
        0x35 -> stackOp1 (const g_verylow) $
          \x -> readBlobWord x (the state calldata)

        -- op: CALLDATASIZE
        0x36 -> burn g_base (push (blobSize (the state calldata)))

        -- op: CALLDATACOPY
        0x37 ->
          case stk of
            ((num -> xTo) : (num -> xFrom) : (num -> xSize) :xs) -> do
              burn (g_verylow + g_copy * ceilDiv (num xSize) 32) $ do
                assign (state . stack) xs
                copyBytesToMemory (the state calldata) xSize xFrom xTo
            _ -> underrun

        -- op: CODESIZE
        0x38 ->
          burn g_base (push (num (BS.length (the state code))))

        -- op: CODECOPY
        0x39 ->
          case stk of
            ((num -> memOffset) : (num -> codeOffset) : (num -> n) : xs) -> do
              burn (g_verylow + g_copy * ceilDiv (num n) 32) $ do
                accessMemoryRange fees memOffset n $ do
                  assign (state . stack) xs
                  copyBytesToMemory (blob (view bytecode this))
                    n codeOffset memOffset
            _ -> underrun

        -- op: GASPRICE
        0x3a ->
          burn g_base (push 0)

        -- op: EXTCODESIZE
        0x3b ->
          case stk of
            (x:xs) -> do
              burn g_high $ do
                assign (state . stack) xs
                touchAccount (num x) >>= push . num . view codesize
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
                  c <- touchAccount (num extAccount)
                  assign (state . stack) xs
                  copyBytesToMemory (blob (view bytecode c))
                    codeSize codeOffset memOffset
            _ -> underrun

        -- op: BLOCKHASH
        0x40 ->
          -- fake zero block hashes everywhere
          stackOp1 (const g_blockhash) (const 0)

        -- op: COINBASE
        0x41 -> burn g_base (push (num (the block coinbase)))

        -- op: TIMESTAMP
        0x42 -> burn g_base (push (the block timestamp))

        -- op: NUMBER
        0x43 -> burn g_base (push (the block number))

        -- op: DIFFICULTY
        0x44 -> burn g_base (push (the block difficulty))

        -- op: GASLIMIT
        0x45 -> burn g_base (push (the block gaslimit))

        -- op: POP
        0x50 ->
          case stk of
            (_:xs) -> burn g_base (assign (state . stack) xs)
            _      -> underrun

        -- op: MLOAD
        0x51 ->
          case stk of
            (x:xs) -> do
              burn g_verylow $
                accessMemoryWord fees x $ do
                  assign (state . stack) (view (word256At (num x)) mem : xs)
            _ -> underrun

        -- op: MSTORE
        0x52 ->
          case stk of
            (x:y:xs) -> do
              burn g_verylow $
                accessMemoryWord fees x $ do
                  assign (state . memory . word256At (num x)) y
                  assign (state . stack) xs
            _ -> underrun

        -- op: MSTORE8
        0x53 ->
          case stk of
            (x:y:xs) -> do
              burn g_verylow $
                accessMemoryRange fees x 1 $ do
                  modifying (state . memory) (setMemoryByte x (wordToByte y))
                  assign (state . stack) xs
            _ -> underrun

        -- op: SLOAD
        0x54 -> stackOp1 (const g_sload) $ \x ->
          fromMaybe 0 (preview (storage . ix x) this)

        -- op: SSTORE
        0x55 -> do
          case stk of
            (x:next:xs) -> do
              let prev = view (storage . at x) this

              -- Gas cost is higher when changing from zero to nonzero.
              let cost = if prev == Nothing && next /= 0 then g_sset else g_sreset

              burn cost $ do
                assign (state . stack) xs
                assign (env . contracts . ix (the state contract) . storage . at x)
                  (if next == 0 then Nothing else Just next)

                -- Give gas refund if clearing the storage slot.
                if prev /= Nothing && next == 0 then refund r_sclear else noop

            _ -> underrun

        -- op: JUMP
        0x56 ->
          case stk of
            (x:xs) -> do
              burn g_mid $ do
                assign (state . stack) xs
                checkJump x
            _ -> underrun

        -- op: JUMPI
        0x57 -> do
          case stk of
            (x:y:xs) -> do
              burn g_high $ do
                assign (state . stack) xs
                unless (y == 0) (checkJump x)
            _ -> underrun

        -- op: PC
        0x58 ->
          burn g_base $
            push (num (the state pc))

        -- op: MSIZE
        0x59 ->
          burn g_base $
            push (num (the state memorySize))

        -- op: GAS
        0x5a ->
          burn g_base $
            push (the state gas - g_base)

        -- op: JUMPDEST
        0x5b -> burn g_jumpdest noop

        -- op: EXP
        0x0a ->
          let cost (x, _) =
                if x == 0
                then g_exp
                else g_exp + num (ceilDiv (popCount x) 8)
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
        0xf0 -> do
          case stk of
            (xValue:_:_:_) | xValue > view balance this -> do
              vmError (BalanceTooLow (view balance this) xValue)

            (xValue:xOffset:xSize:xs) ->
              burn g_create $ do
                accessMemoryRange fees xOffset xSize $ do
                  let
                    newAddr     = newContractAddress self (forceConcreteWord (view nonce this))
                    newCode     = forceConcreteBlob $ readMemory (num xOffset) (num xSize) vm
                    newContract = initialContract newCode
                    newContext  = CreationContext (view codehash newContract)

                  zoom (env . contracts) $ do
                    assign (at newAddr) (Just newContract)
                    modifying (ix self . nonce) succ
                    modifying (ix self . balance) (flip (-) xValue)

                  vm' <- get
                  pushTo frames $ Frame
                    { _frameContext = newContext
                    , _frameState   = (set stack xs) (view state vm')
                    }

                  modifying contextTrace $ \t ->
                    Zipper.children $ Zipper.insert (Node (Right newContext) []) t

                  assign state $
                    blankState
                      & set contract   newAddr
                      & set codeContract newAddr
                      & set code       newCode
                      & set callvalue  xValue
                      & set caller     self
                      & set gas        (view (state . gas) vm')

            _ -> underrun

        -- op: CALL
        0xf1 ->
          case stk of
            (_:_:xValue:_:_:_:_:_) | xValue > view balance this -> do
              vmError (BalanceTooLow (view balance this) xValue)
            ( xGas
              : (num -> xTo)
              : xValue
              : xInOffset
              : xInSize
              : xOutOffset
              : xOutSize
              : xs
             ) -> do
              let
                availableGas = the state gas
                recipient    = view (env . contracts . at xTo) vm
                (cost, gas') = costOfCall fees recipient xValue availableGas xGas
              burn (cost - gas') $
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
                case vm ^. frames of
                  [] ->
                    assign result (Just (VMSuccess (readMemory (num xOffset) (num xSize) vm)))

                  (nextFrame : remainingFrames) -> do
                    assign frames remainingFrames

                    modifying contextTrace $ \t ->
                      case Zipper.parent t of
                        Nothing -> error "internal error (context trace root)"
                        Just t' -> Zipper.nextSpace t'

                    case view frameContext nextFrame of
                      CreationContext _ -> do
                        performCreation (forceConcreteBlob (readMemory (num xOffset) (num xSize) vm))
                        assign state (view frameState nextFrame)

                        -- Move back the gas to the parent context
                        assign (state . gas) (the state gas)

                        push (num (the state contract))

                      CallContext yOffset ySize _ _ _ -> do
                        assign state (view frameState nextFrame)

                        -- Take back the remaining gas allowance
                        modifying (state . gas) (+ the state gas)

                        copyBytesToMemory
                          (readMemory (num xOffset) (num ySize) vm)
                          (num ySize)
                          0
                          (num yOffset)
                        push 1

            _ -> underrun

        -- op: DELEGATECALL
        0xf4 ->
          case stk of
            (xGas:xTo:xInOffset:xInSize:xOutOffset:xOutSize:xs) ->
              burn (num g_call + xGas) $ do
                delegateCall fees xGas (num xTo) xInOffset xInSize xOutOffset xOutSize xs
                  (return ())
            _ -> underrun

        -- op: SELFDESTRUCT
        0xff ->
          case stk of
            [] -> underrun
            (x:_) -> do
              pushTo (tx . selfdestructs) self
              assign (env . contracts . ix self . balance) 0
              modifying
                (env . contracts . ix (num x) . balance)
                (+ (vm ^?! env . contracts . ix self . balance))
              vmError SelfDestruction

        -- op: REVERT
        0xfd ->
          vmError Revert

        xxx ->
          vmError (UnrecognizedOpcode xxx)

delegateCall
  :: Machine e
  => FeeSchedule (Word e)
  -> Word e -> Addr -> Word e -> Word e -> Word e -> Word e -> [Word e]
  -> EVM e ()
  -> EVM e ()
delegateCall fees xGas xTo xInOffset xInSize xOutOffset xOutSize xs continue = do
  preuse (env . contracts . ix xTo) >>=
    \case
      Nothing -> vmError (NoSuchContract xTo)
      Just target ->
        accessMemoryRange fees xInOffset xInSize $ do
          accessMemoryRange fees xOutOffset xOutSize $ do
            burn xGas $ do
              vm <- get

              let newContext = CallContext
                    { callContextOffset = xOutOffset
                    , callContextSize   = xOutSize
                    , callContextCodehash = view codehash target
                    , callContextReversion = view (env . contracts) vm
                    , callContextAbi = Nothing
                        -- if xInSize >= 4
                        -- then Just $! view (state . memory . word32At (num xInOffset)) vm
                        -- else Nothing
                    }

              pushTo frames $ Frame
                { _frameState = (set stack xs) (view state vm)
                , _frameContext = newContext
                }

              modifying contextTrace $ \t ->
                Zipper.children (Zipper.insert (Node (Right newContext) []) t)

              zoom state $ do
                assign gas xGas
                assign pc 0
                assign code (view bytecode target)
                assign codeContract xTo
                assign stack mempty
                assign memory mempty
                assign calldata (readMemory (num xInOffset) (num xInSize) vm)

              continue

{-#
  SPECIALIZE accessMemoryRange
    :: FeeSchedule (Word Concrete)
    -> Word Concrete -> Word Concrete
    -> EVM Concrete () -> EVM Concrete ()
 #-}
accessMemoryRange
  :: Machine e
  => FeeSchedule (Word e)
  -> Word e
  -> Word e
  -> EVM e ()
  -> EVM e ()
accessMemoryRange _ _ 0 continue = continue
accessMemoryRange fees f l continue = do
  m0 <- use (state . memorySize)
  let m1 = max m0 (num (f + l))
  burn (memoryCost fees m1 - memoryCost fees m0) $ do
    assign (state . memorySize) m1
    continue

memoryCost :: Machine e => FeeSchedule (Word e) -> Int -> Word e
memoryCost FeeSchedule{..} (num -> a) = g_memory * a + div (a * a) 512

{-#
  SPECIALIZE accessMemoryWord
    :: FeeSchedule (Word Concrete) -> Word Concrete
    -> EVM Concrete () -> EVM Concrete ()
 #-}
accessMemoryWord
  :: Machine e => FeeSchedule (Word e) -> Word e -> EVM e () -> EVM e ()
accessMemoryWord fees x continue = accessMemoryRange fees x 32 continue

{-#
  SPECIALIZE copyBytesToMemory
    :: Blob Concrete -> Word Concrete -> Word Concrete -> Word Concrete
    -> EVM Concrete ()
 #-}
copyBytesToMemory
  :: Machine e => Blob e -> Word e -> Word e -> Word e -> EVM e ()
copyBytesToMemory bs size xOffset yOffset =
  if size == 0 then noop
  else do
    mem <- use (state . memory)
    assign (state . memory) $
      writeMemory bs size xOffset yOffset mem

{-#
  SPECIALIZE readMemory
    :: Word Concrete -> Word Concrete -> VM Concrete -> Blob Concrete
 #-}
readMemory :: Machine e => Word e -> Word e -> VM e -> Blob e
readMemory offset size vm = sliceMemory offset size (view (state . memory) vm)

{-#
  SPECIALIZE word256At
    :: Functor f => Word Concrete -> (Word Concrete -> f (Word Concrete))
    -> Memory Concrete -> f (Memory Concrete)
 #-}
word256At
  :: (Machine e, Functor f)
  => Word e -> (Word e -> f (Word e))
  -> Memory e -> f (Memory e)
word256At i = lens getter setter where
  getter m = readMemoryWord i m
  setter m x = setMemoryWord i x m

{-# SPECIALIZE push :: Word Concrete -> EVM Concrete () #-}
push :: Machine e => Word e -> EVM e ()
push x = state . stack %= (x :)

pushTo :: MonadState s m => ASetter s s [a] [a] -> a -> m ()
pushTo f x = f %= (x :)

pushToSequence :: MonadState s m => ASetter s s (Seq a) (Seq a) -> a -> m ()
pushToSequence f x = f %= (Seq.|> x)

underrun :: Machine e => EVM e ()
underrun = vmError StackUnderrun

vmError :: Machine e => Error e -> EVM e ()
vmError e = do
  vm <- get
  case view frames vm of
    [] -> assign result (Just (VMFailure e))

    (nextFrame : remainingFrames) -> do
      modifying contextTrace $ \t ->
        case Zipper.parent t of
          Nothing -> error "internal error (context trace root)"
          Just t' -> Zipper.nextSpace t'

      case view frameContext nextFrame of
        CreationContext _ -> do
          assign frames remainingFrames
          assign state (view frameState nextFrame)
          assign (state . gas) 0
          push 0
          let self = vm ^. state . contract
          assign (env . contracts . at self) Nothing

        CallContext _ _ _ _ reversion -> do
          assign frames remainingFrames
          assign state (view frameState nextFrame)
          assign (env . contracts) reversion
          push 0

{-#
  SPECIALIZE stackOp1
    :: (Word Concrete -> Word Concrete)
    -> (Word Concrete -> Word Concrete)
    -> EVM Concrete ()
 #-}
stackOp1
  :: Machine e
  => (Word e -> Word e)
  -> (Word e -> Word e)
  -> EVM e ()
stackOp1 cost f =
  use (state . stack) >>= \case
    (x:xs) ->
      burn (cost x) $ do
        let !y = f x
        state . stack .= y : xs
    _ ->
      underrun

{-#
  SPECIALIZE stackOp2
    :: ((Word Concrete, Word Concrete) -> Word Concrete)
    -> ((Word Concrete, Word Concrete) -> Word Concrete)
    -> EVM Concrete ()
 #-}
stackOp2
  :: Machine e
  => ((Word e, Word e) -> Word e)
  -> ((Word e, Word e) -> Word e)
  -> EVM e ()
stackOp2 cost f =
  use (state . stack) >>= \case
    (x:y:xs) ->
      burn (cost (x, y)) $ do
        state . stack .= f (x, y) : xs
    _ ->
      underrun

{-#
  SPECIALIZE stackOp3
    :: ((Word Concrete, Word Concrete, Word Concrete) -> Word Concrete)
    -> ((Word Concrete, Word Concrete, Word Concrete) -> Word Concrete)
    -> EVM Concrete ()
 #-}
stackOp3
  :: Machine e
  => ((Word e, Word e, Word e) -> Word e)
  -> ((Word e, Word e, Word e) -> Word e)
  -> EVM e ()
stackOp3 cost f =
  use (state . stack) >>= \case
    (x:y:z:xs) ->
      burn (cost (x, y, z)) $ do
        state . stack .= f (x, y, z) : xs
    _ ->
      underrun

{-#
  SPECIALIZE checkJump
    :: Integral n => n -> EVM Concrete ()
 #-}
checkJump :: (Machine e, Integral n) => n -> EVM e ()
checkJump x = do
  theCode <- use (state . code)
  if num x < BS.length theCode && BS.index theCode (num x) == 0x5b
    then
      insidePushData (num x) >>=
        \case
          True ->
            vmError BadJumpDestination
          _ ->
            state . pc .= num x
    else vmError BadJumpDestination

{-#
  SPECIALIZE insidePushData
    :: Int -> EVM Concrete Bool
 #-}
insidePushData :: Machine e => Int -> EVM e Bool
insidePushData i = do
  -- If the operation index for the code pointer is the same
  -- as for the previous code pointer, then it's inside push data.
  self <- use (state . codeContract)
  x <- useJust (env . contracts . ix self . opIxMap)
  return (i == 0 || (x Vector.! i) == (x Vector.! (i - 1)))

touchAccount :: Machine e => Addr -> EVM e (Contract e)
touchAccount a = do
  use (env . contracts . at a) >>=
    \case
      Nothing -> do
        let c = initialContract ""
        assign (env . contracts . at a) (Just c)
        return c
      Just c ->
        return c

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
  } deriving Show

makeVm :: VMOpts -> VM Concrete
makeVm o = VM
  { _result = Nothing
  , _frames = mempty
  , _tx = TxState
    { _selfdestructs = mempty
    , _refunds = mempty
    }
  , _logs = mempty
  , _contextTrace = Zipper.fromForest []
  , _block = Block
    { _coinbase = vmoptCoinbase o
    , _timestamp = w256 $ vmoptTimestamp o
    , _number = w256 $ vmoptNumber o
    , _difficulty = w256 $ vmoptDifficulty o
    , _gaslimit = w256 $ vmoptGaslimit o
    }
  , _state = FrameState
    { _pc = 0
    , _stack = mempty
    , _memory = mempty
    , _memorySize = 0
    , _code = vmoptCode o
    , _contract = vmoptAddress o
    , _codeContract = vmoptAddress o
    , _calldata = B $ vmoptCalldata o
    , _callvalue = w256 $ vmoptValue o
    , _caller = vmoptCaller o
    , _gas = w256 $ vmoptGas o
    }
  , _env = Env
    { _sha3Crack = mempty
    , _origin = vmoptOrigin o
    , _contracts = Map.fromList
      [(vmoptAddress o, initialContract (vmoptCode o))]
    }
  }

viewJust :: Getting (Endo a) s a -> s -> a
viewJust f x = x ^?! f

useJust :: MonadState s f => Getting (Endo a) s a -> f a
useJust f = viewJust f <$> get

opSize :: Word8 -> Int
opSize x | x >= 0x60 && x <= 0x7f = num x - 0x60 + 2
opSize _                          = 1

-- Index i of the resulting vector contains the operation index for
-- the program counter value i.  This is needed because source map
-- entries are per operation, not per byte.
mkOpIxMap :: ByteString -> Vector Int
mkOpIxMap xs = Vector.create $ new (BS.length xs) >>= \v ->
  -- Loop over the byte string accumulating a vector-mutating action.
  -- This is somewhat obfuscated, but should be fast.
  let (_, _, _, m) =
        BS.foldl' (go v) (0 :: Word8, 0, 0, return ()) xs
  in m >> return v
  where
    go v (0, !i, !j, !m) x | x >= 0x60 && x <= 0x7f =
      {- Start of PUSH op. -} (x - 0x60 + 1, i + 1, j,     m >> write v i j)
    go v (1, !i, !j, !m) _ =
      {- End of PUSH op. -}   (0,            i + 1, j + 1, m >> write v i j)
    go v (0, !i, !j, !m) _ =
      {- Other op. -}         (0,            i + 1, j + 1, m >> write v i j)
    go v (n, !i, !j, !m) _ =
      {- PUSH data. -}        (n - 1,        i + 1, j,     m >> write v i j)

{-#
  SPECIALIZE vmOp
    :: VM Concrete -> Maybe Op
 #-}
vmOp :: Machine e => VM e -> Maybe Op
vmOp vm =
  let i  = vm ^. state . pc
      xs = BS.drop i (vm ^. state . code)
      op = BS.index xs 0
  in if BS.null xs
     then Nothing
     else Just (readOp op (BS.drop 1 xs))

{-#
  SPECIALIZE vmOpIx
    :: VM Concrete -> Maybe Int
 #-}
vmOpIx :: Machine e => VM e -> Maybe Int
vmOpIx vm =
  do self <- currentContract vm
     (view opIxMap self) Vector.!? (view (state . pc) vm)

opParams :: Machine e => VM e -> Map String (Word e)
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
  0xfd -> OpRevert
  0xff -> OpSelfdestruct
  _    -> (OpUnknown x)

mkCodeOps :: ByteString -> RegularVector.Vector Op
mkCodeOps bytes = RegularVector.fromList . toList $ go 0 bytes
  where
    go !i !xs =
      case BS.uncons xs of
        Nothing ->
          mempty
        Just (x, xs') ->
          let j = opSize x
          in readOp x xs' Seq.<| go (i + j) (BS.drop j xs)

ceilDiv :: (Num a, Integral a) => a -> a -> a
ceilDiv m n = div (m + n - 1) n

noop :: Monad m => m ()
noop = pure ()

-- Gas cost function for CALL, transliterated from the Yellow Paper.
costOfCall
  :: Machine e
  => FeeSchedule (Word e)
  -> Maybe a -> Word e -> Word e -> Word e
  -> (Word e, Word e)
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

allButOne64th :: (Num a, Integral a) => a -> a
allButOne64th n = n - div n 64
