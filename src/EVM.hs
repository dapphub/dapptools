{-# Language StrictData #-}
{-# Language TemplateHaskell #-}

module EVM where

import Debug.Trace

import Prelude hiding ((^))

import EVM.Types
import EVM.Solidity
import EVM.Keccak

import Control.Monad.State.Strict hiding (state)

import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import Data.Bits (Bits, bit, testBit, complement)
import Data.DoubleWord (loWord, signedWord, unsignedWord, fromHiAndLo)
import Data.Word (Word8, Word32)

import Control.Lens hiding (op, (:<), (|>))

import Data.ByteString              (ByteString)
import Data.IntMap.Strict           (IntMap)
import Data.Map.Strict              (Map)
import Data.Maybe                   (fromMaybe, fromJust)
import Data.Monoid                  (Endo)
import Data.Sequence                (Seq)
import Data.Vector.Storable         (Vector)
import Data.Vector.Storable.Mutable (new, write)
import Data.Foldable                (toList)

import Foreign.ForeignPtr (withForeignPtr)
import Foreign            (castPtr)
import System.IO.Unsafe   (unsafePerformIO)

import qualified Data.ByteString      as BS
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Data.Vector.Storable as Vector

import qualified Data.Vector as RegularVector
import qualified Data.Vector.Mutable as RegularVector (new, write)

data Op
  = OpStop
  | OpAdd
  | OpMul
  | OpSub
  | OpDiv
  | OpSdiv
  | OpMod
  | OpSmod
  | OpAddmod
  | OpMulmod
  | OpExp
  | OpSignextend
  | OpLt
  | OpGt
  | OpSlt
  | OpSgt
  | OpEq
  | OpIszero
  | OpAnd
  | OpOr
  | OpXor
  | OpNot
  | OpByte
  | OpSha3
  | OpAddress
  | OpBalance
  | OpOrigin
  | OpCaller
  | OpCallvalue
  | OpCalldataload
  | OpCalldatasize
  | OpCalldatacopy
  | OpCodesize
  | OpCodecopy
  | OpGasprice
  | OpExtcodesize
  | OpExtcodecopy
  | OpBlockhash
  | OpCoinbase
  | OpTimestamp
  | OpNumber
  | OpDifficulty
  | OpGaslimit
  | OpPop
  | OpMload
  | OpMstore
  | OpMstore8
  | OpSload
  | OpSstore
  | OpJump
  | OpJumpi
  | OpPc
  | OpMsize
  | OpGas
  | OpJumpdest
  | OpCreate
  | OpCall
  | OpCallcode
  | OpReturn
  | OpDelegatecall
  | OpSelfdestruct
  | OpDup !Word8
  | OpSwap !Word8
  | OpLog !Word8
  | OpPush !W256
  | OpUnknown Word8
  deriving (Show, Eq)

-- | The possible result states of a VM
data VMResult
  = VMRunning              -- ^ More operations to run
  | VMFailure              -- ^ An operation failed
  | VMSuccess ByteString   -- ^ Reached STOP, RETURN, or end-of-code
  deriving (Eq, Show)

-- | The state of a stepwise EVM execution
data VM = VM
  { _result        :: VMResult
  , _state         :: FrameState
  , _frames        :: [Frame]
  , _env           :: Env
  , _block         :: Block
  , _selfdestructs :: [Addr]
  , _logs          :: Seq Log
  } deriving Show

-- | A log entry
data Log = Log Addr ByteString [W256]
  deriving Show

-- | An entry in the VM's "call/create stack"
data Frame = Frame
  { _frameContext   :: FrameContext
  , _frameState     :: FrameState
  } deriving Show

-- | Call/create info
data FrameContext
  = CreationContext
  | CallContext
    { callContextOffset   :: W256
    , callContextSize     :: W256
    , callContextCodehash :: W256
    , callContextAbi      :: Maybe Word32
    }
  deriving Show

-- | The "registers" of the VM along with memory and data stack
data FrameState = FrameState
  { _contract    :: Addr
  , _code        :: ByteString
  , _pc          :: Int
  , _stack       :: [W256]
  , _memory      :: IntMap Word8
  , _memorySize  :: Int
  , _calldata    :: ByteString
  , _callvalue   :: W256
  , _caller      :: Addr
  } deriving Show

-- | The state of a contract
data Contract = Contract
  { _bytecode :: ByteString
  , _storage  :: Map W256 W256
  , _balance  :: W256
  , _nonce    :: W256
  , _codehash :: W256
  , _codesize :: Int -- (redundant?)
  , _opIxMap  :: Vector Int
  , _codeOps  :: RegularVector.Vector Op
  } deriving (Eq, Show)

-- | Kind of a hodgepodge?
data Env = Env
  { _contracts   :: Map Addr Contract
  , _solc        :: Map W256 SolcContract
  , _sha3Crack   :: Map W256 ByteString
  , _origin      :: Addr
  } deriving (Show)

data Block = Block
  { _coinbase   :: Addr
  , _timestamp  :: W256
  , _number     :: W256
  , _difficulty :: W256
  , _gaslimit   :: W256
  } deriving Show

blankState :: FrameState
blankState = FrameState
  { _contract   = 0
  , _code       = mempty
  , _pc         = 0
  , _stack      = mempty
  , _memory     = mempty
  , _memorySize = 0
  , _calldata   = mempty
  , _callvalue  = 0
  , _caller     = 0
  }

makeLenses ''FrameState
makeLenses ''Frame
makeLenses ''Block
makeLenses ''Contract
makeLenses ''Env
makeLenses ''VM

type EVM a = State VM a

currentContract vm =
  view (env . contracts . at (view (state . contract) vm)) vm

initialContract :: ByteString -> Contract
initialContract theCode = Contract
  { _bytecode = theCode
  , _codesize = BS.length theCode
  , _codehash = if BS.null theCode then 0 else keccak theCode
  , _storage  = mempty
  , _balance  = 0
  , _nonce    = 0
  , _opIxMap  = mkOpIxMap theCode
  , _codeOps  = mkCodeOps theCode
  }

performCreation :: ByteString -> EVM ()
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

resetState :: EVM ()
resetState = do
  -- TODO: handle selfdestructs
  assign result     VMRunning
  assign frames     []
  assign state      blankState

loadContract :: Addr -> EVM ()
loadContract target =
  preuse (env . contracts . ix target . bytecode) >>=
    \case
      Nothing ->
        error "Call target doesn't exist"
      Just targetCode -> do
        assign (state . contract) target
        assign (state . code)     targetCode

exec1 :: EVM ()
exec1 = do
  vm <- get

  let
    -- Convenience function to access parts of the current VM state.
    -- Arcane type signature needed to avoid monomorphism restriction.
    the :: ((b -> VM -> Const a VM) -> ((a -> Const a a) -> b) -> a)
    the f g = view (f . g) vm

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
          assign result (VMSuccess "")

    else do
      let op = BS.index (the state code) (the state pc)
      state . pc += opSize op

      case op of

        -- op: PUSH
        x | x >= 0x60 && x <= 0x7f ->
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + the state pc)
                                       (the state code))
          in push (word (BS.unpack xs))

        -- op: DUP
        x | x >= 0x80 && x <= 0x8f ->
          let !i = x - 0x80 + 1 in
            maybe underrun push (preview (ix (num i - 1)) stk)

        -- op: SWAP
        x | x >= 0x90 && x <= 0x9f ->
          let !i = x - 0x90 + 1 in
          if length stk < num i + 1
          then underrun
          else do
            assign (state . stack . ix 0) (stk ^?! ix (num i))
            assign (state . stack . ix (num i)) (stk ^?! ix 0)

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
                assign (state . stack) xs'
                pushToSequence logs (Log self bytes topics)
            _ ->
              underrun

        -- op: STOP
        0x00 ->
          case vm ^. frames of
            [] ->
              assign result (VMSuccess "")
            (nextFrame : remainingFrames) -> do
              assign frames remainingFrames
              assign state (view frameState nextFrame)
              push 1

        -- op: ADD
        0x01 -> stackOp2 (uncurry (+))
        -- op: MUL
        0x02 -> stackOp2 (uncurry (*))
        -- op: SUB
        0x03 -> stackOp2 (uncurry (-))

        -- op: DIV
        0x04 -> stackOp2 $
          \case (_, 0) -> 0
                (x, y) -> div x y

        -- op: SDIV
        0x05 ->
          stackOp2 $ \case
              (_, 0) -> 0
              (W256 x, W256 y) ->
                let sx = signedWord x
                    sy = signedWord y
                    k  = if (sx < 0) /= (sy < 0)
                         then (-1)
                         else 1
                in W256 . unsignedWord $ k * div (abs sx) (abs sy)

        -- op: MOD
        0x06 -> stackOp2 $ \case
          (_, 0) -> 0
          (x, y) -> mod x y

        -- op: SMOD
        0x07 -> stackOp2 $ \case
           (_, 0) -> 0
           (W256 x, W256 y) ->
             let sx = signedWord x
                 sy = signedWord y
                 k  = if sx < 0 then (-1) else 1
             in W256 . unsignedWord $ k * mod (abs sx) (abs sy)

        -- op: ADDMOD
        0x08 -> stackOp3 $ \case
          (_, _, 0) -> 0
          (x, y, z) ->
            fromWord512
              ((toWord512 x + toWord512 y) `mod` (toWord512 z))

        -- op: MULMOD
        0x09 -> stackOp3 $ \case
          (_, _, 0) -> 0
          (x, y, z) ->
            fromWord512
              ((toWord512 x * toWord512 y) `mod` (toWord512 z))

        -- op: LT
        0x10 -> stackOp2 $ \(x, y) -> if x < y then 1 else 0
        -- op: GT
        0x11 -> stackOp2 $ \(x, y) -> if x > y then 1 else 0

        -- op: SLT
        0x12 -> stackOp2 $ \(W256 x, W256 y) ->
          if signedWord x < signedWord y then 1 else 0

        -- op: SGT
        0x13 -> stackOp2 $ \(W256 x, W256 y) ->
          if signedWord x > signedWord y then 1 else 0

        -- op: EQ
        0x14 -> stackOp2 $ \(x, y) -> if x == y then 1 else 0

        -- op: ISZERO
        0x15 -> stackOp1 $ \case 0 -> 1; _ -> 0

        -- op: AND
        0x16 -> stackOp2 $ uncurry (.&.)
        -- op: OR
        0x17 -> stackOp2 $ uncurry (.|.)
        -- op: XOR
        0x18 -> stackOp2 $ uncurry xor
        -- op: NOT
        0x19 -> stackOp1 complement

        -- op: BYTE
        0x1a -> stackOp2 $ \case
          (n, _) | n >= 32 ->
            0
          (n, x) ->
            0xff .&. shiftR x (8 * (31 - num n))

        -- op: SHA3
        0x20 ->
          case stk of
            (xOffset:xSize:xs) -> do
              let bytes = readMemory (num xOffset) (num xSize) vm
                  hash  = keccak bytes
              assign (state . stack) (hash : xs)
              assign (env . sha3Crack . at hash) (Just bytes)
              accessMemoryRange (num xOffset) (num xSize)
            _ -> underrun

        -- op: ADDRESS
        0x30 -> push (num (the state contract))

        -- op: BALANCE
        0x31 -> stackOp1 $ \x ->
          preview (env . contracts . ix (num x) . balance) vm ?: 0

        -- op: ORIGIN
        0x32 -> push (num (the env origin))

        -- op: CALLER
        0x33 -> push (num (the state caller))

        -- op: CALLVALUE
        0x34 -> push (the state callvalue)

        -- op: CALLDATALOAD
        0x35 -> stackOp1 $ \x -> wordAt (num x) (the state calldata)

        -- op: CALLDATASIZE
        0x36 -> push (num (BS.length (the state calldata)))

        -- op: CALLDATACOPY
        0x37 ->
          case stk of
            (xTo:xFrom:xSize:xs) -> do
              assign (state . stack) xs
              copyBytesToMemory (the state calldata)
                (num xSize) (num xFrom) (num xTo)
            _ -> underrun

        -- op: CODESIZE
        0x38 ->
          push (num (BS.length (the state code)))

        -- op: CODECOPY
        0x39 ->
          case stk of
            (memOffset:codeOffset:n:xs) -> do
              assign (state . stack) xs
              copyBytesToMemory (view bytecode this)
                (num n) (num codeOffset) (num memOffset)
            _ -> underrun

        -- op: GASPRICE
        0x3a ->
          push 0

        -- op: EXTCODESIZE
        0x3b -> stackOp1 $ \x ->
          num (preview (env . contracts . ix (num x) . codesize) vm ?: 0)

        -- op: EXTCODECOPY
        0x3c ->
          case stk of
            (extAccount:memOffset:codeOffset:codeSize:xs) -> do
              let theCode =
                    (vm ^? env . contracts . ix (num extAccount) . bytecode)
                      ?: mempty
              assign (state . stack) xs
              copyBytesToMemory theCode
                (num codeSize) (num codeOffset) (num memOffset)
            _ -> underrun

        -- op: BLOCKHASH
        0x40 ->
          -- fake zero block hashes everywhere
          stackOp1 (const 0)

        -- op: COINBASE
        0x41 -> push (num (the block coinbase))

        -- op: TIMESTAMP
        0x42 -> push (the block timestamp)

        -- op: NUMBER
        0x43 -> push (the block number)

        -- op: DIFFICULTY
        0x44 -> push (the block difficulty)

        -- op: GASLIMIT
        0x45 -> push (the block gaslimit)

        -- op: POP
        0x50 ->
          case stk of
            (_:xs) -> assign (state . stack) xs
            _      -> underrun

        -- op: MLOAD
        0x51 ->
          case stk of
            (x:xs) -> do
              assign (state . stack) (view (word256At (num x)) mem : xs)
              accessMemoryWord x
            _ -> underrun

        -- op: MSTORE
        0x52 ->
          case stk of
            (x:y:xs) -> do
              assign (state . memory . word256At (num x)) y
              assign (state . stack) xs
              accessMemoryWord x
            _ -> underrun

        -- op: MSTORE8
        0x53 ->
          case stk of
            (x:y:xs) -> do
              assign (state . memory . at (num x)) (Just (num (y .&. 0xff)))
              assign (state . stack) xs
              accessMemoryRange x 1
            _ -> underrun

        -- op: SLOAD
        0x54 -> stackOp1 $ \x -> preview (storage . ix x) this ?: 0

        -- op: SSTORE
        0x55 -> do
          case stk of
            (x:y:xs) -> do
              assign
                (env . contracts . ix (the state contract) . storage . at x)
                (if y == 0 then Nothing else Just y)
              assign (state . stack) xs
            _ -> underrun

        -- op: JUMP
        0x56 ->
          case stk of
            (x:xs) -> do
              assign (state . stack) xs
              checkJump x
            _ -> underrun

        -- op: JUMPI
        0x57 -> do
          case stk of
            (x:y:xs) -> do
              assign (state . stack) xs
              unless (y == 0) (checkJump x)
            _ -> underrun

        -- op: PC
        0x58 ->
          push (num (the state pc))

        -- op: MSIZE
        0x59 ->
          push (num (the state memorySize))

        -- op: GAS
        0x5a -> push (0xffffffffffffffffff :: W256)

        -- op: JUMPDEST
        0x5b -> return ()

        -- op: EXP
        0x0a ->
          stackOp2 (uncurry (^))

        -- op: SIGNEXTEND
        0x0b ->
          stackOp2 $ \(bytes, x) ->
            if bytes >= 32 then x
            else let n = num bytes * 8 + 7 in
              if testBit x n
              then x .|. complement (bit n - 1)
              else x .&. (bit n - 1)

        -- op: CREATE
        0xf0 -> do
          case stk of
            (xValue:xOffset:xSize:xs) -> do
              accessMemoryRange xOffset xSize

              let
                newAddr      = newContractAddress self (view nonce this)
                creationCode = readMemory (num xOffset) (num xSize) vm

              zoom (env . contracts) $ do
                assign (at newAddr) . Just $
                  initialContract creationCode
                modifying (ix self . nonce) succ

              vm' <- get
              pushTo frames $ Frame
                { _frameContext = CreationContext
                , _frameState   = (set stack xs) (view state vm')
                }

              assign state $
                blankState
                  & set contract   newAddr
                  & set code       creationCode
                  & set callvalue  xValue
                  & set caller     self

            _ -> underrun

        -- op: CALL
        0xf1 ->
          case stk of
            (_:xTo:xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs) -> do
              delegateCall (num xTo) xInOffset xInSize xOutOffset xOutSize xs
              zoom state $ do
                assign callvalue xValue
                assign caller (the state contract)
                assign contract (num xTo)
            _ ->
              underrun

        -- op: CALLCODE
        0xf2 ->
          error "CALLCODE not supported (use DELEGATECALL)"

        -- op: RETURN
        0xf3 ->
          case stk of
            (xOffset:xSize:_) -> do
              accessMemoryRange xOffset xSize
              case vm ^. frames of
                [] ->
                  assign result (VMSuccess (readMemory (num xOffset) (num xSize) vm))

                (nextFrame : remainingFrames) -> do
                  assign frames remainingFrames

                  case view frameContext nextFrame of
                    CreationContext -> do
                      performCreation (readMemory (num xOffset) (num xSize) vm)
                      assign state (view frameState nextFrame)
                      push (num (the state contract))

                    CallContext yOffset ySize _ _ -> do
                      assign state (view frameState nextFrame)
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
            (_:xTo:xInOffset:xInSize:xOutOffset:xOutSize:xs) ->
              delegateCall (num xTo) xInOffset xInSize xOutOffset xOutSize xs
            _ -> underrun

        -- op: SELFDESTRUCT
        0xff ->
          case stk of
            [] -> underrun
            (x:_) -> do
              pushTo selfdestructs self
              assign (env . contracts . ix self . balance) 0
              modifying
                (env . contracts . ix (num x) . balance)
                (+ (vm ^?! env . contracts . ix self . balance))
              returnOp 0 (0, 0)

        _ ->
          returnOp 0 (0, 0)

delegateCall :: Addr -> W256 -> W256 -> W256 -> W256 -> [W256] -> EVM ()
delegateCall xTo xInOffset xInSize xOutOffset xOutSize xs =
  preuse (env . contracts . ix xTo) >>=
    \case
      Nothing -> returnOp 0 (0, 0)
      Just target -> do
        vm <- get

        pushTo frames $ Frame
          { _frameState = (set stack xs) (view state vm)
          , _frameContext = CallContext
              { callContextOffset = xOutOffset
              , callContextSize   = xOutSize
              , callContextCodehash = view codehash target
              , callContextAbi =
                  if xInSize >= 4
                  then Just $! view (state . memory . word32At (num xInOffset)) vm
                  else Nothing
              }
          }

        zoom state $ do
          assign pc 0
          assign code (view bytecode target)
          assign stack mempty
          assign memory mempty
          assign calldata (readMemory (num xInOffset) (num xInSize) vm)

        accessMemoryRange xInOffset xInSize
        accessMemoryRange xOutOffset xOutSize

accessMemoryRange :: W256 -> W256 -> EVM ()
accessMemoryRange _ 0 = return ()
accessMemoryRange f l =
  state . memorySize %= \n -> max n (ceilDiv (num (f + l)) 32)
  where
    ceilDiv a b =
      let (q, r) = quotRem a b
      in q + if r /= 0 then 1 else 0

accessMemoryWord :: W256 -> EVM ()
accessMemoryWord x = accessMemoryRange x 32

copyBytesToMemory
  :: ByteString -> Int -> Int -> Int -> EVM ()
copyBytesToMemory bs size xOffset yOffset =
  if size == 0 then return ()
  else do
    mem <- use (state . memory)
    assign (state . memory) $
      snd $ BS.foldl' (\(!i, !a) x -> (i + 1, IntMap.insert i x a)) (yOffset, mem)
        (BS.take size (BS.drop xOffset bs))

readMemory :: Int -> Int -> VM -> ByteString
readMemory offset size vm = readIntMap offset size (view (state . memory) vm)

readIntMap :: Int -> Int -> IntMap Word8 -> ByteString
readIntMap offset size intmap =
  if size == 0 then ""
  else
    let
      (_, bigger) =
        IntMap.split (offset - 1) intmap
      (range, _) =
        IntMap.split (offset + size) bigger
      vec =
        Vector.create $ do
          v <- new size
          IntMap.foldlWithKey'
            (\m k x -> m >> write v (k - offset) x)
            (return ())
            range
          return v
    in
      unsafePerformIO $ do
        withForeignPtr (fst (Vector.unsafeToForeignPtr0 vec))
          (\ptr -> BS.packCStringLen (castPtr ptr, size))

push :: W256 -> EVM ()
push x = state . stack %= (x :)

pushTo :: MonadState s m => ASetter s s [a] [a] -> a -> m ()
pushTo f x = f %= (x :)

pushToSequence :: MonadState s m => ASetter s s (Seq a) (Seq a) -> a -> m ()
pushToSequence f x = f %= (Seq.|> x)

underrun :: EVM ()
underrun = returnOp 0 (0, 0)

returnOp :: W256 -> (W256, W256) -> EVM ()
returnOp returnCode (xOffset, xSize) = do
  vm <- get
  case view frames vm of
    [] -> do
      assign result $
        case returnCode of
          0 -> VMFailure
          _ -> VMSuccess (readMemory (num xOffset) (num xSize) vm)

    (nextFrame : remainingFrames) -> do
      accessMemoryRange xOffset xSize
      case view frameContext nextFrame of
        CreationContext -> do
            let self = vm ^. state . contract
            if xSize == 0
              then do
                push 0
                assign result VMFailure
                assign (env . contracts . at self) Nothing
              else do
                push (num self)
                performCreation (readMemory (num xOffset) (num xSize) vm)

        CallContext yOffset ySize _ _ -> do
          assign frames remainingFrames
          assign state (view frameState nextFrame)
          push returnCode
          copyBytesToMemory
            (readMemory (num xOffset) (num ySize) vm)
            (num ySize)
            0
            (num yOffset)
          accessMemoryRange yOffset xSize

toWord512 :: W256 -> Word512
toWord512 (W256 x) = fromHiAndLo 0 x

fromWord512 :: Word512 -> W256
fromWord512 x = W256 (loWord x)

stackOp1 :: (W256 -> W256) -> EVM ()
stackOp1 f =
  use (state . stack) >>= \case
    (x:xs) ->
      let !y = f x in
      state . stack .= y : xs
    _ ->
      underrun

stackOp2 :: ((W256, W256) -> W256) -> EVM ()
stackOp2 f =
  use (state . stack) >>= \case
    (x:y:xs) ->
      state . stack .= f (x, y) : xs
    _ ->
      underrun

stackOp3 :: ((W256, W256, W256) -> W256) -> EVM ()
stackOp3 f =
  use (state . stack) >>= \case
    (x:y:z:xs) ->
      state . stack .= f (x, y, z) : xs
    _ ->
      underrun

checkJump :: Integral n => n -> EVM ()
checkJump x = do
  theCode <- use (state . code)
  if num x < BS.length theCode && BS.index theCode (num x) == 0x5b
    then
      insidePushData (num x) >>=
        \case
          True ->
            returnOp 0 (0, 0)
          _ ->
            state . pc .= num x
    else returnOp 0 (0, 0)

insidePushData :: Int -> EVM Bool
insidePushData i = do
  -- If the operation index for the code pointer is the same
  -- as for the previous code pointer, then it's inside push data.
  self <- use (state . contract)
  x <- useJust (env . contracts . ix self . opIxMap)
  return (i == 0 || (x Vector.! i) == (x Vector.! (i - 1)))

data VMOpts = VMOpts
  { vmoptCode :: ByteString
  , vmoptCalldata :: ByteString
  , vmoptValue :: W256
  , vmoptAddress :: Addr
  , vmoptCaller :: Addr
  , vmoptOrigin :: Addr
  , vmoptNumber :: W256
  , vmoptTimestamp :: W256
  , vmoptCoinbase :: Addr
  , vmoptDifficulty :: W256
  , vmoptGaslimit :: W256
  } deriving Show

makeVm :: VMOpts -> VM
makeVm o = VM
  { _result = VMRunning
  , _frames = mempty
  , _selfdestructs = mempty
  , _logs = mempty
  , _block = Block
    { _coinbase = vmoptCoinbase o
    , _timestamp = vmoptTimestamp o
    , _number = vmoptNumber o
    , _difficulty = vmoptDifficulty o
    , _gaslimit = vmoptGaslimit o
    }
  , _state = FrameState
    { _pc = 0
    , _stack = mempty
    , _memory = mempty
    , _memorySize = 0
    , _code = vmoptCode o
    , _contract = vmoptAddress o
    , _calldata = vmoptCalldata o
    , _callvalue = vmoptValue o
    , _caller = vmoptCaller o
    }
  , _env = Env
    { _sha3Crack = mempty
    , _solc = mempty
    , _origin = vmoptOrigin o
    , _contracts = Map.fromList
      [(vmoptAddress o, initialContract (vmoptCode o))]
    }
  }

-- Copied from the standard library just to get specialization.
-- We also use bit operations instead of modulo and multiply.
-- (This operation was significantly slow.)
(^) :: W256 -> W256 -> W256
x0 ^ y0 | y0 < 0    = errorWithoutStackTrace "Negative exponent"
        | y0 == 0   = 1
        | otherwise = f x0 y0
    where
          f x y | not (testBit y 0) = f (x * x) (y `shiftR` 1)
                | y == 1      = x
                | otherwise   = g (x * x) ((y - 1) `shiftR` 1) x
          g x y z | not (testBit y 0) = g (x * x) (y `shiftR` 1) z
                  | y == 1      = x * z
                  | otherwise   = g (x * x) ((y - 1) `shiftR` 1) (x * z)

num :: (Integral a, Num b) => a -> b
num = fromIntegral

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

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

word :: Integral a => [a] -> W256
word xs = sum [ num x `shiftL` (8*n)
              | (n, x) <- zip [0..] (reverse xs) ]

word32 :: Integral a => [a] -> Word32
word32 xs = sum [ num x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

wordAt :: Int -> ByteString -> W256
wordAt i bs = word [(bs ^? ix j) ?: 0 | j <- [i..(i+31)]]

word256At :: Int -> Lens' (IntMap Word8) W256
word256At i = lens getter setter where
  getter m =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ IntMap.findWithDefault 0 (i + n) m)
                                (8 * (31 - n))) (n - 1)
    in {-# SCC word256At_getter #-}
      go (0 :: W256) (31 :: Int)
  setter m x =
    {-# SCC word256At_setter #-}
    -- Optimizing this would help significantly.
    IntMap.union (IntMap.fromAscList [(i + 31 - j, byteAt x j) | j <- reverse [0..31]]) m

word32At :: Int -> Lens' (IntMap Word8) Word32
word32At i = lens getter setter where
  getter m =
    word32 [(m ^? ix (i + j)) ?: 0 | j <- [0..3]]
  setter m x =
    IntMap.union (IntMap.fromAscList [(i + 3 - j, byteAt x j) | j <- reverse [0..3]]) m

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

vmOp :: VM -> Maybe Op
vmOp vm =
  let i  = vm ^. state . pc
      xs = BS.drop i (vm ^. state . code)
      op = BS.index xs 0
  in if BS.null xs
     then Nothing
     else Just (readOp op (BS.drop 1 xs))

opParams :: VM -> Map String W256
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
      len = BS.length xs'
  in if len == fromIntegral n
     then OpPush (word (BS.unpack xs'))
     else OpPush (word (BS.unpack xs' ++ replicate (len - num n) 0))
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

{-
  Unimplemented:
    callcode
    delegatecall
-}
