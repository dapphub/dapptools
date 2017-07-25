{-# Language StrictData #-}
{-# Language TemplateHaskell #-}
{-# Language TypeOperators #-}
{-# Language ScopedTypeVariables #-}
{-# Language ConstraintKinds #-}
{-# Language StandaloneDeriving #-}
{-# Language FlexibleInstances #-}

module EVM where

import Prelude hiding ((^), log, Word)

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
import Data.String                  (IsString)
import Data.Vector.Storable         (Vector)
import Data.Vector.Storable.Mutable (new, write)
import Data.Foldable                (toList)

import Data.Tree

import Foreign.ForeignPtr (withForeignPtr)
import Foreign            (castPtr)
import System.IO.Unsafe   (unsafePerformIO)

import qualified Data.ByteString      as BS
import qualified Data.IntMap.Strict   as IntMap
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Data.Vector.Storable as Vector
import qualified Data.Tree.Zipper     as Zipper

import qualified Data.Vector as RegularVector

num :: (Integral a, Num b) => a -> b
num = fromIntegral

data Concrete

class Machine' e where
  data Byte e
  data Word e
  data Blob e
  data Memory e

  w256 :: W256 -> Word e
  blob :: ByteString -> Blob e

  sdiv :: Word e -> Word e -> Word e
  slt  :: Word e -> Word e -> Word e
  sgt  :: Word e -> Word e -> Word e
  smod :: Word e -> Word e -> Word e
  addmod :: Word e -> Word e -> Word e -> Word e
  mulmod :: Word e -> Word e -> Word e -> Word e

  exponentiate :: Word e -> Word e -> Word e

  forceConcreteBlob :: Blob e -> ByteString
  forceConcreteWord :: Word e -> W256

  sliceMemory :: Word e -> Word e -> Memory e -> Blob e
  writeMemory :: Blob e -> Word e -> Word e -> Word e -> Memory e -> Memory e

  readMemoryWord :: Word e -> Memory e -> Word e
  setMemoryWord :: Word e -> Word e -> Memory e -> Memory e

  readBlobWord :: Word e -> Blob e -> Word e
  blobSize :: Blob e -> Word e

  keccakBlob :: Blob e -> Word e

type Machine e =
  ( Machine' e
  , Show (Word e)
  , Num (Word e)
  , Num (Byte e)
  , Integral (Word e)
  , Bits (Word e)
  , Monoid (Memory e)
  , Monoid (Blob e)
  , IsString (Blob e)
  , Show (Blob e)
  , At (Memory e)
  , Index (Memory e) ~ Word e
  , IxValue (Memory e) ~ Byte e
  )

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

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

wordAt :: Int -> ByteString -> W256
wordAt i bs = word [(bs ^? ix j) ?: 0 | j <- [i..(i+31)]]

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

word :: Integral a => [a] -> W256
word xs = sum [ num x `shiftL` (8*n)
              | (n, x) <- zip [0..] (reverse xs) ]

toWord512 :: W256 -> Word512
toWord512 (W256 x) = fromHiAndLo 0 x

fromWord512 :: Word512 -> W256
fromWord512 x = W256 (loWord x)

instance Machine' Concrete where
  w256 = C
  blob = B

  newtype Word Concrete = C W256
  newtype Blob Concrete = B ByteString
  newtype Byte Concrete = ConcreteByte Word8
  newtype Memory Concrete = ConcreteMemory (IntMap (Byte Concrete))

  exponentiate (C x) (C y) = C (x ^ y)

  sdiv _ (C (W256 0)) = 0
  sdiv (C (W256 x)) (C (W256 y)) =
    let sx = signedWord x
        sy = signedWord y
        k  = if (sx < 0) /= (sy < 0)
             then (-1)
             else 1
    in C . W256 . unsignedWord $ k * div (abs sx) (abs sy)

  smod _ (C (W256 0)) = 0
  smod (C (W256 x)) (C (W256 y)) =
    let sx = signedWord x
        sy = signedWord y
        k  = if sx < 0 then (-1) else 1
    in C . W256 . unsignedWord $ k * mod (abs sx) (abs sy)

  addmod _ _ (C (W256 0)) = 0
  addmod (C x) (C y) (C z) =
    C $ fromWord512
         ((toWord512 x + toWord512 y) `mod` (toWord512 z))

  mulmod _ _ (C (W256 0)) = 0
  mulmod (C x) (C y) (C z) =
    C $ fromWord512
          ((toWord512 x * toWord512 y) `mod` (toWord512 z))

  slt (C (W256 x)) (C (W256 y)) =
    if signedWord x < signedWord y then 1 else 0

  sgt (C (W256 x)) (C (W256 y)) =
    if signedWord x > signedWord y then 1 else 0

  forceConcreteBlob (B x) = x
  forceConcreteWord (C x) = x

  sliceMemory o s (ConcreteMemory m) =
    B $ readIntMap (num o) (num s) (fmap num m)

  writeMemory (B bs) (C size) (C xOffset) (C yOffset) (ConcreteMemory mem') =
    let mem = fmap num mem'
    in ConcreteMemory . fmap num . snd $
         BS.foldl' (\(i, a) x -> (i + 1, IntMap.insert (num i) x a)) (yOffset, mem)
           (BS.take (num size) (BS.drop (num xOffset) bs))

  readMemoryWord (C i) (ConcreteMemory m) =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ IntMap.findWithDefault 0 (num i + n) m)
                                (8 * (31 - n))) (n - 1)
    in {-# SCC word256At_getter #-}
      C $ go (0 :: W256) (31 :: Int)

  setMemoryWord (C i) (C x) (ConcreteMemory m) =
    -- known to be relatively slow
    ConcreteMemory $
      IntMap.union (IntMap.fromAscList [(num i + 31 - j, byteAt x j) | j <- reverse [0..31]]) m

  readBlobWord (C i) (B x) =
    C (wordAt (num i) x)

  blobSize (B x) = C (num (BS.length x))

  keccakBlob (B x) = C (keccak x)

type instance Index (Memory Concrete) = Word Concrete
type instance IxValue (Memory Concrete) = Byte Concrete

deriving instance Bits (Word Concrete)
deriving instance Enum (Word Concrete)
deriving instance Eq (Word Concrete)
deriving instance Integral (Word Concrete)
deriving instance Num (Word Concrete)
deriving instance Real (Word Concrete)
deriving instance Show (Word Concrete)

deriving instance Bits (Byte Concrete)
deriving instance Enum (Byte Concrete)
deriving instance Eq (Byte Concrete)
deriving instance Integral (Byte Concrete)
deriving instance Num (Byte Concrete)
deriving instance Ord (Byte Concrete)
deriving instance Real (Byte Concrete)

-- deriving instance Monoid (Word Concrete)
deriving instance Monoid (Blob Concrete)
deriving instance IsString (Blob Concrete)
deriving instance Show (Blob Concrete)

deriving instance Monoid (Memory Concrete)
deriving instance Ord (Word Concrete)

instance At (Memory Concrete) where
  at k f (ConcreteMemory m) =
    fmap ConcreteMemory $ f mv <&> \r -> case r of
      Nothing -> maybe m (const (IntMap.delete (num k) m)) mv
      Just v' -> IntMap.insert (num k) v' m
      where mv = IntMap.lookup (num k) m

instance Ixed (Memory Concrete) where
  ix k f (ConcreteMemory m) =
    fmap ConcreteMemory $ case IntMap.lookup (num k) m of
     Just v -> f v <&> \v' -> IntMap.insert (num k) v' m
     Nothing -> pure m

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
  | OpRevert
  | OpSelfdestruct
  | OpDup !Word8
  | OpSwap !Word8
  | OpLog !Word8
  | OpPush !W256
  | OpUnknown Word8
  deriving (Show, Eq)

data Error e
  = BalanceTooLow (Word e) (Word e)
  | UnrecognizedOpcode Word8
  | SelfDestruction
  | StackUnderrun
  | BadJumpDestination
  | Revert
  | NoSuchContract Addr

deriving instance Show (Error Concrete)

-- type Error = Error' Concrete

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
  , _selfdestructs :: [Addr]
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
  , _code        :: ByteString
  , _pc          :: Int
  , _stack       :: [Word e]
  , _memory      :: Memory e
  , _memorySize  :: Int
  , _calldata    :: Blob e
  , _callvalue   :: Word e
  , _caller      :: Addr
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

type EVM e a = State (VM e) a

currentContract :: Machine e => VM e -> Maybe (Contract e)
currentContract vm =
  view (env . contracts . at (view (state . contract) vm)) vm

zipperRootForest :: Zipper.TreePos Zipper.Empty a -> Forest a
zipperRootForest z =
  case Zipper.parent z of
    Nothing -> Zipper.toForest z
    Just z' -> zipperRootForest (Zipper.nextSpace z')

contextTraceForest :: Machine e => VM e -> Forest (Either (Log e) (FrameContext e))
contextTraceForest vm =
  view (contextTrace . to zipperRootForest) vm

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

exec1 :: forall e. Machine e => EVM e ()
exec1 = do
  vm <- get

  let
    -- Convenience function to access parts of the current VM state.
    -- Arcane type signature needed to avoid monomorphism restriction.
    the :: (b -> VM e -> Const a (VM e)) -> ((a -> Const a a) -> b) -> a
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
          assign result (Just (VMSuccess (blob "")))

    else do
      let op = BS.index (the state code) (the state pc)
      state . pc += opSize op

      case op of

        -- op: PUSH
        x | x >= 0x60 && x <= 0x7f ->
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + the state pc)
                                       (the state code))
          in push (w256 (word (BS.unpack xs)))

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
                    log           = Log self bytes topics
                assign (state . stack) xs'
                pushToSequence logs log
                modifying contextTrace $ \t ->
                  Zipper.nextSpace (Zipper.insert (Node (Left log) []) t)
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
          stackOp2 (uncurry (sdiv))

        -- op: MOD
        0x06 -> stackOp2 $ \case
          (_, 0) -> 0
          (x, y) -> mod x y

        -- op: SMOD
        0x07 -> stackOp2 $ uncurry smod
        -- op: ADDMOD
        0x08 -> stackOp3 $ (\(x, y, z) -> addmod x y z)
        -- op: MULMOD
        0x09 -> stackOp3 $ (\(x, y, z) -> mulmod x y z)

        -- op: LT
        0x10 -> stackOp2 $ \(x, y) -> if x < y then 1 else 0
        -- op: GT
        0x11 -> stackOp2 $ \(x, y) -> if x > y then 1 else 0
        -- op: SLT
        0x12 -> stackOp2 $ uncurry slt
        -- op: SGT
        0x13 -> stackOp2 $ uncurry sgt

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
                  hash  = keccakBlob bytes
              assign (state . stack) (hash : xs)
              assign (env . sha3Crack . at hash) (Just bytes)
              accessMemoryRange (num xOffset) (num xSize)
            _ -> underrun

        -- op: ADDRESS
        0x30 -> push (num (the state contract))

        -- op: BALANCE
        0x31 ->
          case stk of
            (x:xs) -> do
              assign (state . stack) xs
              touchAccount (num x) >>= push . view balance
            [] ->
              underrun

        -- op: ORIGIN
        0x32 -> push (num (the env origin))

        -- op: CALLER
        0x33 -> push (num (the state caller))

        -- op: CALLVALUE
        0x34 -> push (the state callvalue)

        -- op: CALLDATALOAD
        0x35 -> stackOp1 $ \x -> readBlobWord x (the state calldata)

        -- op: CALLDATASIZE
        0x36 -> push (blobSize (the state calldata))

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
              copyBytesToMemory (blob (view bytecode this))
                (num n) (num codeOffset) (num memOffset)
            _ -> underrun

        -- op: GASPRICE
        0x3a ->
          push 0

        -- op: EXTCODESIZE
        0x3b ->
          case stk of
            (x:xs) -> do
              assign (state . stack) xs
              touchAccount (num x) >>= push . num . view codesize
            [] ->
              underrun

        -- op: EXTCODECOPY
        0x3c ->
          case stk of
            (extAccount:memOffset:codeOffset:codeSize:xs) -> do
              c <- touchAccount (num (extAccount))
              assign (state . stack) xs
              copyBytesToMemory (blob (view bytecode c))
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
        0x5a -> push (w256 0xffffffffffffffffff)

        -- op: JUMPDEST
        0x5b -> return ()

        -- op: EXP
        0x0a ->
          stackOp2 (uncurry exponentiate)

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
            (xValue:_:_:_) | xValue > view balance this -> do
              vmError (BalanceTooLow (view balance this) xValue)

            (xValue:xOffset:xSize:xs) -> do
              accessMemoryRange xOffset xSize

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
                  & set code       newCode
                  & set callvalue  xValue
                  & set caller     self

            _ -> underrun

        -- op: CALL
        0xf1 ->
          case stk of
            (_:_:xValue:_:_:_:_:_) | xValue > view balance this -> do
              vmError (BalanceTooLow (view balance this) xValue)
            (_:xTo:xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs) -> do
              delegateCall (num xTo) xInOffset xInSize xOutOffset xOutSize xs
              zoom state $ do
                assign callvalue xValue
                assign caller (the state contract)
                assign contract (num xTo)
              zoom (env . contracts) $ do
                ix self      . balance -= xValue
                ix (num xTo) . balance += xValue
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
                      push (num (the state contract))

                    CallContext yOffset ySize _ _ _ -> do
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
              vmError SelfDestruction

        -- op: REVERT
        0xfe ->
          vmError Revert

        xxx ->
          vmError (UnrecognizedOpcode xxx)

delegateCall
  :: Machine e
  => Addr
  -> Word e -> Word e -> Word e -> Word e -> [Word e]
  -> EVM e ()
delegateCall xTo xInOffset xInSize xOutOffset xOutSize xs = do
  preuse (env . contracts . ix xTo) >>=
    \case
      Nothing -> vmError (NoSuchContract xTo)
      Just target -> do
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
          assign pc 0
          assign code (view bytecode target)
          assign stack mempty
          assign memory mempty
          assign calldata (readMemory (num xInOffset) (num xInSize) vm)

        accessMemoryRange xInOffset xInSize
        accessMemoryRange xOutOffset xOutSize

accessMemoryRange :: Machine e => Word e -> Word e -> EVM e ()
accessMemoryRange _ 0 = return ()
accessMemoryRange f l =
  state . memorySize %= \n -> max n (ceilDiv (num (f + l)) 32)
  where
    ceilDiv a b =
      let (q, r) = quotRem a b
      in q + if r /= 0 then 1 else 0

accessMemoryWord :: Machine e => Word e -> EVM e ()
accessMemoryWord x = accessMemoryRange x 32

copyBytesToMemory
  :: Machine e => Blob e -> Word e -> Word e -> Word e -> EVM e ()
copyBytesToMemory bs size xOffset yOffset =
  if size == 0 then return ()
  else do
    mem <- use (state . memory)
    assign (state . memory) $
      writeMemory bs size xOffset yOffset mem

readMemory :: Machine e => Word e -> Word e -> VM e -> Blob e
readMemory offset size vm = sliceMemory offset size (view (state . memory) vm)

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
          push 0
          let self = vm ^. state . contract
          assign (env . contracts . at self) Nothing

        CallContext _ _ _ _ reversion -> do
          assign frames remainingFrames
          assign state (view frameState nextFrame)
          assign (env . contracts) reversion
          push 0

stackOp1 :: Machine e => (Word e -> Word e) -> EVM e ()
stackOp1 f =
  use (state . stack) >>= \case
    (x:xs) ->
      let !y = f x in
      state . stack .= y : xs
    _ ->
      underrun

stackOp2 :: Machine e => ((Word e, Word e) -> Word e) -> EVM e ()
stackOp2 f =
  use (state . stack) >>= \case
    (x:y:xs) ->
      state . stack .= f (x, y) : xs
    _ ->
      underrun

stackOp3 :: Machine e => ((Word e, Word e, Word e) -> Word e) -> EVM e ()
stackOp3 f =
  use (state . stack) >>= \case
    (x:y:z:xs) ->
      state . stack .= f (x, y, z) : xs
    _ ->
      underrun

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

insidePushData :: Machine e => Int -> EVM e Bool
insidePushData i = do
  -- If the operation index for the code pointer is the same
  -- as for the previous code pointer, then it's inside push data.
  self <- use (state . contract)
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
  , _selfdestructs = mempty
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
    , _calldata = B $ vmoptCalldata o
    , _callvalue = C $ vmoptValue o
    , _caller = vmoptCaller o
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

word32 :: Integral a => [a] -> Word32
word32 xs = sum [ num x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

word256At :: Machine e => Word e -> Lens' (Memory e) (Word e)
word256At i = lens getter setter where
  getter m = readMemoryWord i m
    -- let
    --   go !a (-1) = a
    --   go !a !n = go (a + shiftL (num $ IntMap.findWithDefault 0 (i + n) m)
    --                             (8 * (31 - n))) (n - 1)
    -- in {-# SCC word256At_getter #-}
    --   go (0 :: W256) (31 :: Int)
  setter m x = setMemoryWord i x m
    -- {-# SCC word256At_setter #-}
    -- -- Optimizing this would help significantly.
    -- IntMap.union (IntMap.fromAscList [(i + 31 - j, byteAt x j) | j <- reverse [0..31]]) m

word32At :: Int -> Lens' (IntMap Word8) Word32
word32At i = lens getter setter where
  getter m =
    word32 [(m ^? ix (i + j)) ?: 0 | j <- [0..3]]
  setter m x =
    IntMap.union (IntMap.fromAscList [(i + 3 - j, byteAt x j) | j <- reverse [0..3]]) m

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

vmOp :: Machine e => VM e -> Maybe Op
vmOp vm =
  let i  = vm ^. state . pc
      xs = BS.drop i (vm ^. state . code)
      op = BS.index xs 0
  in if BS.null xs
     then Nothing
     else Just (readOp op (BS.drop 1 xs))

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
  0xfe -> OpRevert
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
