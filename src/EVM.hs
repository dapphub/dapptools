{-# Language BangPatterns #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language FlexibleContexts #-}

module EVM where

import Prelude hiding ((^))

import EVM.Types
import EVM.Solidity
import EVM.Keccak

-- Bits and words
import Data.Bits (xor, shiftL, shiftR, (.&.), (.|.))
import Data.Bits (Bits, bit, testBit, complement)
import Data.DoubleWord (Int256)
import Data.DoubleWord (loWord, signedWord, unsignedWord, fromHiAndLo)
import Data.Word (Word8, Word32)

-- We make heavy use of the lens library for nested field access
import Control.Lens hiding (op, (:<), (|>))

-- Various data structures
import Data.ByteString             (ByteString)
import Data.Map.Strict             (Map, union, fromList)
import Data.Maybe                  (fromMaybe)
import Data.Sequence               (Seq)
import Data.Vector.Unboxed         (Vector)
import Data.Vector.Unboxed.Mutable (new, write)

import qualified Data.ByteString      as BS
import qualified Data.Map.Strict      as Map
import qualified Data.Sequence        as Seq
import qualified Data.Vector.Unboxed  as Vector

-- The different result states of a VM
data VMResult
  = VMRunning
  | VMFailure
  | VMSuccess !ByteString
  deriving (Eq, Show)

-- Main structure of the program: state of a stepwise EVM execution.
data VM = VM
  { _result      :: !VMResult
  , _state       :: !FrameState
  , _frames      :: ![Frame]
  , _env         :: !Env
  , _block       :: !Block
  , _suicides    :: ![Addr]
  , _memorySize  :: !W256
  } deriving Show

-- A frame is an entry in the VM's "call stack"
-- with the context needed to return from a CALL/CREATE
-- along with trace information.
data Frame = Frame
  { _frameContext   :: !FrameContext
  , _frameState     :: !FrameState
  , _frameTrace     :: !FrameTrace
  } deriving Show

-- When returning from a CALL, we save return data in memory
-- and put the error flag on the stack.
--
-- When returning from a CREATE -- we save return data as contract code
-- and put the new address on the stack.
data FrameContext
  = CallContext
    { callContextOffset :: W256
    , callContextSize   :: W256 }
  | CreationContext
  deriving Show

-- Used to associate a frame with contract and method.
--
-- TODO: Put this in the frame context instead.
--       Different info is anyway needed for CALL vs CREATE.
data FrameTrace = FrameTrace
  { _traceCodehash :: W256
  , _traceAbi      :: Maybe Word32
  } deriving Show

-- The ``registers'' of the VM along with memory and data stack.
data FrameState = FrameState
  { _contract    :: !Addr
  , _code        :: !ByteString
  , _pc          :: !Int
  , _stack       :: ![W256]
  , _memory      :: !(Map W256 Word8)
  , _calldata    :: !ByteString
  , _callvalue   :: !W256
  , _caller      :: !Addr
  } deriving Show

-- The state of a contract.
data Contract = Contract
  { _bytecode :: !ByteString
  , _storage  :: !(Map W256 W256)
  , _balance  :: !W256
  , _nonce    :: !W256
  , _codehash :: !W256
  , _codesize :: !Int -- (redundant?)
  , _opIxMap  :: !(Vector Int)
  } deriving (Eq, Show)

-- Kind of a hodgepodge?
data Env = Env
  { _contracts   :: !(Map Addr Contract)
  , _solc        :: (Map Addr SolcContract)
  , _sha3Crack   :: (Map W256 ByteString)
  , _sourceCache :: SourceCache
  , _origin      :: Addr
  } deriving (Show)

data Block = Block
  { _coinbase   :: !Addr
  , _timestamp  :: !W256
  , _number     :: !W256
  , _difficulty :: !W256
  , _gaslimit   :: !W256
  } deriving Show

blankState :: FrameState
blankState = FrameState
  { _contract  = 0
  , _code      = mempty
  , _pc        = 0
  , _stack     = mempty
  , _memory    = mempty
  , _calldata  = mempty
  , _callvalue = 0
  , _caller    = 0
  }

makeLenses ''FrameState
makeLenses ''FrameTrace
makeLenses ''Frame
makeLenses ''Block
makeLenses ''Contract
makeLenses ''Env
makeLenses ''VM

initialContract :: ByteString -> Contract
initialContract theCode = Contract
  { _bytecode = theCode
  , _codesize = BS.length theCode
  , _codehash = keccak theCode
  , _storage  = mempty
  , _balance  = 0
  , _nonce    = 0
  , _opIxMap  = mkOpIxMap theCode
  }

ceilDiv :: Integral a => a -> a -> a
ceilDiv a b =
  let (q, r) = quotRem a b
  in q + if r /= 0 then 1 else 0

accessMemoryRange :: W256 -> W256 -> VM -> VM
accessMemoryRange _ 0 vm = vm
accessMemoryRange f l vm =
  vm & memorySize .~ max (vm ^. memorySize) (ceilDiv (f + l) 32)

accessMemoryWord :: W256 -> VM -> VM
accessMemoryWord x = accessMemoryRange x 32

copyBytesToMemory
  :: (IxValue s ~ Word8, Ixed s, Num (Index s))
  => s -> W256 -> W256 -> W256 -> VM -> VM
copyBytesToMemory bs size xOffset yOffset vm =
  if size == 0 then vm
  else vm & state . memory .~
              flip union (vm ^. state . memory)
                (fromList
                  [ (yOffset + i, (bs ^? ix (num (xOffset + i))) ?: 0)
                    | i <- [0 .. size - 1] ])

readMemory :: W256 -> W256 -> VM -> ByteString
readMemory offset size vm =
  if size == 0 then ""
  else
    let mem = vm ^. state . memory
    in BS.pack [(Map.lookup (offset + i) mem) ?: 0
                | i <- [0..size-1]]

exec1 :: VM -> IO VM
exec1 vm = do

  -- short name for the current contract
  let Just !c = vm ^? env . contracts . ix (vm ^. state . contract)

  -- inside code bounds?
  if vm ^. state . pc < BS.length (vm ^. state . code)
    then let

      -- old vm with updated code pointer
      vm' = vm & state . pc +~ opSize op

      -- current operation byte
      op  = BS.index (vm ^. state . code) (vm ^. state . pc)

      -- short names for current memory and stack
      mem = vm ^. state . memory
      stk = vm ^. state . stack

    in case op of

        -- op: PUSH
        x | x >= 0x60 && x <= 0x7f ->
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + vm ^. state . pc)
                                       (vm ^. state . code))
          in stackOp0_1 vm' (word (BS.unpack xs))

        -- op: DUP
        x | x >= 0x80 && x <= 0x8f ->
          let !i = x - 0x80 + 1 in
          if length stk < num i
          then underrun vm
          else
            let !y = vm ^?! state . stack . ix (num i - 1)
            in return $!
                 vm' & state . stack %~ (y:)

        -- op: SWAP
        x | x >= 0x90 && x <= 0x9f ->
          let !i = x - 0x90 + 1 in
          if length stk < num i + 1
          then underrun vm
          else return $!
            vm' & state . stack . ix 0 .~
                    (vm ^?! state . stack . ix (num i))
                & state . stack . ix (num i) .~
                    (vm ^?! state . stack . ix 0)

        -- op: LOG
        x | x >= 0xa0 && x <= 0xa4 ->
          let !n = x - 0xa0 + 1 in
          return $! vm' & state . stack %~ drop (num n + 1)

        -- op: STOP
        0x00 ->
          case vm ^. frames of
            [] -> return $! vm' & result .~ VMSuccess ""
            (f:fs) -> do
              return $! vm'
                & frames .~ fs
                & state .~ f ^. frameState
                & state . stack %~ (1 :)

        -- op: ADD
        0x01 -> stackOp2_1 vm' (+)

        -- op: MUL
        0x02 -> stackOp2_1 vm' (*)

        -- op: SUB
        0x03 -> stackOp2_1 vm' (-)

        -- op: DIV
        0x04 -> stackOp2_1 vm' $ \x -> \case 0 -> 0; y -> div x y

        -- op: SDIV
        0x05 ->
          stackOp2_1 vm' $ \(W256 x) ->
            \case
              0 -> 0
              (W256 y) ->
                let sx = signedWord x
                    sy = signedWord y
                    k  = if (sx < 0) /= (sy < 0)
                         then (-1)
                         else 1
                in W256 . unsignedWord $ k * div (abs sx) (abs sy)

        -- op: MOD
        0x06 -> stackOp2_1 vm' $ \x -> \case 0 -> 0; y -> mod x y

        -- op: SMOD
        0x07 ->
          stackOp2_1 vm' $ \(W256 x) ->
            \case
              0 -> 0
              (W256 y) ->
                let sx = signedWord x
                    sy = signedWord y
                    k  = if sx < 0 then (-1) else 1
                in W256 . unsignedWord $ k * mod (abs sx) (abs sy)

        -- op: ADDMOD
        0x08 ->
          stackOp3_1 vm'
            (\x y z -> if z == 0 then 0 else fromWord512
              ((toWord512 x + toWord512 y) `mod` (toWord512 z)))

        -- op: MULMOD
        0x09 ->
          stackOp3_1 vm'
            (\x y z -> if z == 0 then 0 else fromWord512
              ((toWord512 x * toWord512 y) `mod` (toWord512 z)))

        -- op: LT
        0x10 -> stackOp2_1 vm' (\x y -> if x < y then 1 else 0)

        -- op: GT
        0x11 -> stackOp2_1 vm' (\x y -> if x > y then 1 else 0)

        -- op: SLT
        0x12 ->
          stackOp2_1 vm' $ \x y ->
            if (num x :: Int256) < (num y :: Int256) then 1 else 0

        -- op: SGT
        0x13 ->
          stackOp2_1 vm' $ \x y ->
            if (num x :: Int256) > (num y :: Int256) then 1 else 0

        -- op: EQ
        0x14 -> stackOp2_1 vm' (\x y -> if x == y then 1 else 0)

        -- op: ISZERO
        0x15 -> stackOp1_1 vm' (\case 0 -> 1; _ -> 0)

        -- op: AND
        0x16 -> stackOp2_1 vm' (.&.)

        -- op: OR
        0x17 -> stackOp2_1 vm' (.|.)

        -- op: XOR
        0x18 -> stackOp2_1 vm' xor

        -- op: NOT
        0x19 -> stackOp1_1 vm' complement

        -- op: BYTE
        0x1a -> stackOp2_1 vm' $ \n x ->
                  if n >= 32 then 0
                  else (shiftR x (8 * (31 - num n))) .&. 0xff

        -- op: SHA3
        0x20 ->
          case stk of
            (xOffset:xSize:xs) ->
              let bytes = readMemory xOffset xSize vm
                  hash  = keccak bytes
              in do
                return $! vm'
                  & state . stack             .~ hash : xs
                  & env . sha3Crack . at hash .~ Just bytes
                  & accessMemoryRange xOffset xSize
            _ -> underrun vm

        -- op: ADDRESS
        0x30 -> stackOp0_1 vm' (vm ^. state . contract)

        -- op: BALANCE
        0x31 -> stackOp1_1 vm' $ \x ->
          (vm ^? env . contracts . ix (num x) . balance) ?: 0

        -- op: ORIGIN
        0x32 -> stackOp0_1 vm' (vm ^. env . origin)

        -- op: CALLER
        0x33 -> stackOp0_1 vm' (vm ^. state . caller)

        -- op: CALLVALUE
        0x34 -> stackOp0_1 vm' (vm ^. state . callvalue)

        -- op: CALLDATALOAD
        0x35 -> stackOp1_1 vm' $ \x -> wordAt (num x) (vm ^. state . calldata)

        -- op: CALLDATASIZE
        0x36 -> stackOp0_1 vm' $ BS.length (vm ^. state . calldata)

        -- op: CALLDATACOPY
        0x37 ->
          case stk of
            (toOffset:fromOffset:theSize:xs) ->
              return $!
                vm' & state . stack .~ xs
                    & copyBytesToMemory
                        (vm ^. state . calldata)
                        theSize fromOffset toOffset
            _ -> underrun vm

        -- op: CODESIZE
        0x38 ->
          stackOp0_1 vm' (BS.length (vm ^. state . code))

        -- op: CODECOPY
        0x39 ->
          case stk of
            (memOffset:codeOffset:codeSize:xs) -> return $!
              vm' & state . stack .~ xs
                  & copyBytesToMemory
                      (c ^. bytecode)
                      codeSize codeOffset memOffset
            _ -> underrun vm

        -- op: GASPRICE
        0x3a ->
          stackOp0_1 vm' (0 :: W256)

        -- op: EXTCODESIZE
        0x3b ->
          stackOp1_1 vm'
            (\x -> num ((vm ^? env . contracts . ix (num x) . codesize) ?: 0))

        -- op: EXTCODECOPY
        0x3c ->
          case stk of
            (extAccount:memOffset:codeOffset:codeSize:xs) ->
              let theCode =
                    (vm ^? env . contracts . ix (num extAccount) . bytecode)
                      ?: mempty
              in return $!
                   vm' & state . stack .~ xs
                       & copyBytesToMemory
                           theCode codeSize codeOffset memOffset
            _ -> underrun vm

        -- op: BLOCKHASH
        0x40 ->
          -- fake zero block hashes everywhere
          stackOp1_1 vm' (const 0)

        -- op: COINBASE
        0x41 -> stackOp0_1 vm' (vm ^. block . coinbase)

        -- op: TIMESTAMP
        0x42 -> stackOp0_1 vm' (vm ^. block . timestamp)

        -- op: NUMBER
        0x43 -> stackOp0_1 vm' (vm ^. block . number)

        -- op: DIFFICULTY
        0x44 -> stackOp0_1 vm' (vm ^. block . difficulty)

        -- op: GASLIMIT
        0x45 -> stackOp0_1 vm' (vm ^. block . gaslimit)

        -- op: POP
        0x50 ->
          case stk of
            (_:xs) -> return $! vm' & state . stack .~ xs
            _ -> underrun vm

        -- op: MLOAD
        0x51 ->
          case vm ^. state . stack of
            (x:xs) -> return $!
              vm' & state . stack .~ (mem ^. word256At x : xs)
                  & accessMemoryWord x
            _ -> underrun vm

        -- op: MSTORE
        0x52 ->
          case stk of
            (x:y:xs) ->
              return $!
                vm' & state . memory . word256At x .~ y
                    & state . stack .~ xs
                    & accessMemoryWord x
            _ -> underrun vm

        -- op: MSTORE8
        0x53 ->
          case stk of
            (x:y:xs) ->
              return $!
                vm' & state . memory . at x .~ Just (num (y .&. 0xff))
                    & state . stack .~ xs
                    & accessMemoryRange x 1
            _ -> underrun vm

        -- op: SLOAD
        0x54 -> do
          stackOp1_1 vm' (\x -> (c ^. storage . at x) ?: 0)

        -- op: SSTORE
        0x55 -> do
          case stk of
            (x:y:xs) -> do
              return $!
                vm' & env . contracts . ix (vm ^. state . contract)
                          . storage . at x .~ (if y == 0
                                               then Nothing
                                               else Just y)
                    & state . stack .~ xs
            _ -> underrun vm

        -- op: JUMP
        0x56 ->
          case stk of
            (x:xs) -> checkJump (vm & state . stack .~ xs) x
            _ -> underrun vm

        -- op: JUMPI
        0x57 -> do
          case stk of
            (x:y:xs) ->
              if y == 0
              then return $! vm' & state . stack .~ xs
              else checkJump (vm & state . stack .~ xs) x
            _ -> underrun vm

        -- op: PC
        0x58 ->
          stackOp0_1 vm' (vm ^. state . pc)

        -- op: MSIZE
        0x59 ->
          stackOp0_1 vm' (vm ^. memorySize)

        -- op: GAS
        0x5a -> stackOp0_1 vm' (0xffffffffffffffffff :: W256)

        -- op: JUMPDEST
        0x5b -> return $! vm'

        -- op: EXP
        0x0a ->
          stackOp2_1 vm' (^)

        -- op: SIGNEXTEND
        0x0b ->
          stackOp2_1 vm' $ \bytes x ->
            if bytes >= 32 then x
            else let n = num bytes * 8 + 7 in
              if testBit x n
              then x .|. complement (bit n - 1)
              else x .&. (bit n - 1)

        -- op: CREATE
        0xf0 -> do
          case stk of
            (xValue:xOffset:xSize:xs) ->
              let address' =
                    newContractAddress (vm' ^. state . contract) (c ^. nonce)
                  xCode = readMemory xOffset xSize vm
                  contract' = initialContract mempty
                  contracts' =
                    (vm ^. env . contracts) & at address' .~ (Just $! contract')
                                            & ix (vm ^. state . contract) . nonce +~ 1
              in return $! vm'
                & accessMemoryRange xOffset xSize
                & env . contracts .~ contracts'
                & frames %~
                    (Frame
                     { _frameContext = CreationContext
                     , _frameState   = ((vm' ^. state) & stack .~ xs)
                     , _frameTrace   = FrameTrace
                         { _traceCodehash = 0
                         , _traceAbi      = Nothing
                         }
                     } :)
                & state .~
                    (blankState
                      & code      .~ xCode
                      & callvalue .~ xValue
                      & caller    .~ (vm ^. state . contract)
                      & contract  .~ address')
            _ -> underrun vm

        -- op: CALL
        0xf1 ->
          case stk of
            (_:xTo:xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs) ->
              case vm ^? env . contracts . ix (num xTo) . bytecode of
                Nothing ->
                  returnOp 0 (0, 0) vm
                Just newCode ->
                  let frame' =
                        Frame
                          { _frameContext = CallContext xOutOffset xOutSize
                          , _frameState = ((vm' ^. state) & stack .~ xs)
                          , _frameTrace = FrameTrace
                            { _traceCodehash =
                                vm ^?! env . contracts . ix (num xTo) . codehash
                            , _traceAbi =
                                if xInSize >= 4
                                then Just $! mem ^. word32At xInOffset
                                else Nothing
                            }
                          }
                      state' = (vm' ^. state)
                        & pc        .~ 0
                        & code      .~ newCode
                        & stack     .~ mempty
                        & memory    .~ mempty
                        & contract  .~ num xTo
                        & calldata  .~ readMemory xInOffset xInSize vm
                        & callvalue .~ xValue
                        & caller    .~ (vm ^. state . contract)
                  in return $! vm'
                       & frames %~ (frame' :)
                       & state .~ state'
                       & accessMemoryRange xInOffset xInSize
                       & accessMemoryRange xOutOffset xOutSize

            _ -> underrun vm

        -- op: CALLCODE
        0xf2 ->
          error "CALLCODE not supported (use DELEGATECALL)"

        -- op: RETURN
        0xf3 ->
          case stk of
            (xOffset:xSize:_) ->
              case vm ^. frames of
                [] ->
                  return $! vm'
                    & result .~ VMSuccess (readMemory xOffset xSize vm)
                (f:fs) ->
                  case f ^. frameContext of
                    CreationContext ->
                      return $! vm'
                        & frames .~ fs
                        & state .~ f ^. frameState
                        & state . stack %~ (num (vm ^. state . contract) :)
                        & accessMemoryRange xOffset xSize
                        & env . contracts . at (vm ^. state . contract) .~
                            if xSize == 0
                            then Nothing
                            else let created =
                                       vm ^?! env . contracts . ix (vm ^. state . contract)
                                     contract' =
                                       initialContract
                                        (readMemory xOffset xSize vm)
                                        & storage .~ (created ^. storage)
                                        & balance .~ (created ^. balance)
                                   in Just contract'

                    CallContext yOffset ySize ->
                      return $! vm'
                        & frames .~ fs
                        & state .~ f ^. frameState
                        & state . stack %~ (1 :)
                        & copyBytesToMemory mem ySize xOffset yOffset
                        & accessMemoryRange xOffset xSize

            _ -> underrun vm

        -- op: SUICIDE
        0xff ->
          case vm ^. state . stack of
            [] -> underrun vm
            (x:_) ->
              let self = vm ^. state . contract
                  vm'' = vm' & suicides %~ (self :)
                             & env . contracts . ix (num x) . balance +~
                                 (vm ^?! env . contracts . ix self . balance)
                             & env . contracts . ix self . balance .~ 0
              in returnOp 0 (0, 0) vm''

        x -> do
          error ("opcode " ++ show x)

    else
      case vm ^. frames of
        [] -> return $! vm & result .~ VMSuccess ""
        (f:fs) -> do
          return $! vm
            & frames .~ fs
            & state .~ f ^. frameState
            & state . stack %~ (1 :)

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

underrun :: Monad m => VM -> m VM
underrun vm = returnOp 0 (0, 0) vm

returnOp :: Monad m => W256 -> (W256, W256) -> VM -> m VM
returnOp returnCode (xOffset, xSize) vm =
  case vm ^. frames of
    [] -> return $!
      vm & result .~
             case returnCode of
               0 -> VMFailure
               _ -> VMSuccess (readMemory xOffset xSize vm)
    (f:fs) ->
      case f ^. frameContext of
        CreationContext ->
          return $! vm
            & (if xSize == 0
               then result .~ VMFailure -- because all gas is consumed
               else id)
            & accessMemoryRange xOffset xSize
            & frames .~ fs
            & state .~ (f ^. frameState)
            & state . stack %~ ((if xSize == 0 then 0 else num (vm ^. state . contract)) :)
            & env . contracts . at (vm ^. state . contract) .~
                if xSize == 0
                then Nothing
                else
                  let created =
                        vm ^?! env . contracts . ix (vm ^. state . contract)
                  in Just $ initialContract
                              (BS.pack
                                [ Map.findWithDefault 0 (xOffset + num i)
                                    (vm ^. state . memory)
                                  | i <- [0..xSize-1]])
                              & storage .~ (created ^. storage)
                              & balance .~ (created ^. balance)

        CallContext yOffset ySize ->
          return $! vm
            & accessMemoryRange xOffset xSize
            & frames .~ fs
            & state .~ f ^. frameState
            & state . stack %~ (returnCode :)
            & copyBytesToMemory
                (vm ^. state . memory)
                ySize xOffset yOffset

toWord512 :: W256 -> Word512
toWord512 (W256 x) = fromHiAndLo 0 x

fromWord512 :: Word512 -> W256
fromWord512 x = W256 (loWord x)

stackOp0_1 :: (Integral i, Monad m) => VM -> i -> m VM
stackOp0_1 vm !x =
  return $! vm & state . stack %~ (num x :)

stackOp1_1 :: Monad m => VM -> (W256 -> W256) -> m VM
stackOp1_1 vm f =
  case vm ^. state . stack of
    (x:xs) ->
      let !y = f x in
      return $! vm & state . stack .~ y : xs
    _ -> underrun vm

stackOp2_1 :: Monad m => VM -> (W256 -> W256 -> W256) -> m VM
stackOp2_1 vm f =
  case vm ^. state . stack of
    (x:y:xs) ->
      let !z = f x y in
      return $! vm & state . stack .~ z : xs
    _ -> underrun vm

stackOp3_1 :: Monad m => VM
  -> (W256 -> W256 -> W256 -> W256)
  -> m VM
stackOp3_1 vm f =
  case vm ^. state . stack of
    (x:y:z:xs) ->
      let !a = f x y z in
      return $! vm & state . stack .~ a : xs
    _ -> underrun vm

checkJump :: (Monad m, Integral n) => VM -> n -> m VM
checkJump vm x =
  let theCode = vm ^. state . code in
  if num x < BS.length theCode && BS.index theCode (num x) == 0x5b
  then return $! vm & state . pc .~ num x
  else error "bad jump destination"

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
  , _suicides = mempty
  , _memorySize = 0
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
    , _code = vmoptCode o
    , _contract = vmoptAddress o
    , _calldata = vmoptCalldata o
    , _callvalue = vmoptValue o
    , _caller = vmoptCaller o
    }
  , _env = Env
    { _sha3Crack = mempty
    , _sourceCache = mempty
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

word256At :: W256 -> Lens' (Map W256 Word8) W256
word256At i = lens getter setter where
  getter m =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ Map.findWithDefault 0 (i + num n) m)
                                (8 * (31 - n))) (n - 1)
    in {-# SCC word256At_getter #-}
      go (0 :: W256) (31 :: Int)
  setter m x =
    {-# SCC word256At_setter #-}
    -- Optimizing this would help significantly.
    union (fromList [(i + 31 - j, byteAt x (num j)) | j <- [0..31]]) m

word32At :: W256 -> Lens' (Map W256 Word8) Word32
word32At i = lens getter setter where
  getter m =
    word32 [(m ^? ix (i + j)) ?: 0 | j <- [0..3]]
  setter m x =
    union (fromList [(i + 3 - num j, byteAt x j) | j <- [0..3]]) m

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

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
  | OpSuicide
  | OpDup !Word8
  | OpSwap !Word8
  | OpLog !Word8
  | OpPush !W256
  | OpUnknown Word8
  deriving (Show, Eq)

vmOp :: VM -> Maybe Op
vmOp vm =
  let i  = vm ^. state . pc
      xs = BS.drop i (vm ^. state . code)
      op = BS.index xs 0
  in if BS.null xs
     then Nothing
     else Just (readOp op (BS.drop 1 xs))

readOp :: Word8 -> ByteString -> Op
readOp x _  | x >= 0x80 && x <= 0x8f = OpDup (x - 0x80 + 1)
readOp x _  | x >= 0x90 && x <= 0x9f = OpSwap (x - 0x90 + 1)
readOp x _  | x >= 0xa0 && x <= 0xa4 = OpLog (x - 0xa0 + 1)
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
  0xff -> OpSuicide
  _    -> (OpUnknown x)

codeOps :: ByteString -> Seq Op
codeOps xs =
  case BS.uncons xs of
    Nothing ->
      mempty
    Just (x, xs') ->
      readOp x xs' Seq.<| codeOps (BS.drop (opSize x) xs)

{-
  Unimplemented:
    callcode
    delegatecall
-}
