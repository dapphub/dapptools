{-# Language BangPatterns #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}

module EVM where

import Prelude hiding ((^))

import EVM.Solidity
import EVM.Keccak

import Data.DoubleWord

import Control.Lens hiding (op, (:<), (|>))
import Control.Monad

import Data.Bits
import Data.Maybe
import Data.Word

import System.Directory (setCurrentDirectory)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, union, fromList)

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Vector.Unboxed as Vector
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed.Mutable (new, write)

import IPPrint.Colored
import Debug.Trace

-- When invoking another contract, e.g., through CALL or CREATE, the
-- linkage is what we need to remember when we come back, aside from
-- all the VM state.  (The term `linkage' comes from SICP's
-- compiler chapter.)
data Linkage = CallLinkage Word256 Word256
             | CreateLinkage
               deriving Show

-- The VM maintains a stack of `frames' that save the VM state (plus
-- linkage) for every nested level of contract invocation.
data Frame = Frame {
  _frameContract  :: !Word256,
  _framePc        :: !Int,
  _frameStack     :: ![Word256],
  _frameMemory    :: !(Map Word256 Word8),
  _frameCalldata  :: !ByteString,
  _frameCallvalue :: !Word256,
  _frameCaller    :: !Word256,
  _frameLinkage   :: !Linkage,
  _frameTrace     :: (Word256, Maybe Word32) {- (codehash, abi) -}
} deriving Show
makeLenses ''Frame

data Contract = Contract {
  _code     :: !ByteString,
  _codehash :: !Word256,
  _codesize :: !Int,
  _storage  :: !(Map Word256 Word256),
  _balance  :: !Word256,
  _nonce    :: !Word256,
  _opIxMap  :: !(Vector Int)
} deriving Show
makeLenses ''Contract

data Env = Env {
  _contracts   :: !(Map Word256 Contract),
  _solc        :: (Map Word256 SolcContract),
  _sha3Crack   :: (Map Word256 [Word8]),
  _sourceCache :: SourceCache
} deriving (Show)
makeLenses ''Env

data VM = VM {
  _done        :: !Bool,
  _contract    :: !Word256,
  _vmCode      :: !ByteString,
  _pc          :: !Int,
  _stack       :: ![Word256],
  _memory      :: !(Map Word256 Word8),
  _calldata    :: !ByteString,
  _callvalue   :: !Word256,
  _callstack   :: ![Frame],
  _caller      :: !Word256,
  _env         :: !Env
} deriving Show
makeLenses ''VM

initialContract :: ByteString -> Contract
initialContract theCode = Contract {
  _code     = theCode,
  _codesize = BS.length theCode,
  _codehash = keccak theCode,
  _storage  = mempty,
  _balance  = 0,
  _nonce    = 0,
  _opIxMap  = mkOpIxMap theCode
}

exec1 :: VM -> IO VM
exec1 vm = do
  let Just !c = vm ^? env . contracts . ix (vm ^. contract)
  if vm ^. pc < BS.length (vm ^. vmCode)
    then
      let vm' = vm & pc +~ opSize op
          op  = BS.index (vm ^. vmCode) (vm ^. pc)
          mem = vm ^. memory
          stk = vm ^. stack
      in case op of

        x {- PUSH -} | x >= 0x60 && x <= 0x7f ->
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + vm ^. pc) (vm ^. vmCode))
          in stackOp0_1 vm' (word (BS.unpack xs))

        0x15 {- ISZERO -} ->
          stackOp1_1 vm' (\case 0 -> 1; _ -> 0)

        0x57 {- JUMPI -} -> do
          case stk of
            (x:y:xs) ->
              if y == 0 then return $! vm' & stack .~ xs
              else checkJump (vm & stack .~ xs) x
            _ -> error "underrun"

        0x56 {- JUMP -} ->
          case stk of
            (x:xs) -> checkJump (vm & stack .~ xs) x
            _ -> error "underrun"

        x | x >= 0x90 && x <= 0x9f ->
          let !i = x - 0x90 + 1 in
          if length stk < num i
          then error "underrun"
          else return $! vm' & stack . ix 0       .~ (vm ^?! stack . ix (num i))
                             & stack . ix (num i) .~ (vm ^?! stack . ix 0)

        x | x >= 0x80 && x <= 0x8f ->
          let !i = x - 0x80 + 1 in
          if length stk < num i
          then error "underrun"
          else let !y = (vm ^?! stack . ix (num i - 1)) in return $! vm' & stack %~ (y:)

        0x50 {- POP -} ->
          case stk of
            (_:xs) -> return $! vm' & stack .~ xs
            _ -> error "underrun"

        0x52 {- MSTORE -} ->
          case stk of
            (x:y:xs) ->
              return $!
                vm' & memory . word256At x .~ y
                    & stack .~ xs
            _ -> error "underrun"

        0x55 {- SSTORE -} -> do
          case stk of
            (x:y:xs) -> do
              return $! vm' & env . contracts . ix (vm ^. contract) . storage . at x .~ Just y
                            & stack .~ xs
            _ -> error "underrun"

        0x04 {- DIV -} ->
          stackOp2_1 vm' (\x -> \case 0 -> 0; y -> div x y)

        0x06 {- MOD -} ->
          stackOp2_1 vm' (\x -> \case 0 -> 0; y -> mod x y)

        0x16 {- AND -} ->
          stackOp2_1 vm' (.&.)

        0x17 {- OR -} ->
          stackOp2_1 vm' (.|.)

        0x19 {- NOT -} ->
          stackOp1_1 vm' complement

        0x02 {- MUL -} ->
          stackOp2_1 vm' (*)

        0x0a {- EXP -} ->
          stackOp2_1 vm' (^)

        0x01 {- ADD -} ->
          stackOp2_1 vm' (+)

        0x03 {- SUB -} ->
          stackOp2_1 vm' (-)

        0x14 {- EQ -} ->
          stackOp2_1 vm' (\x y -> if x == y then 1 else 0)

        0x10 {- LT -} ->
          stackOp2_1 vm' (\x y -> if x < y then 1 else 0)

        0x12 {- SLT -} ->
          stackOp2_1 vm' $ \x y ->
            if (num x :: Int256) < (num y :: Int256) then 1 else 0

        0x13 {- SGT -} ->
          stackOp2_1 vm' $ \x y ->
            if (num x :: Int256) > (num y :: Int256) then 1 else 0

        0x11 {- GT -} ->
          stackOp2_1 vm' (\x y -> if x > y then 1 else 0)

        0x5b {- JUMPDEST -} -> return $! vm'

        x {- LOG -} | x >= 0xa0 && x <= 0xa4 ->
          let !n = x - 0xa0 + 1 in
          return $! vm' & stack %~ drop (num n + 1)

        0x36 {- CALLDATASIZE -} ->
          stackOp0_1 vm' (num (BS.length (vm ^. calldata)))

        0x33 {- CALLER -} ->
          stackOp0_1 vm' (vm ^. caller)

        0x35 {- CALLDATALOAD -} ->
          stackOp1_1 vm'
            (\x -> wordAt (num x) (vm ^. calldata))

        0x37 {- CALLDATACOPY -} ->
          case stk of
            (toOffset:fromOffset:theSize:xs) ->
              let theCalldata = vm ^. calldata in
                return $!
                  vm' & stack .~ xs
                      & memory .~ flip union mem
                          (fromList [(toOffset + i,
                                      (vm ^? calldata . ix (num (fromOffset + i))) ?: 0)
                                     | i <- [0..theSize-1]])
            _ -> error "underrun"

        0x20 {- SHA3 -} ->
          case stk of
            (xOffset:xSize:xs) ->
              let m = vm' ^. memory
                  bytes = [(Map.lookup (xOffset + i) m) ?: 0
                           | i <- [0..xSize-1]]
                  hash = keccak (BS.pack bytes)
              in do
                -- cpprint ("SHA3 preimage", bytes)
                return $! vm'
                  & stack .~ hash : xs
                  & env . sha3Crack . at hash .~ Just bytes
            _ -> error "underrun"

        0x34 {- CALLVALUE -} ->
          stackOp0_1 vm' (vm ^. callvalue)

        0x30 {- CALLER -} ->
          stackOp0_1 vm' (vm ^. contract)

        0x51 {- MLOAD -} ->
          stackOp1_1 vm' (\x -> vm ^. memory . word256At x)

        0x54 {- SLOAD -} -> do
          stackOp1_1 vm' (\x -> (c ^. storage . at x) ?: 0)

        0x3b {- EXTCODESIZE -} ->
          stackOp1_1 vm'
            (\x -> num ((vm ^? env . contracts . ix x . codesize) ?: 0))

        0x5a {- GAS -} ->
          stackOp0_1 vm' 0xffffffffffffffffff

        0x42 {- TIMESTAMP -} ->
          stackOp0_1 vm' 1486299707

        0xf1 {- CALL -} ->
          case stk of
            (xGas:xTo:xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs) -> do
              let frame' = Frame (vm' ^. contract)
                                 (vm' ^. pc)
                                 xs
                                 (vm' ^. memory)
                                 (vm' ^. calldata)
                                 (vm' ^. callvalue)
                                 (vm' ^. caller)
                                 (CallLinkage xOutOffset xOutSize)
                                 (vm ^?! env . contracts . ix xTo . codehash,
                                  if xInSize >= 4
                                  then Just $! vm ^. memory . word32At xInOffset
                                  else Nothing)
              return $! vm'
                & callstack %~ (frame':)
                & vmCode   .~ (vm ^?! env . contracts . ix xTo . code)
                & pc       .~ 0
                & stack    .~ mempty
                & memory   .~ mempty
                & contract .~ xTo
                & calldata .~
                    (let m = vm' ^. memory in BS.pack [
                        (Map.lookup (xInOffset + i) m) ?: 0
                        | i <- [0..xInSize-1]
                     ])
                & callvalue .~ xValue
                & caller    .~ (vm ^. contract)
            _ -> error "underrun"

        0x00 {- STOP -} ->
          case vm ^. callstack of
            [] -> return $! vm' & done .~ True
            (frame:frames) -> do
              -- print $ frame ^. frameLinkage
              case frame ^. frameLinkage of
                CallLinkage _ 0 ->
                  return $! vm'
                    & callstack .~ frames
                    & pc        .~ (frame ^. framePc)
                    & stack     .~ (1 : (frame ^. frameStack))
                    & memory    .~ (frame ^. frameMemory)
                    & callvalue .~ (frame ^. frameCallvalue)
                    & calldata  .~ (frame ^. frameCalldata)
                    & caller    .~ (frame ^. frameCaller)
                    & contract  .~ (frame ^. frameContract)
                    & vmCode    .~ (vm ^?! env . contracts . ix (frame ^. frameContract) . code)

        0xf3 {- RETURN -} ->
          case stk of
            (xOffset:xSize:_) ->
              case vm ^. callstack of
                [] -> return $! vm' & done .~ True
                (frame:frames) ->
                  case frame ^. frameLinkage of
                    CreateLinkage ->
                      let created = vm ^?! env . contracts . ix (vm ^. contract)
                          contract' =
                            initialContract
                             (BS.pack [Map.findWithDefault 0 (xOffset + num i) mem | i <- [0..xSize-1]])
                             & storage .~ (created ^. storage)
                             & balance .~ (created ^. balance)
                      in return $! vm'
                        & callstack .~ frames
                        & pc        .~ (frame ^. framePc)
                        & stack     .~ vm ^. contract : (frame ^. frameStack)
                        & memory    .~ (frame ^. frameMemory)
                        & callvalue .~ (frame ^. frameCallvalue)
                        & calldata  .~ (frame ^. frameCalldata)
                        & caller    .~ (frame ^. frameCaller)
                        & contract  .~ (frame ^. frameContract)
                        & vmCode    .~ (vm ^?! env . contracts . ix (frame ^. frameContract) . code)
                        & env . contracts . at (vm ^. contract) .~ Just contract'

                    CallLinkage yOffset ySize ->
                      return $! vm'
                        & callstack .~ frames
                        & pc        .~ (frame ^. framePc)
                        & stack     .~ (1 : (frame ^. frameStack))
                        & memory    .~ (
                            let m = frame ^. frameMemory
                            in flip union m
                                 (fromList [(yOffset + i,
                                            (vm ^? memory . ix (num (xOffset + i))) ?: 0)
                                           | i <- [0..ySize-1]])
                          )
                        & callvalue .~ (frame ^. frameCallvalue)
                        & calldata  .~ (frame ^. frameCalldata)
                        & caller    .~ (frame ^. frameCaller)
                        & contract  .~ (frame ^. frameContract)
                        & vmCode    .~ (vm ^?! env . contracts . ix (frame ^. frameContract) . code)

            _ -> error "underrun"

        0xf0 {- CREATE -} -> do
          case stk of
            (xValue:xOffset:xSize:xs) ->
              let address' =
                    newContractAddress (vm' ^. contract) (c ^. nonce)
                  contract' =
                    initialContract $
                      BS.pack [Map.findWithDefault 0 (xOffset + i) mem | i <- [0..xSize-1]]
                  contracts' =
                    (vm ^. env . contracts) & at address' .~ (Just $! contract')
                                            & ix (vm ^. contract) . nonce +~ 1
              in return $! vm'
                & callstack %~ (Frame (vm' ^. contract)
                                      (vm' ^. pc)
                                      xs
                                      (vm' ^. memory)
                                      (vm' ^. calldata)
                                      (vm' ^. callvalue)
                                      (vm' ^. caller)
                                      CreateLinkage
                                      (0, Nothing) :)
                & pc        .~ 0
                & vmCode    .~ contract' ^. code
                & stack     .~ mempty
                & memory    .~ mempty
                & calldata  .~ mempty
                & callvalue .~ xValue
                & caller    .~ (vm ^. contract)
                & contract  .~ address'
                & env . contracts .~ contracts'
            _ -> error "underrun"

        0x39 {- CODECOPY -} ->
          case stk of
            (memoryOffset:codeOffset:codeSize:xs) -> return $!
              vm' & stack .~ xs
                  & memory .~ flip union (vm' ^. memory)
                      (fromList [(memoryOffset + i,
                                  (c ^? code . ix (num (codeOffset + i))) ?: 0)
                                 | i <- [0..codeSize-1]])
            _ -> error "underrun"

    else do
      cpprint ("pc" :: String, vm ^. pc)
      cpprint ("contract" :: String, vm ^. contract)
      error "error"

stackOp0_1 :: Monad m => VM -> Word256 -> m VM
stackOp0_1 vm !x =
  return $! vm & stack %~ (x :)

stackOp1_1 :: Monad m => VM -> (Word256 -> Word256) -> m VM
stackOp1_1 vm f =
  case vm ^. stack of
    (x:xs) ->
      let !y = f x in
      return $! vm & stack .~ y : xs
    _ -> error "underrun"

stackOp2_1 :: Monad m => VM -> (Word256 -> Word256 -> Word256) -> m VM
stackOp2_1 vm f =
  case vm ^. stack of
    (x:y:xs) ->
      let !z = f x y in
      return $! vm & stack .~ z : xs
    _ -> error "underrun"

exec :: VM -> IO VM
exec vm = if vm ^. done then return vm else exec1 vm >>= exec

continue :: ByteString -> Word256 -> Text -> Map Text SolcContract -> SourceCache -> VM -> VM
continue theCalldata theCallvalue theContractName theSolc theSourceCache vm =
  initialVm theCalldata theCallvalue theContractName theSolc theSourceCache
    & env .~ (vm ^. env)

checkJump :: (Monad m, Integral n) => VM -> n -> m VM
checkJump vm x =
  let theCode = vm ^. env . contracts . ix (vm ^. contract) . code in
  if num x < BS.length theCode && BS.index theCode (num x) == 0x5b
  then return $! vm & pc .~ num x
  else error "bad jump destination"

initialVm :: ByteString -> Word256 -> Text -> Map Text SolcContract -> SourceCache -> VM
initialVm theCalldata theCallvalue theContractName theSolc theSourceCache = VM {
  _done = False,
  _env = Env {
    _contracts = Map.fromList [
      (123, initialContract (theSolc ^?! ix theContractName . runtimeCode))
    ],
    _solc = Map.fromList [(x ^. solcCodehash,  x) | x <- Map.elems theSolc],
    _sha3Crack = mempty,
    _sourceCache = theSourceCache
  },
  _contract = 123,
  _pc = 0,
  _vmCode = theSolc ^?! ix theContractName . runtimeCode,
  _stack = mempty,
  _memory = mempty,
  _calldata = theCalldata,
  _callvalue = theCallvalue,
  _callstack = mempty,
  _caller = 0
}

run :: Text -> Text -> Text -> IO VM
run file contractName abi = do
  Just (c, cache) <- readSolc (Text.unpack file)
  exec (initialVm (word32Bytes $ abiKeccak (encodeUtf8 abi)) 0
    contractName c cache)

-- Copied from the standard library just to get specialization.
-- We also use bit operations instead of modulo and multiply.
-- (This operation was significantly slow.)
(^) :: Word256 -> Word256 -> Word256
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

word :: Integral a => [a] -> Word256
word xs = sum [ num x `shiftL` (8*n)
              | (n, x) <- zip [0..] (reverse xs) ]

word32 :: Integral a => [a] -> Word32
word32 xs = sum [ num x `shiftL` (8*n)
                | (n, x) <- zip [0..] (reverse xs) ]

wordAt :: Int -> ByteString -> Word256
wordAt i bs = word [(bs ^? ix j) ?: 0 | j <- [i..(i+31)]]

word256At :: Word256 -> Lens' (Map Word256 Word8) Word256
word256At i = lens getter setter where
  getter m =
    let
      go !a (-1) = a
      go !a !n = go (a + shiftL (num $ Map.findWithDefault 0 (i + num n) m)
                                (8 * (31 - n))) (n - 1)
    in {-# SCC word256At_getter #-}
      go (0 :: Word256) (31 :: Int)
  setter m x =
    {-# SCC word256At_setter #-}
    -- Optimizing this would help significantly.
    union (fromList [(i + 31 - j, byteAt x (num j)) | j <- [0..31]]) m

word32At :: Word256 -> Lens' (Map Word256 Word8) Word32
word32At i = lens getter setter where
  getter m =
    word32 [(m ^? ix (i + j)) ?: 0 | j <- [0..3]]
  setter m x =
    union (fromList [(i + 3 - num j, byteAt x j) | j <- [0..3]]) m

byteAt :: (Bits a, Bits b, Integral a, Num b) => a -> Int -> b
byteAt x j = num (x `shiftR` (j * 8)) .&. 0xff

word32Bytes :: Word32 -> ByteString
word32Bytes x = BS.pack [byteAt x (3 - i) | i <- [0..3]]

{- We will need to parse ops into data soon, so let's keep this code. -}

-- readOp :: Word8 -> Seq Word8 -> Op
-- readOp x _  | x >= 0x80 && x <= 0x8f = OpDup (x - 0x80 + 1)
-- readOp x _  | x >= 0x90 && x <= 0x9f = OpSwap (x - 0x90 + 1)
-- readOp x _  | x >= 0xa0 && x <= 0xa4 = OpLog (x - 0xa0 + 1)
-- readOp x xs | x >= 0x60 && x <= 0x7f =
--   let n   = x - 0x60 + 1
--       xs' = Seq.take (num n) xs
--       len = Seq.length xs'
--   in if len == fromIntegral n
--      then OpPush (toList xs')
--      else OpPush (toList xs' ++ replicate (len - num n) 0)
-- readOp x _ = case x of
--   0x00 -> OpStop
--   0x01 -> OpAdd
--   0x02 -> OpMul
--   0x03 -> OpSub
--   0x04 -> OpDiv
--   0x05 -> OpSdiv
--   0x06 -> OpMod
--   0x07 -> OpSmod
--   0x08 -> OpAddmod
--   0x09 -> OpMulmod
--   0x0a -> OpExp
--   0x0b -> OpSignextend
--   0x10 -> OpLt
--   0x11 -> OpGt
--   0x12 -> OpSlt
--   0x13 -> OpSgt
--   0x14 -> OpEq
--   0x15 -> OpIszero
--   0x16 -> OpAnd
--   0x17 -> OpOr
--   0x18 -> OpXor
--   0x19 -> OpNot
--   0x1a -> OpByte
--   0x20 -> OpSha3
--   0x30 -> OpAddress
--   0x31 -> OpBalance
--   0x32 -> OpOrigin
--   0x33 -> OpCaller
--   0x34 -> OpCallvalue
--   0x35 -> OpCalldataload
--   0x36 -> OpCalldatasize
--   0x37 -> OpCalldatacopy
--   0x38 -> OpCodesize
--   0x39 -> OpCodecopy
--   0x3a -> OpGasprice
--   0x3b -> OpExtcodesize
--   0x3c -> OpExtcodecopy
--   0x40 -> OpBlockhash
--   0x41 -> OpCoinbase
--   0x42 -> OpTimestamp
--   0x43 -> OpNumber
--   0x44 -> OpDifficulty
--   0x45 -> OpGaslimit
--   0x50 -> OpPop
--   0x51 -> OpMload
--   0x52 -> OpMstore
--   0x53 -> OpMstore8
--   0x54 -> OpSload
--   0x55 -> OpSstore
--   0x56 -> OpJump
--   0x57 -> OpJumpi
--   0x58 -> OpPc
--   0x59 -> OpMsize
--   0x5a -> OpGas
--   0x5b -> OpJumpdest
--   0xf0 -> OpCreate
--   0xf1 -> OpCall
--   0xf2 -> OpCallcode
--   0xf3 -> OpReturn
--   0xf4 -> OpDelegatecall
--   0xf5 -> OpSuicide
--   _    -> (OpUnknown x)

{-
  Unimplemented:
    addmod
    address
    balance
    blockhash
    byte
    callcode
    coinbase
    delegatecall
    difficulty
    gaslimit
    gasprice
    msize
    mulmod
    number
    origin
    sdiv
    signextend
    smod
    suicide
    xor
-}    
