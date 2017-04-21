{-# Language BangPatterns #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
{-# Language Rank2Types #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}

module EVM where

import Prelude hiding ((^))

import EVM.Solidity
import EVM.Keccak

import Data.DoubleWord
import Data.DoubleWord.TH

import Data.Data
import GHC.Generics

import Control.Lens hiding (op, (:<), (|>))

import Data.Bits
import Data.Maybe
import Data.Word

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

mkUnpackedDoubleWord "Word512" ''Word256 "Int512" ''Int256 ''Word256
  [''Typeable, ''Data, ''Generic]

-- When invoking another contract, e.g., through CALL or CREATE, the
-- linkage is what we need to remember when we come back, aside from
-- all the VM state.  (The term `linkage' comes from SICP's
-- compiler chapter.)
data Linkage = CallLinkage Word256 Word256
             | CreateLinkage
               deriving Show

data FrameState = FrameState {
  _contract    :: !Word256,
  _vmCode      :: !ByteString,
  _pc          :: !Int,
  _stack       :: ![Word256],
  _memory      :: !(Map Word256 Word8),
  _calldata    :: !ByteString,
  _callvalue   :: !Word256,
  _caller      :: !Word256
} deriving Show

makeLenses ''FrameState

blankState :: FrameState
blankState = FrameState
  { _contract = 0
  , _vmCode = mempty
  , _pc = 0
  , _stack = mempty
  , _memory = mempty
  , _calldata = mempty
  , _callvalue = 0
  , _caller = 0
  }

data Frame = Frame {
  _frameState     :: !FrameState,
  _frameLinkage   :: !Linkage,
  _frameTrace     :: (Word256, Maybe Word32) {- (codehash, abi) -}
} deriving Show
makeLenses ''Frame

data Contract = Contract {
  _bytecode :: !ByteString,
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
  _sourceCache :: SourceCache,
  _origin      :: Word256
} deriving (Show)
makeLenses ''Env

data Block = Block {
  _coinbase :: !Word256,
  _timestamp :: !Word256,
  _number :: !Word256,
  _difficulty :: !Word256,
  _gaslimit :: !Word256
} deriving Show
makeLenses ''Block

data VM = VM {
  _done        :: !Bool,
  _env         :: !Env,
  _block       :: !Block,
  _frames      :: ![Frame],
  _suicides    :: ![Word256],
  _memorySize  :: !Word256,
  _state       :: !FrameState
} deriving Show
makeLenses ''VM

initialContract :: ByteString -> Contract
initialContract theCode = Contract {
  _bytecode = theCode,
  _codesize = BS.length theCode,
  _codehash = keccak theCode,
  _storage  = mempty,
  _balance  = 0,
  _nonce    = 0,
  _opIxMap  = mkOpIxMap theCode
}

ceilDiv :: Integral a => a -> a -> a
ceilDiv a b =
  let (q, r) = quotRem a b
  in q + if r /= 0 then 1 else 0

accessMemoryRange :: Word256 -> Word256 -> VM -> VM
accessMemoryRange _ 0 vm = vm
accessMemoryRange f l vm =
  vm & memorySize .~ max (vm ^. memorySize) (ceilDiv (f + l) 32)

accessMemoryWord :: Word256 -> VM -> VM
accessMemoryWord x = accessMemoryRange x 32



exec1 :: VM -> IO VM
exec1 vm = do
  let Just !c = vm ^? env . contracts . ix (vm ^. state . contract)
  if vm ^. state . pc < BS.length (vm ^. state . vmCode)
    then
      let vm' = vm & state . pc +~ opSize op
          op  = BS.index (vm ^. state . vmCode) (vm ^. state .pc)
          mem = vm ^. state . memory
          stk = vm ^. state . stack
      in case op of

        x {- PUSH -} | x >= 0x60 && x <= 0x7f ->
          let !n = num x - 0x60 + 1
              !xs = BS.take n (BS.drop (1 + vm ^. state . pc)
                                       (vm ^. state . vmCode))
          in stackOp0_1 vm' (word (BS.unpack xs))

        0x15 {- ISZERO -} ->
          stackOp1_1 vm' (\case 0 -> 1; _ -> 0)

        0x57 {- JUMPI -} -> do
          case stk of
            (x:y:xs) ->
              if y == 0
              then return $! vm' & state . stack .~ xs
              else checkJump (vm & state . stack .~ xs) x
            _ -> error "underrun"

        0x56 {- JUMP -} ->
          case stk of
            (x:xs) -> checkJump (vm & state . stack .~ xs) x
            _ -> error "underrun"

        x | x >= 0x90 && x <= 0x9f -> {- SWAP -}
          let !i = x - 0x90 + 1 in
          if length stk < num i
          then error "underrun"
          else return $!
            vm' & state . stack . ix 0 .~
                    (vm ^?! state . stack . ix (num i))
                & state . stack . ix (num i) .~
                    (vm ^?! state . stack . ix 0)

        x | x >= 0x80 && x <= 0x8f -> {- DUP -}
          let !i = x - 0x80 + 1 in
          if length stk < num i
          then error "underrun"
          else
            let !y = vm ^?! state . stack . ix (num i - 1)
            in return $!
                 vm' & state . stack %~ (y:)

        0x50 {- POP -} ->
          case stk of
            (_:xs) -> return $! vm' & state . stack .~ xs
            _ -> error "underrun"

        0x52 {- MSTORE -} ->
          case stk of
            (x:y:xs) ->
              return $!
                vm' & state . memory . word256At x .~ y
                    & state . stack .~ xs
                    & accessMemoryWord x
            _ -> error "underrun"

        0x53 {- MSTORE8 -} ->
          case stk of
            (x:y:xs) ->
              return $!
                vm' & state . memory . at x .~ Just (num (y .&. 0xff))
                    & state . stack .~ xs
                    & accessMemoryRange x 1
            _ -> error "underrun"

        0x55 {- SSTORE -} -> do
          case stk of
            (x:y:xs) -> do
              return $!
                vm' & env . contracts . ix (vm ^. state . contract)
                          . storage . at x .~ Just y
                    & state . stack .~ xs
            _ -> error "underrun"

        0x58 {- PC -} ->
          stackOp0_1 vm' (num (vm ^. state . pc))

        0x59 {- MSIZE -} ->
          stackOp0_1 vm' (vm ^. memorySize)

        0x04 {- DIV -} ->
          stackOp2_1 vm' $ \x ->
            \case
              0 -> 0
              y -> div x y

        0x05 {- SDIV -} ->
          stackOp2_1 vm' $ \x ->
            \case
              0 -> 0
              y -> unsignedWord (div (signedWord x) (signedWord y))

        0x06 {- MOD -} ->
          stackOp2_1 vm' $ \x ->
            \case
              0 -> 0
              y -> mod x y

        0x07 {- SMOD -} ->
          stackOp2_1 vm' $ \x ->
            \case
              0 -> 0
              y -> unsignedWord (mod (signedWord x) (signedWord y))

        0x16 {- AND -} ->
          stackOp2_1 vm' (.&.)

        0x17 {- OR -} ->
          stackOp2_1 vm' (.|.)

        0x18 {- XOR -} ->
          stackOp2_1 vm' xor

        0x19 {- NOT -} ->
          stackOp1_1 vm' complement

        0x1a {- BYTE -} ->
          stackOp2_1 vm' $ \n x ->
            if n >= 32 then 0
            else (shiftR x (8 * (31 - num n))) .&. 0xff

        0x02 {- MUL -} ->
          stackOp2_1 vm' (*)

        0x0a {- EXP -} ->
          stackOp2_1 vm' (^)

        0x0b {- SIGNEXTEND -} ->
          stackOp2_1 vm' $ \bytes x ->
            if bytes >= 32 then x
            else let n = num bytes * 8 + 7 in
              if testBit x n
              then x .|. complement (bit n - 1)
              else x .&. (bit n - 1)

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
          return $! vm' & state . stack %~ drop (num n + 1)

        0x36 {- CALLDATASIZE -} ->
          stackOp0_1 vm' (num (BS.length (vm ^. state . calldata)))

        0x35 {- CALLDATALOAD -} ->
          stackOp1_1 vm'
            (\x -> wordAt (num x) (vm ^. state . calldata))

        0x37 {- CALLDATACOPY -} ->
          case stk of
            (toOffset:fromOffset:theSize:xs) ->
              return $!
                vm' & state . stack .~ xs
                    & state . memory .~ flip union mem
                        (fromList [
                            (toOffset + i,
                             (vm ^? state . calldata
                                          . ix (num (fromOffset + i)))
                               ?: 0)
                            | i <- [0..theSize-1]])
            _ -> error "underrun"

        0x20 {- SHA3 -} ->
          case stk of
            (xOffset:xSize:xs) ->
              let m = vm' ^. state . memory
                  bytes = [(Map.lookup (xOffset + i) m) ?: 0
                           | i <- [0..xSize-1]]
                  hash = keccak (BS.pack bytes)
              in do
                -- cpprint ("SHA3 preimage", bytes)
                return $! vm'
                  & state . stack .~ hash : xs
                  & env . sha3Crack . at hash .~ Just bytes
                  & accessMemoryRange xOffset xSize
            _ -> error "underrun"

        0x30 {- ADDRESS -} ->
          stackOp0_1 vm' (vm ^. state . contract)

        0x34 {- CALLVALUE -} ->
          stackOp0_1 vm' (vm ^. state . callvalue)

        0x33 {- CALLER -} ->
          stackOp0_1 vm' (vm ^. state . caller)

        0x31 {- BALANCE -} ->
          stackOp1_1 vm' $ \x ->
            (vm ^? env . contracts . ix x . balance) ?: 0

        0x32 {- ORIGIN -} ->
          stackOp0_1 vm' (vm ^. env . origin)

        0x51 {- MLOAD -} ->
          case vm ^. state . stack of
            (x:xs) -> return $!
              vm & state . stack .~ (vm ^. state . memory . word256At x : xs)
                 & accessMemoryWord x
            _ -> error "underrun"

        0x54 {- SLOAD -} -> do
          stackOp1_1 vm' (\x -> (c ^. storage . at x) ?: 0)

        0x3b {- EXTCODESIZE -} ->
          stackOp1_1 vm'
            (\x -> num ((vm ^? env . contracts . ix x . codesize) ?: 0))

        0x40 {- BLOCKHASH -} ->
          -- fake zero block hashes everywhere
          stackOp1_1 vm' (const 0)

        0x41 {- COINBASE -}   -> stackOp0_1 vm' (vm ^. block . coinbase)
        0x42 {- TIMESTAMP -}  -> stackOp0_1 vm' (vm ^. block . timestamp)
        0x43 {- NUMBER -}     -> stackOp0_1 vm' (vm ^. block . number)
        0x44 {- DIFFICULTY -} -> stackOp0_1 vm' (vm ^. block . difficulty)
        0x45 {- GASLIMIT -}   -> stackOp0_1 vm' (vm ^. block . gaslimit)
        0x5a {- GAS -}        -> stackOp0_1 vm' 0xffffffffffffffffff

        0xf1 {- CALL -} ->
          case stk of
            (_:xTo:xValue:xInOffset:xInSize:xOutOffset:xOutSize:xs) -> do
              let frame' =
                    Frame
                      ((vm' ^. state) & stack .~ xs)
                      (CallLinkage xOutOffset xOutSize)
                      (vm ^?! env . contracts . ix xTo . codehash,
                       if xInSize >= 4
                         then Just $!
                                vm ^. state . memory . word32At xInOffset
                         else Nothing)
                      
                  state' = (vm' ^. state)
                    & pc .~ 0
                    & stack .~ mempty
                    & memory .~ mempty
                    & contract .~ xTo
                    & calldata .~
                        (let m = vm' ^. state . memory in BS.pack [
                            (Map.lookup (xInOffset + i) m) ?: 0
                            | i <- [0..xInSize-1]
                         ])
                    & callvalue .~ xValue
                    & caller    .~ (vm ^. state . contract)
                    
              return $! vm'
                & frames %~ (frame' :)
                & state .~ state'
                & accessMemoryRange xInOffset xInSize
                & accessMemoryRange xOutOffset xOutSize
    
            _ -> error "underrun"

        0xf2 {- CALLCODE -} ->
          error "CALLCODE not supported (use DELEGATECALL)"

        0x00 {- STOP -} ->
          case vm ^. frames of
            [] -> return $! vm' & done .~ True
            (f:fs) -> do
              return $! vm'
                & frames .~ fs
                & state .~ f ^. frameState
                & state . stack %~ (1 :)

        0xff {- SUICIDE -} ->
          case vm ^. state . stack of
            [] -> error "underrun"
            (x:_) ->
              let self = vm ^. state . contract
                  vm'' = vm' & suicides %~ (self :)
                             & env . contracts . ix x . balance +~
                                 (vm ^?! env . contracts . ix self . balance)
                             & env . contracts . ix self . balance .~ 0
              in returnOp 0 (0, 0) vm''

        0xf3 {- RETURN -} ->
          case stk of
            (xOffset:xSize:_) ->
              case vm ^. frames of
                [] -> return $! vm' & done .~ True
                (f:fs) ->
                  case f ^. frameLinkage of
                    CreateLinkage ->
                      let created =
                            vm ^?! env . contracts . ix (vm ^. state . contract)
                          contract' =
                            initialContract
                             (BS.pack
                              [ Map.findWithDefault 0 (xOffset + num i) mem
                                | i <- [0..xSize-1]
                              ])
                             & storage .~ (created ^. storage)
                             & balance .~ (created ^. balance)
                      in return $! vm'
                        & frames .~ fs
                        & state .~ f ^. frameState
                        & state . stack %~ (vm ^. state . contract :)
                        & accessMemoryRange xOffset xSize
                        & env . contracts . at (vm ^. state . contract) .~ Just contract'

                    CallLinkage yOffset ySize ->
                      return $! vm'
                        & frames .~ fs
                        & state .~ f ^. frameState
                        & state . stack %~ (1 :)
                        & state . memory    .~ (
                            let m = f ^. frameState . memory
                            in flip union m
                                 (fromList [(yOffset + i,
                                            (vm ^? state . memory . ix (num (xOffset + i))) ?: 0)
                                           | i <- [0..ySize-1]])
                          )
                        & accessMemoryRange xOffset xSize

            _ -> error "underrun"

        0xf0 {- CREATE -} -> do
          case stk of
            (xValue:xOffset:xSize:xs) ->
              let address' =
                    newContractAddress (vm' ^. state . contract) (c ^. nonce)
                  xCode = BS.pack [
                    Map.findWithDefault 0 (xOffset + i) mem
                      | i <- [0..xSize-1]
                    ]
                  contract' = initialContract mempty
                  contracts' =
                    (vm ^. env . contracts) & at address' .~ (Just $! contract')
                                            & ix (vm ^. state . contract) . nonce +~ 1
              in return $! vm'
                & accessMemoryRange xOffset xSize
                & env . contracts .~ contracts'
                & frames %~
                    (Frame ((vm' ^. state) & stack .~ xs)
                           CreateLinkage
                           (0, Nothing) :)
                & state .~
                    (blankState
                      & vmCode    .~ xCode
                      & callvalue .~ xValue
                      & caller    .~ (vm ^. state . contract)
                      & contract  .~ address')
            _ -> error "underrun"

        0x38 {- CODESIZE -} ->
          stackOp0_1 vm' (fromIntegral (BS.length (vm ^. state . vmCode)))

        0x39 {- CODECOPY -} ->
          case stk of
            (memOffset:codeOffset:codeSize:xs) -> return $!
              vm' & state . stack .~ xs
                  & state . memory .~ flip union mem
                      (fromList [(memOffset + i,
                                  (c ^? bytecode . ix (num (codeOffset + i))) ?: 0)
                                 | i <- [0..codeSize-1]])
            _ -> error "underrun"

        0x08 {- ADDMOD -} ->
          stackOp3_1 vm'
            (\x y z -> fromWord512
              ((toWord512 x + toWord512 y) `mod` (toWord512 z)))

        0x09 {- MULMOD -} ->
          stackOp3_1 vm'
            (\x y z -> fromWord512
              ((toWord512 x * toWord512 y) `mod` (toWord512 z)))

        x -> do
          error ("opcode " ++ show x)

    else do
      cpprint ("pc" :: String, vm ^. state . pc)
      cpprint ("contract" :: String, vm ^. state . contract)
      error "error"

returnOp :: Word256 -> (Word256, Word256) -> VM -> IO VM
returnOp returnCode (xOffset, xSize) vm =
  case vm ^. frames of
    [] -> return $! vm & done .~ True
    (f:fs) ->
      case f ^. frameLinkage of
        CreateLinkage ->
          let created = vm ^?! env . contracts . ix (vm ^. state . contract)
              contract' =
                initialContract
                 (BS.pack
                   [ Map.findWithDefault 0 (xOffset + num i)
                       (vm ^. state . memory)
                     | i <- [0..xSize-1]])
                 & storage .~ (created ^. storage)
                 & balance .~ (created ^. balance)
          in return $! vm
            & accessMemoryRange xOffset xSize
            & env . contracts . at (vm ^. state . contract) .~ Just contract'
            & frames .~ fs
            & state .~ (f ^. frameState)
            & state . stack %~ (vm ^. state . contract :)

        CallLinkage yOffset ySize ->
          return $! vm
            & accessMemoryRange xOffset xSize
            & frames .~ fs
            & state .~ f ^. frameState
            & state . stack %~ (returnCode :)
            & state . memory    .~ (
                let m = f ^. frameState . memory
                in flip union m
                     (fromList [(yOffset + i,
                                (vm ^? state . memory . ix (num (xOffset + i))) ?: 0)
                               | i <- [0..ySize-1]])
              )

toWord512 :: Word256 -> Word512
toWord512 x = fromHiAndLo 0 x

fromWord512 :: Word512 -> Word256
fromWord512 x = loWord x

stackOp0_1 :: Monad m => VM -> Word256 -> m VM
stackOp0_1 vm !x =
  return $! vm & state . stack %~ (x :)

stackOp1_1 :: Monad m => VM -> (Word256 -> Word256) -> m VM
stackOp1_1 vm f =
  case vm ^. state . stack of
    (x:xs) ->
      let !y = f x in
      return $! vm & state . stack .~ y : xs
    _ -> error "underrun"

stackOp2_1 :: Monad m => VM -> (Word256 -> Word256 -> Word256) -> m VM
stackOp2_1 vm f =
  case vm ^. state . stack of
    (x:y:xs) ->
      let !z = f x y in
      return $! vm & state . stack .~ z : xs
    _ -> error "underrun"

stackOp3_1 :: Monad m => VM
  -> (Word256 -> Word256 -> Word256 -> Word256)
  -> m VM
stackOp3_1 vm f =
  case vm ^. state . stack of
    (x:y:z:xs) ->
      let !a = f x y z in
      return $! vm & state . stack .~ a : xs
    _ -> error "underrun"

exec :: VM -> IO VM
exec vm = if vm ^. done then return vm else exec1 vm >>= exec

continue :: ByteString -> Word256 -> Text -> Map Text SolcContract -> SourceCache -> Block -> Word256 -> VM -> VM
continue theCalldata theCallvalue theContractName theSolc theSourceCache theBlock theOrigin vm =
  initialVm theCalldata theCallvalue theContractName theSolc theSourceCache theBlock theOrigin
    & env .~ (vm ^. env)

checkJump :: (Monad m, Integral n) => VM -> n -> m VM
checkJump vm x =
  let theCode = vm ^. state . vmCode in
  if num x < BS.length theCode && BS.index theCode (num x) == 0x5b
  then return $! vm & state . pc .~ num x
  else error "bad jump destination"

initialVm :: ByteString -> Word256 -> Text -> Map Text SolcContract -> SourceCache -> Block -> Word256 -> VM
initialVm theCalldata theCallvalue theContractName theSolc theSourceCache theBlock theOrigin = VM {
  _done = False,
  _env = Env {
    _contracts = Map.fromList [
      (123, initialContract (theSolc ^?! ix theContractName . runtimeCode))
    ],
    _solc = Map.fromList [(x ^. solcCodehash,  x) | x <- Map.elems theSolc],
    _sha3Crack = mempty,
    _sourceCache = theSourceCache,
    _origin = theOrigin
  },
  _block = theBlock,
  _frames = mempty,
  _suicides = mempty,
  _memorySize = 0,
  _state = FrameState {
    _contract = 123,
    _pc = 0,
    _vmCode = theSolc ^?! ix theContractName . runtimeCode,
    _stack = mempty,
    _memory = mempty,
    _calldata = theCalldata,
    _callvalue = theCallvalue,
    _caller = 0
  }
}

run :: Text -> Text -> Text -> IO VM
run file contractName abi = do
  Just (c, cache) <- readSolc (Text.unpack file)
  let theBlock = Block 0 0 0 0 0
  exec (initialVm (word32Bytes $ abiKeccak (encodeUtf8 abi)) 0
    contractName c cache theBlock 0)

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
    callcode
    delegatecall
-}
