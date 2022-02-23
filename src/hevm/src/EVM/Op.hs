module EVM.Op
  ( Op (..)
  , opString
  ) where

import Data.Word (Word8)
import Numeric (showHex)

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
  | OpShl
  | OpShr
  | OpSar
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
  | OpReturndatasize
  | OpReturndatacopy
  | OpExtcodehash
  | OpBlockhash
  | OpCoinbase
  | OpTimestamp
  | OpNumber
  | OpDifficulty
  | OpGaslimit
  | OpChainid
  | OpSelfbalance
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
  | OpStaticcall
  | OpCallcode
  | OpReturn
  | OpDelegatecall
  | OpCreate2
  | OpRevert
  | OpSelfdestruct
  | OpDup !Word8
  | OpSwap !Word8
  | OpLog !Word8
  | OpPush !Word8
  | OpUnknown Word8
  deriving (Show, Eq)

opString :: (Integral a, Show a) => (a, Op) -> String
opString (i, o) = let showPc x | x < 0x10 = '0' : showHex x ""
                               | otherwise = showHex x ""
                  in showPc i <> " " ++ case o of
  OpStop -> "STOP"
  OpAdd -> "ADD"
  OpMul -> "MUL"
  OpSub -> "SUB"
  OpDiv -> "DIV"
  OpSdiv -> "SDIV"
  OpMod -> "MOD"
  OpSmod -> "SMOD"
  OpAddmod -> "ADDMOD"
  OpMulmod -> "MULMOD"
  OpExp -> "EXP"
  OpSignextend -> "SIGNEXTEND"
  OpLt -> "LT"
  OpGt -> "GT"
  OpSlt -> "SLT"
  OpSgt -> "SGT"
  OpEq -> "EQ"
  OpIszero -> "ISZERO"
  OpAnd -> "AND"
  OpOr -> "OR"
  OpXor -> "XOR"
  OpNot -> "NOT"
  OpByte -> "BYTE"
  OpShl -> "SHL"
  OpShr -> "SHR"
  OpSar -> "SAR"
  OpSha3 -> "SHA3"
  OpAddress -> "ADDRESS"
  OpBalance -> "BALANCE"
  OpOrigin -> "ORIGIN"
  OpCaller -> "CALLER"
  OpCallvalue -> "CALLVALUE"
  OpCalldataload -> "CALLDATALOAD"
  OpCalldatasize -> "CALLDATASIZE"
  OpCalldatacopy -> "CALLDATACOPY"
  OpCodesize -> "CODESIZE"
  OpCodecopy -> "CODECOPY"
  OpGasprice -> "GASPRICE"
  OpExtcodesize -> "EXTCODESIZE"
  OpExtcodecopy -> "EXTCODECOPY"
  OpReturndatasize -> "RETURNDATASIZE"
  OpReturndatacopy -> "RETURNDATACOPY"
  OpExtcodehash -> "EXTCODEHASH"
  OpBlockhash -> "BLOCKHASH"
  OpCoinbase -> "COINBASE"
  OpTimestamp -> "TIMESTAMP"
  OpNumber -> "NUMBER"
  OpDifficulty -> "DIFFICULTY"
  OpGaslimit -> "GASLIMIT"
  OpChainid -> "CHAINID"
  OpSelfbalance -> "SELFBALANCE"
  OpPop -> "POP"
  OpMload -> "MLOAD"
  OpMstore -> "MSTORE"
  OpMstore8 -> "MSTORE8"
  OpSload -> "SLOAD"
  OpSstore -> "SSTORE"
  OpJump -> "JUMP"
  OpJumpi -> "JUMPI"
  OpPc -> "PC"
  OpMsize -> "MSIZE"
  OpGas -> "GAS"
  OpJumpdest -> "JUMPDEST"
  OpCreate -> "CREATE"
  OpCall -> "CALL"
  OpStaticcall -> "STATICCALL"
  OpCallcode -> "CALLCODE"
  OpReturn -> "RETURN"
  OpDelegatecall -> "DELEGATECALL"
  OpCreate2 -> "CREATE2"
  OpSelfdestruct -> "SELFDESTRUCT"
  OpDup x -> "DUP" ++ show x
  OpSwap x -> "SWAP" ++ show x
  OpLog x -> "LOG" ++ show x
  OpPush x -> "PUSH " ++ show x
  OpRevert -> "REVERT"
  OpUnknown x -> case x of
    254 -> "INVALID"
    _ -> "UNKNOWN " ++ (showHex x "")
