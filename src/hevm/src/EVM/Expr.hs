{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
  Expr implements an abstract respresentation of an EVM program

  Memory and storage are both represented as a sequence of sequenced writes on top of some base state.
  In the case of Memory that base state is always empty, but in the case of Storage it can be either empty (for init code) or abstract, for runtime code.

  Calldata is immutable, so we can simply represent it as an slice without needing to keep track of which state is being read from
  Returndata is represented as a slice of a particular memory expression, allowing returndata in the current call frame to reference memory from the sub call frame.

  TODO: probably we need some capture avoidance / freshness stuff for memory / calldata? Maybe debruijn indicies for call frames?
-}
module Expr where

import Data.DoubleWord (Word256)

-- phantom type tags for AST construction
data EType
  = Memory
  | Storage
  | Returndata
  | Logs
  | W256
  | End

data Expr (a :: EType) where

  -- identifiers
  Lit     :: Word256 -> Expr W256
  Var     :: String  -> Expr W256

  -- control flow
  Invalid :: Expr End
  Revert  :: String          -> Expr End
  Stop    :: Expr Storage    -> Expr End
  Return  :: Expr Returndata -> Expr Storage -> Expr End
  ITE     :: Expr W256       -> Expr W256    -> Expr W256 -> Expr W256

  -- integers
  Add    :: Expr W256 -> Expr W256 -> Expr W256
  Sub    :: Expr W256 -> Expr W256 -> Expr W256
  Mul    :: Expr W256 -> Expr W256 -> Expr W256
  Div    :: Expr W256 -> Expr W256 -> Expr W256
  SDiv   :: Expr W256 -> Expr W256 -> Expr W256
  Mod    :: Expr W256 -> Expr W256 -> Expr W256
  SMod   :: Expr W256 -> Expr W256 -> Expr W256
  AddMod :: Expr W256 -> Expr W256 -> Expr W256
  MulMod :: Expr W256 -> Expr W256 -> Expr W256
  Exp    :: Expr W256 -> Expr W256 -> Expr W256
  Sex    :: Expr W256 -> Expr W256

  -- booleans
  LT     :: Expr W256 -> Expr W256 -> Expr W256
  GT     :: Expr W256 -> Expr W256 -> Expr W256
  SLT    :: Expr W256 -> Expr W256 -> Expr W256
  SGT    :: Expr W256 -> Expr W256 -> Expr W256
  Eq     :: Expr W256 -> Expr W256 -> Expr W256
  IsZero :: Expr W256 -> Expr W256 -> Expr W256

  -- bits
  And    :: Expr W256 -> Expr W256 -> Expr W256
  Or     :: Expr W256 -> Expr W256 -> Expr W256
  Xor    :: Expr W256 -> Expr W256 -> Expr W256
  Not    :: Expr W256 -> Expr W256
  SHL    :: Expr W256 -> Expr W256 -> Expr W256
  SHR    :: Expr W256 -> Expr W256 -> Expr W256
  SAR    :: Expr W256 -> Expr W256 -> Expr W256

  -- keccack
  Sha3   :: Expr W256 -> Expr W256 -> Expr Memory -> Expr W256

  -- context
  Address     :: Expr W256
  Balance     :: Expr W256
  Origin      :: Expr W256
  Caller      :: Expr W256
  CallValue   :: Expr W256
  BlockHash   :: Expr W256
  Coinbase    :: Expr W256
  Timestamp   :: Expr W256
  Number      :: Expr W256
  Difficulty  :: Expr W256
  GasLimit    :: Expr W256
  ChainId     :: Expr W256
  SelfBalance :: Expr W256
  BaseFee     :: Expr W256
  Pc          :: Expr W256
  Gas         :: Expr W256

  -- calldata
  CalldataLoad  :: Expr W256 -> Expr W256
  CalldataSize  :: Expr W256
  CalldataCopy  :: Expr W256    -- dst offset
                -> Expr W256    -- src offset
                -> Expr W256    -- size
                -> Expr Memory  -- old memory
                -> Expr Memory  -- new memory
  -- code
  CodeSize      :: Expr W256
  ExtCodeHash   :: Expr W256

  CodeCopy      :: Expr W256   -- dst offset
                -> Expr W256   -- src offset
                -> Expr W256   -- size
                -> Expr Memory -- old memory
                -> Expr Memory -- new memory

  ExtCodeCopy   :: Expr W256   -- address
                -> Expr W256   -- dst offset
                -> Expr W256   -- src offset
                -> Expr W256   -- size
                -> Expr Memory -- old memory
                -> Expr Memory -- new memory

  -- returndata
  ReturndataSize :: Expr W256
  ReturndataCopy :: Expr W256       -- dst offset
                 -> Expr W256       -- src offset
                 -> Expr W256       -- size
                 -> Expr Returndata -- returndata
                 -> Expr Memory     -- old mem
                 -> Expr Memory     -- new mem

  EmptyRet :: Expr Returndata
  WriteRet :: Expr W256       -- offset
           -> Expr W256       -- size
           -> Expr Memory     -- memory
           -> Expr Returndata -- new returndata

  -- logs
  EmptyLog :: Expr Logs
  Log0 :: Expr W256 -> Expr W256 -> Expr W256 -> Expr Logs -> Expr Logs
  Log1 :: Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr Logs -> Expr Logs
  Log2 :: Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr Logs -> Expr Logs
  Log3 :: Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr Logs -> Expr Logs
  Log4 :: Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr W256 -> Expr Logs -> Expr Logs

  -- Contract Creation
  Create   :: Expr W256    -- value
           -> Expr W256    -- offset
           -> Expr W256    -- size
           -> Expr Memory  -- memory
           -> Expr Logs    -- old logs
           -> Expr Logs    -- new logs
           -> Expr Storage -- old storage
           -> Expr Storage -- new storage
           -> Expr W256

  Create2  :: Expr W256    -- value
           -> Expr W256    -- offset
           -> Expr W256    -- size
           -> Expr W256    -- salt
           -> Expr Memory  -- memory
           -> Expr Logs    -- old logs
           -> Expr Logs    -- new logs
           -> Expr Storage -- old storage
           -> Expr Storage -- new storage
           -> Expr W256    -- address

  -- Calls
  Call          :: Expr W256       -- gas
                -> Expr W256       -- address
                -> Expr W256       -- value
                -> Expr W256       -- args offset
                -> Expr W256       -- args size
                -> Expr W256       -- ret offset
                -> Expr W256       -- ret size
                -> Expr Logs       -- old logs
                -> Expr Logs       -- new logs
                -> Expr Storage    -- old storage
                -> Expr Storage    -- new storage
                -> Expr Returndata -- return data
                -> Expr W256       -- success

  CallCode      :: Expr W256       -- gas
                -> Expr W256       -- address
                -> Expr W256       -- value
                -> Expr W256       -- args offset
                -> Expr W256       -- args size
                -> Expr W256       -- ret offset
                -> Expr W256       -- ret size
                -> Expr Logs       -- old logs
                -> Expr Logs       -- new logs
                -> Expr Storage    -- old storage
                -> Expr Storage    -- new storage
                -> Expr Returndata -- return data
                -> Expr W256       -- success

  DelegeateCall :: Expr W256       -- gas
                -> Expr W256       -- address
                -> Expr W256       -- value
                -> Expr W256       -- args offset
                -> Expr W256       -- args size
                -> Expr W256       -- ret offset
                -> Expr W256       -- ret size
                -> Expr Logs       -- old logs
                -> Expr Logs       -- new logs
                -> Expr Storage    -- old storage
                -> Expr Storage    -- new storage
                -> Expr Returndata -- return data
                -> Expr W256       -- success

  -- memory
  MLoad    :: Expr W256   -- index
           -> Expr Memory -- memory
           -> Expr W256   -- result

  MStore   :: Expr W256   -- dst offset
           -> Expr W256   -- value
           -> Expr Memory -- prev memory
           -> Expr Memory -- new memory

  MStore8  :: Expr W256   -- dst offset
           -> Expr W256   -- value
           -> Expr Memory -- prev memory
           -> Expr Memory -- new memory

  EmptyMem :: Expr Memory
  MSize    :: Expr W256

  -- storage
  SLoad    :: Expr W256    -- address
           -> Expr W256    -- index
           -> Expr Storage -- storage
           -> Expr W256    -- result

  SStore   :: Expr W256    -- address
           -> Expr W256    -- index
           -> Expr W256    -- value
           -> Expr Storage -- old storage
           -> Expr Storage -- new storae

  EmptyStore    :: Expr Storage
  AbstractStore :: Expr Storage

deriving instance Show (Expr a)
