{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
  Expr implements an abstract respresentation of an EVM program

  Memory and storage are both represented as a sequence of sequenced writes on top of some base state.
  In the case of Memory that base state is always empty, but in the case of Storage it can be either empty (for init code) or abstract, for runtime code.

  Calldata is immutable, so we can simply represent it as an slice from the calldata buffer in a particular frame
  Returndata is represented as a slice of a particular memory expression, allowing returndata in the current call frame to reference memory from the sub call frame.

  TODO: how do we compose the subcall into the new storage / logs?
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
  Lit            :: Word256 -> Expr W256
  Var            :: String  -> Expr W256

  -- control flow
  Invalid        :: Expr End
  Revert         :: String          -> Expr End
  Stop           :: Expr Storage    -> Expr End
  Return         :: Expr Returndata -> Expr Storage -> Expr End
  ITE            :: Expr W256       -> Expr W256    -> Expr W256 -> Expr W256

  -- integers
  Add            :: Expr W256 -> Expr W256 -> Expr W256
  Sub            :: Expr W256 -> Expr W256 -> Expr W256
  Mul            :: Expr W256 -> Expr W256 -> Expr W256
  Div            :: Expr W256 -> Expr W256 -> Expr W256
  SDiv           :: Expr W256 -> Expr W256 -> Expr W256
  Mod            :: Expr W256 -> Expr W256 -> Expr W256
  SMod           :: Expr W256 -> Expr W256 -> Expr W256
  AddMod         :: Expr W256 -> Expr W256 -> Expr W256
  MulMod         :: Expr W256 -> Expr W256 -> Expr W256
  Exp            :: Expr W256 -> Expr W256 -> Expr W256
  Sex            :: Expr W256 -> Expr W256

  -- booleans
  LT             :: Expr W256 -> Expr W256 -> Expr W256
  GT             :: Expr W256 -> Expr W256 -> Expr W256
  SLT            :: Expr W256 -> Expr W256 -> Expr W256
  SGT            :: Expr W256 -> Expr W256 -> Expr W256
  Eq             :: Expr W256 -> Expr W256 -> Expr W256
  IsZero         :: Expr W256 -> Expr W256 -> Expr W256

  -- bits
  And            :: Expr W256 -> Expr W256 -> Expr W256
  Or             :: Expr W256 -> Expr W256 -> Expr W256
  Xor            :: Expr W256 -> Expr W256 -> Expr W256
  Not            :: Expr W256 -> Expr W256
  SHL            :: Expr W256 -> Expr W256 -> Expr W256
  SHR            :: Expr W256 -> Expr W256 -> Expr W256
  SAR            :: Expr W256 -> Expr W256 -> Expr W256

  -- keccak
  Keccak         :: Expr W256   -- offset
                 -> Expr W256   -- size
                 -> Expr Memory -- memory
                 -> Expr W256   -- result

  -- block context
  Origin         :: Expr W256
  BlockHash      :: Expr W256
  Coinbase       :: Expr W256
  Timestamp      :: Expr W256
  Number         :: Expr W256
  Difficulty     :: Expr W256
  GasLimit       :: Expr W256
  ChainId        :: Expr W256
  BaseFee        :: Expr W256

  -- frame context
  CallValue      :: Int          -- frame idx
                 -> Expr W256

  Caller         :: Int          -- frame idx
                 -> Expr W256

  Address        :: Int          -- frame idx
                 -> Expr W256

  Balance        :: Int          -- frame idx
                 -> Int          -- PC (in case we're checking the current contract)
                 -> Expr W256    -- address
                 -> Expr W256

  SelfBalance    :: Int          -- frame idx
                 -> Int          -- PC
                 -> Expr W256

  Gas            :: Int          -- frame idx
                 -> Int          -- PC
                 -> Expr W256

  -- calldata
  CalldataSize   :: Expr W256

  CalldataLoad   :: Int          -- frame idx
                 -> Expr W256    -- data idx
                 -> Expr W256    -- result

  CalldataCopy   :: Int          -- frame idx
                 -> Expr W256    -- dst offset
                 -> Expr W256    -- src offset
                 -> Expr W256    -- size
                 -> Expr Memory  -- old memory
                 -> Expr Memory  -- new memory
  -- code
  CodeSize       :: Expr W256
  ExtCodeHash    :: Expr W256

  CodeCopy       :: Expr W256   -- dst offset
                 -> Expr W256   -- src offset
                 -> Expr W256   -- size
                 -> Expr Memory -- old memory
                 -> Expr Memory -- new memory

  ExtCodeCopy    :: Expr W256   -- address
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

  EmptyRet       :: Expr Returndata
  WriteRet       :: Expr W256       -- offset
                 -> Expr W256       -- size
                 -> Expr Memory     -- memory
                 -> Expr Returndata -- new returndata

  -- logs
  EmptyLog       :: Expr Logs

  Log0           :: Expr W256   -- offset
                 -> Expr W256   -- size
                 -> Expr Memory -- memory
                 -> Expr Logs   -- old logs
                 -> Expr Logs   -- new logs

  Log1           :: Expr W256   -- offset
                 -> Expr W256   -- size
                 -> Expr W256   -- topic
                 -> Expr Memory -- memory
                 -> Expr Logs   -- old logs
                 -> Expr Logs   -- new logs

  Log2           :: Expr W256   -- offset
                 -> Expr W256   -- size
                 -> Expr W256   -- topic 1
                 -> Expr W256   -- topic 2
                 -> Expr Memory -- memory
                 -> Expr Logs   -- old logs
                 -> Expr Logs   -- new logs

  Log3           :: Expr W256   -- offset
                 -> Expr W256   -- size
                 -> Expr W256   -- topic 1
                 -> Expr W256   -- topic 2
                 -> Expr W256   -- topic 3
                 -> Expr Memory -- memory
                 -> Expr Logs   -- old logs
                 -> Expr Logs   -- new logs

  Log4           :: Expr W256   -- offset
                 -> Expr W256   -- size
                 -> Expr W256   -- topic 1
                 -> Expr W256   -- topic 2
                 -> Expr W256   -- topic 3
                 -> Expr W256   -- topic 4
                 -> Expr Memory -- memory
                 -> Expr Logs   -- old logs
                 -> Expr Logs   -- new logs

  -- Contract Creation
  Create         :: Expr W256    -- value
                 -> Expr W256    -- offset
                 -> Expr W256    -- size
                 -> Expr Memory  -- memory
                 -> Expr Logs    -- logs
                 -> Expr Storage -- storage
                 -> Expr W256    -- address

  Create2        :: Expr W256    -- value
                 -> Expr W256    -- offset
                 -> Expr W256    -- size
                 -> Expr W256    -- salt
                 -> Expr Memory  -- memory
                 -> Expr Logs    -- logs
                 -> Expr Storage -- storage
                 -> Expr W256    -- address

  -- Calls
  Call           :: Expr W256         -- gas
                 -> Maybe (Expr W256) -- target
                 -> Expr W256         -- value
                 -> Expr W256         -- args offset
                 -> Expr W256         -- args size
                 -> Expr W256         -- ret offset
                 -> Expr W256         -- ret size
                 -> Expr Logs         -- logs
                 -> Expr Storage      -- storage
                 -> Expr W256         -- success

  CallCode       :: Expr W256       -- gas
                 -> Expr W256       -- address
                 -> Expr W256       -- value
                 -> Expr W256       -- args offset
                 -> Expr W256       -- args size
                 -> Expr W256       -- ret offset
                 -> Expr W256       -- ret size
                 -> Expr Logs       -- logs
                 -> Expr Storage    -- storage
                 -> Expr W256       -- success

  DelegeateCall  :: Expr W256       -- gas
                 -> Expr W256       -- address
                 -> Expr W256       -- value
                 -> Expr W256       -- args offset
                 -> Expr W256       -- args size
                 -> Expr W256       -- ret offset
                 -> Expr W256       -- ret size
                 -> Expr Logs       -- logs
                 -> Expr Storage    -- storage
                 -> Expr W256       -- success

  -- memory
  MLoad          :: Expr W256   -- index
                 -> Expr Memory -- memory
                 -> Expr W256   -- result

  MStore         :: Expr W256   -- dst offset
                 -> Expr W256   -- value
                 -> Expr Memory -- prev memory
                 -> Expr Memory -- new memory

  MStore8        :: Expr W256   -- dst offset
                 -> Expr W256   -- value
                 -> Expr Memory -- prev memory
                 -> Expr Memory -- new memory

  EmptyMem       :: Expr Memory
  MSize          :: Expr W256

  -- storage
  SLoad          :: Expr W256    -- address
                 -> Expr W256    -- index
                 -> Expr Storage -- storage
                 -> Expr W256    -- result

  SStore         :: Expr W256    -- address
                 -> Expr W256    -- index
                 -> Expr W256    -- value
                 -> Expr Storage -- old storage
                 -> Expr Storage -- new storae

  EmptyStore     :: Expr Storage
  AbstractStore  :: Expr Storage

deriving instance Show (Expr a)
