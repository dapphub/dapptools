{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
  Expr implements an abstract respresentation of an EVM program

  This type can give insight into the provenance of a term which is useful, both for the aesthetic purpose of printing terms in a richer way, but also do optimizations on the AST instead of letting the SMT solver do all the heavy lifting.

  Memory and storage are both represented as a sequence of writes on top of some base state.
  In the case of Memory that base state is always empty, but in the case of Storage it can be either empty (for init code) or abstract, for runtime code.

  Calldata is immutable, so we can simply represent it as an slice from the calldata buffer in a particular frame
  Returndata is represented as a slice of a particular memory expression, allowing returndata in the current call frame to reference memory from the sub call frame.
-}
module EVM.Expr where

import Data.DoubleWord (Word256)

-- phantom type tags for AST construction
data EType
  = Memory
  | Storage
  | Returndata
  | Logs
  | EWord
  | End

data Expr (a :: EType) where

  -- identifiers
  Lit            :: Word256 -> Expr EWord
  Var            :: String  -> Expr EWord

  -- control flow
  Invalid        :: Expr End
  SelfDestruct   :: Expr EWord       -> Expr End
  Revert         :: String           -> Expr End
  Stop           :: Expr Storage     -> Expr End
  Return         :: Expr Returndata  -> Expr Storage -> Expr End
  ITE            :: Expr EWord       -> Expr EWord   -> Expr EWord -> Expr EWord

  -- integers
  Add            :: Expr EWord -> Expr EWord -> Expr EWord
  Sub            :: Expr EWord -> Expr EWord -> Expr EWord
  Mul            :: Expr EWord -> Expr EWord -> Expr EWord
  Div            :: Expr EWord -> Expr EWord -> Expr EWord
  SDiv           :: Expr EWord -> Expr EWord -> Expr EWord
  Mod            :: Expr EWord -> Expr EWord -> Expr EWord
  SMod           :: Expr EWord -> Expr EWord -> Expr EWord
  AddMod         :: Expr EWord -> Expr EWord -> Expr EWord
  MulMod         :: Expr EWord -> Expr EWord -> Expr EWord
  Exp            :: Expr EWord -> Expr EWord -> Expr EWord
  Sex            :: Expr EWord -> Expr EWord

  -- booleans
  LT             :: Expr EWord -> Expr EWord -> Expr EWord
  GT             :: Expr EWord -> Expr EWord -> Expr EWord
  SLT            :: Expr EWord -> Expr EWord -> Expr EWord
  SGT            :: Expr EWord -> Expr EWord -> Expr EWord
  Eq             :: Expr EWord -> Expr EWord -> Expr EWord
  IsZero         :: Expr EWord -> Expr EWord -> Expr EWord

  -- bits
  And            :: Expr EWord -> Expr EWord -> Expr EWord
  Or             :: Expr EWord -> Expr EWord -> Expr EWord
  Xor            :: Expr EWord -> Expr EWord -> Expr EWord
  Not            :: Expr EWord -> Expr EWord
  SHL            :: Expr EWord -> Expr EWord -> Expr EWord
  SHR            :: Expr EWord -> Expr EWord -> Expr EWord
  SAR            :: Expr EWord -> Expr EWord -> Expr EWord

  -- keccak
  Keccak         :: Expr EWord        -- offset
                 -> Expr EWord        -- size
                 -> Expr Memory       -- memory
                 -> Expr EWord        -- result

  -- block context
  Origin         :: Expr EWord
  BlockHash      :: Expr EWord
  Coinbase       :: Expr EWord
  Timestamp      :: Expr EWord
  Number         :: Expr EWord
  Difficulty     :: Expr EWord
  GasLimit       :: Expr EWord
  ChainId        :: Expr EWord
  BaseFee        :: Expr EWord

  -- frame context
  CallValue      :: Int               -- frame idx
                 -> Expr EWord

  Caller         :: Int               -- frame idx
                 -> Expr EWord

  Address        :: Int               -- frame idx
                 -> Expr EWord

  Balance        :: Int               -- frame idx
                 -> Int               -- PC (in case we're checking the current contract)
                 -> Expr EWord        -- address
                 -> Expr EWord

  SelfBalance    :: Int               -- frame idx
                 -> Int               -- PC
                 -> Expr EWord

  Gas            :: Int               -- frame idx
                 -> Int               -- PC
                 -> Expr EWord

  -- calldata
  CalldataSize   :: Expr EWord

  CalldataLoad   :: Int                -- frame idx
                 -> Expr EWord         -- data idx
                 -> Expr EWord         -- result

  CalldataCopy   :: Int                -- frame idx
                 -> Expr EWord         -- dst offset
                 -> Expr EWord         -- src offset
                 -> Expr EWord         -- size
                 -> Expr Memory        -- old memory
                 -> Expr Memory        -- new memory

  -- code
  CodeSize       :: Expr EWord         -- address
                 -> Expr EWord         -- size

  ExtCodeHash    :: Expr EWord         -- address
                 -> Expr EWord         -- size

  CodeCopy       :: Expr EWord         -- address
                 -> Expr EWord         -- dst offset
                 -> Expr EWord         -- src offset
                 -> Expr EWord         -- size
                 -> Expr Memory        -- old memory
                 -> Expr Memory        -- new memory

  -- returndata
  ReturndataSize :: Expr EWord
  ReturndataCopy :: Expr EWord         -- dst offset
                 -> Expr EWord         -- src offset
                 -> Expr EWord         -- size
                 -> Expr Returndata    -- returndata
                 -> Expr Memory        -- old mem
                 -> Expr Memory        -- new mem

  EmptyRet       :: Expr Returndata
  WriteRet       :: Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr Memory        -- memory
                 -> Expr Returndata    -- new returndata

  -- logs
  EmptyLog       :: Expr Logs

  Log0           :: Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- old logs
                 -> Expr Logs          -- new logs

  Log1           :: Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr EWord         -- topic
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- old logs
                 -> Expr Logs          -- new logs

  Log2           :: Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr EWord         -- topic 1
                 -> Expr EWord         -- topic 2
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- old logs
                 -> Expr Logs          -- new logs

  Log3           :: Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr EWord         -- topic 1
                 -> Expr EWord         -- topic 2
                 -> Expr EWord         -- topic 3
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- old logs
                 -> Expr Logs          -- new logs

  Log4           :: Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr EWord         -- topic 1
                 -> Expr EWord         -- topic 2
                 -> Expr EWord         -- topic 3
                 -> Expr EWord         -- topic 4
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- old logs
                 -> Expr Logs          -- new logs

  -- Contract Creation
  Create         :: Expr EWord         -- value
                 -> Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- address

  Create2        :: Expr EWord         -- value
                 -> Expr EWord         -- offset
                 -> Expr EWord         -- size
                 -> Expr EWord         -- salt
                 -> Expr Memory        -- memory
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- address

  -- Calls
  Call           :: Expr EWord         -- gas
                 -> Maybe (Expr EWord) -- target
                 -> Expr EWord         -- value
                 -> Expr EWord         -- args offset
                 -> Expr EWord         -- args size
                 -> Expr EWord         -- ret offset
                 -> Expr EWord         -- ret size
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- success

  CallCode       :: Expr EWord         -- gas
                 -> Expr EWord         -- address
                 -> Expr EWord         -- value
                 -> Expr EWord         -- args offset
                 -> Expr EWord         -- args size
                 -> Expr EWord         -- ret offset
                 -> Expr EWord         -- ret size
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- success

  DelegeateCall  :: Expr EWord         -- gas
                 -> Expr EWord         -- address
                 -> Expr EWord         -- value
                 -> Expr EWord         -- args offset
                 -> Expr EWord         -- args size
                 -> Expr EWord         -- ret offset
                 -> Expr EWord         -- ret size
                 -> Expr Logs          -- logs
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- success

  -- memory
  MLoad          :: Expr EWord         -- index
                 -> Expr Memory        -- memory
                 -> Expr EWord         -- result

  MStore         :: Expr EWord         -- dst offset
                 -> Expr EWord         -- value
                 -> Expr Memory        -- prev memory
                 -> Expr Memory        -- new memory

  MStore8        :: Expr EWord         -- dst offset
                 -> Expr EWord         -- value
                 -> Expr Memory        -- prev memory
                 -> Expr Memory        -- new memory

  EmptyMem       :: Expr Memory
  MSize          :: Expr EWord

  -- storage
  SLoad          :: Expr EWord         -- address
                 -> Expr EWord         -- index
                 -> Expr Storage       -- storage
                 -> Expr EWord         -- result

  SStore         :: Expr EWord         -- address
                 -> Expr EWord         -- index
                 -> Expr EWord         -- value
                 -> Expr Storage       -- old storage
                 -> Expr Storage       -- new storae

  EmptyStore     :: Expr Storage
  AbstractStore  :: Expr Storage

deriving instance Show (Expr a)
