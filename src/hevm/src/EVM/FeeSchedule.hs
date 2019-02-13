module EVM.FeeSchedule where

data Num n => FeeSchedule n = FeeSchedule
  { g_zero :: n
  , g_base :: n
  , g_verylow :: n
  , g_low :: n
  , g_mid :: n
  , g_high :: n
  , g_extcode :: n
  , g_balance :: n
  , g_sload :: n
  , g_jumpdest :: n
  , g_sset :: n
  , g_sreset :: n
  , r_sclear :: n
  , r_selfdestruct :: n
  , r_selfdestruct_newaccount :: n
  , g_create :: n
  , g_codedeposit :: n
  , g_call :: n
  , g_callvalue :: n
  , g_callstipend :: n
  , g_newaccount :: n
  , g_exp :: n
  , g_expbyte :: n
  , g_memory :: n
  , g_txcreate :: n
  , g_txdatazero :: n
  , g_txdatanonzero :: n
  , g_transaction :: n
  , g_log :: n
  , g_logdata :: n
  , g_logtopic :: n
  , g_sha3 :: n
  , g_sha3word :: n
  , g_copy :: n
  , g_blockhash :: n
  , g_extcodehash :: n
  } deriving Show

-- For the purposes of this module, we define an EIP as just a fee
-- schedule modification.
type EIP n = Num n => FeeSchedule n -> FeeSchedule n

-- EIP150: Gas cost changes for IO-heavy operations
-- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-150.md>
eip150 :: EIP n
eip150 fees = fees
  { g_extcode = 700
  , g_balance = 400
  , g_sload = 200
  , g_call = 700
  , r_selfdestruct = 5000
  , r_selfdestruct_newaccount = 25000
  }

-- EIP160: EXP cost increase
-- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-160.md>
eip160 :: EIP n
eip160 fees = fees
  { g_expbyte = 50 }

homestead :: Num n => FeeSchedule n
homestead = FeeSchedule
  { g_zero = 0
  , g_base = 2
  , g_verylow = 3
  , g_low = 5
  , g_mid = 8
  , g_high = 10
  , g_extcode = 20
  , g_balance = 20
  , g_sload = 50
  , g_jumpdest = 1
  , g_sset = 20000
  , g_sreset = 5000
  , r_sclear = 15000
  , r_selfdestruct = 0
  , r_selfdestruct_newaccount = 0
  , g_create = 32000
  , g_codedeposit = 200
  , g_call = 40
  , g_callvalue = 9000
  , g_callstipend = 2300
  , g_newaccount = 25000
  , g_exp = 10
  , g_expbyte = 10
  , g_memory = 3
  , g_txcreate = 32000
  , g_txdatazero = 4
  , g_txdatanonzero = 68
  , g_transaction = 21000
  , g_log = 375
  , g_logdata = 8
  , g_logtopic = 375
  , g_sha3 = 30
  , g_sha3word = 6
  , g_copy = 3
  , g_blockhash = 20
  , g_extcodehash = 400
  }

metropolis :: Num n => FeeSchedule n
metropolis = eip160 . eip150 $ homestead
