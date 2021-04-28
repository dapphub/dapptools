module EVM.FeeSchedule where

data FeeSchedule n = FeeSchedule
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
  , g_selfdestruct :: n
  , g_selfdestruct_newaccount :: n
  , r_selfdestruct :: n
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
  , g_quaddivisor :: n
  , g_ecadd :: n
  , g_ecmul :: n
  , g_pairing_point :: n
  , g_pairing_base :: n
  , g_fround :: n
  , r_block :: n
  , g_cold_sload :: n
  , g_cold_account_access :: n
  , g_warm_storage_read :: n
  , g_access_list_address :: n
  , g_access_list_storage_key :: n
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
  , g_selfdestruct = 5000
  , g_selfdestruct_newaccount = 25000
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
  , g_selfdestruct = 0
  , g_selfdestruct_newaccount = 0
  , r_selfdestruct = 24000
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
  , g_quaddivisor = 20
  , g_ecadd = 500
  , g_ecmul = 40000
  , g_pairing_point = 80000
  , g_pairing_base = 100000
  , g_fround = 1
  , r_block = 2000000000000000000
  , g_cold_sload = 2100
  , g_cold_account_access = 2600
  , g_warm_storage_read = 100
  , g_access_list_address = 2400
  , g_access_list_storage_key = 1900
  }

metropolis :: Num n => FeeSchedule n
metropolis = eip160 . eip150 $ homestead

-- EIP1108: Reduce alt_bn128 precompile gas costs
-- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1108.md>
eip1108 :: EIP n
eip1108 fees = fees
  { g_ecadd = 150
  , g_ecmul = 6000
  , g_pairing_point = 34000
  , g_pairing_base = 45000
  }

-- EIP1884: Repricing for trie-size-dependent opcodes
-- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-1884.md>
eip1884 :: EIP n
eip1884 fees = fees
  { g_sload = 800
  , g_balance = 700
  , g_extcodehash = 700
  }

-- EIP2028: Transaction data gas cost reduction
-- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2028.md>
eip2028 :: EIP n
eip2028 fees = fees
  { g_txdatanonzero = 16
  }

-- EIP2200: Structured definitions for gas metering
-- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2200.md>
eip2200 :: EIP n
eip2200 fees = fees
  { g_sload = 800
  , g_sset = 20000   -- not changed
  , g_sreset = 5000  -- not changed
  , r_sclear = 15000 -- not changed
  }

istanbul :: Num n => FeeSchedule n
istanbul = eip1108 . eip1884 . eip2028 . eip2200 $ metropolis

  -- EIP2929: Gas cost increases for state access opcodes
  -- <https://github.com/ethereum/EIPs/blob/master/EIPS/eip-2929.md>
eip2929 :: EIP n
eip2929 fees = fees
  { g_sload = 100 
  , g_sreset = 5000 - 2100 
  , g_call = 2600
  , g_balance = 2600
  , g_extcode = 2600
  , g_extcodehash = 2600
  }

berlin :: Num n => FeeSchedule n
berlin = eip2929 istanbul
