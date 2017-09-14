module EVM.FeeSchedule where

type N = Int

data FeeSchedule = FeeSchedule
  { g_zero :: N
  , g_base :: N
  , g_verylow :: N
  , g_low :: N
  , g_mid :: N
  , g_high :: N
  , g_extcode :: N
  , g_balance :: N
  , g_sload :: N
  , g_jumpdest :: N
  , g_sset :: N
  , g_sreset :: N
  , r_sclear :: N
  , r_selfdestruct :: N
  , g_create :: N
  , g_codedeposit :: N
  , g_call :: N
  , g_callvalue :: N
  , g_callstipend :: N
  , g_newaccount :: N
  , g_exp :: N
  , g_expbyte :: N
  , g_memory :: N
  , g_txcreate :: N
  , g_txdatazero :: N
  , g_txdatanonzero :: N
  , g_transaction :: N
  , g_log :: N
  , g_logdata :: N
  , g_logtopic :: N
  , g_sha3 :: N
  , g_sha3word :: N
  , g_copy :: N
  , g_blockhash :: N
  } deriving Show

metropolis :: FeeSchedule
metropolis = FeeSchedule
  { g_zero = 0
  , g_base = 2
  , g_verylow = 3
  , g_low = 5
  , g_mid = 8
  , g_high = 10
  , g_extcode = 700
  , g_balance = 400
  , g_sload = 200
  , g_jumpdest = 1
  , g_sset = 20000
  , g_sreset = 5000
  , r_sclear = 15000
  , r_selfdestruct = 24000
  , g_create = 32000
  , g_codedeposit = 200
  , g_call = 700
  , g_callvalue = 9000
  , g_callstipend = 2300
  , g_newaccount = 25000
  , g_exp = 10
  , g_expbyte = 50
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
  }
