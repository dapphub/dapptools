module EVM.Exec where

import Data.ByteString (ByteString)

import Control.Monad.State (State)
import Control.Lens

import EVM
import EVM.Types
import EVM.Keccak (newContractAddress)

ethrunAddress :: Addr
ethrunAddress = Addr 0x00a329c0648769a73afac7f9381e08fb43dbea72

vmForEthrunCreation :: ByteString -> VM
vmForEthrunCreation creationCode =
  (makeVm $ VMOpts
    { vmoptCode = creationCode
    , vmoptCalldata = ""
    , vmoptValue = 0
    , vmoptAddress = newContractAddress ethrunAddress 1
    , vmoptCaller = ethrunAddress
    , vmoptOrigin = ethrunAddress
    , vmoptCoinbase = 0
    , vmoptNumber = 0
    , vmoptTimestamp = 0
    , vmoptGaslimit = 0
    , vmoptDifficulty = 0
    }) & set (env . contracts . at ethrunAddress)
             (Just (initialContract mempty))

exec :: State VM VMResult
exec =
  use EVM.result >>= \case
    EVM.VMRunning -> EVM.exec1 >> exec
    x -> return x
