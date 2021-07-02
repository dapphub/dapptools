module EVM.Exec where

import EVM
import EVM.Concrete (createAddress)
import EVM.Symbolic (litAddr)
import EVM.Types

import qualified EVM.FeeSchedule as FeeSchedule

import Control.Lens
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (runState)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)

import qualified Control.Monad.State.Class as State

ethrunAddress :: Addr
ethrunAddress = Addr 0x00a329c0648769a73afac7f9381e08fb43dbea72

vmForEthrunCreation :: ByteString -> VM
vmForEthrunCreation creationCode =
  (makeVm $ VMOpts
    { vmoptContract = initialContract (InitCode (ConcreteBuffer creationCode))
    , vmoptCalldata = (mempty, 0)
    , vmoptValue = 0
    , vmoptAddress = createAddress ethrunAddress 1
    , vmoptCaller = litAddr ethrunAddress
    , vmoptOrigin = ethrunAddress
    , vmoptCoinbase = 0
    , vmoptNumber = 0
    , vmoptTimestamp = 0
    , vmoptBlockGaslimit = 0
    , vmoptGasprice = 0
    , vmoptDifficulty = 0
    , vmoptGas = 0xffffffffffffffff
    , vmoptGaslimit = 0xffffffffffffffff
    , vmoptBaseFee = 0
    , vmoptPriorityFee = 0
    , vmoptMaxCodeSize = 0xffffffff
    , vmoptSchedule = FeeSchedule.berlin
    , vmoptChainId = 1
    , vmoptCreate = False
    , vmoptStorageModel = ConcreteS
    , vmoptTxAccessList = mempty
    , vmoptAllowFFI = False
    }) & set (env . contracts . at ethrunAddress)
             (Just (initialContract (RuntimeCode mempty)))

exec :: MonadState VM m => m VMResult
exec =
  use EVM.result >>= \case
    Nothing -> State.state (runState exec1) >> exec
    Just x  -> return x

run :: MonadState VM m => m VM
run =
  use EVM.result >>= \case
    Nothing -> State.state (runState exec1) >> run
    Just _  -> State.get

execWhile :: MonadState VM m => (VM -> Bool) -> m Int
execWhile p = go 0
  where
    go i = do
      x <- State.get
      if p x && isNothing (view result x)
        then do
          State.state (runState exec1)
          go $! (i + 1)
      else
        return i

-- locateBreakpoint :: UIState -> Text -> Int -> Maybe [(Word256, Vector Bool)]
-- locateBreakpoint ui fileName lineNo = do
--   (i, (t, s)) <-
--     flip find (Map.toList (ui ^. uiSourceCache . sourceFiles))
--       (\(_, (t, _)) -> t == fileName)
--   let ls = BS.split 0x0a s
--       l = ls !! (lineNo - 1)
--       offset = 1 + sum (map ((+ 1) . BS.length) (take (lineNo - 1) ls))
--       horizon = offset + BS.length l
--   return $ Map.elems (ui ^. uiVm . _Just . env . solc)
--     & map (\c -> (
--         c ^. solcCodehash,
--         Vector.create $ new (Seq.length (c ^. solcSrcmap)) >>= \v -> do
--           fst $ foldl' (\(!m, !j) (sm@SM { srcMapOffset = o }) ->
--             if srcMapFile sm == i && o >= offset && o < horizon
--             then (m >> write v j True, j + 1)
--             else (m >> write v j False, j + 1)) (return (), 0) (c ^. solcSrcmap)
--           return v
--       ))
