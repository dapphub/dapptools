module EVM.Demand (demand) where

import Control.DeepSeq (NFData, force)
import Control.Exception.Base (evaluate)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- | This is an easy way to force full evaluation of a value inside of
-- the IO monad, being essentially just the composition of @evaluate@
-- and @force@.
demand :: (MonadIO m, NFData a) => a -> m ()
demand x = do
  _ <- liftIO (evaluate (force x))
  return ()
