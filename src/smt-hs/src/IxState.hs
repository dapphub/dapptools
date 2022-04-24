module IxState where

import Control.Monad.Identity

class IxMonad m where
    ireturn :: a -> m p p a
    ibind :: m p q a -> (a -> m q r b) -> m p r b

newtype IxStateT m si so v = IxStateT { runIxStateT:: si -> m (so,v) }

instance Monad m => IxMonad (IxStateT m) where
  ireturn x = IxStateT (\si -> Prelude.return (si,x))
  ibind (IxStateT m) f = IxStateT (\si -> m si Prelude.>>= (\ (sm,x) -> runIxStateT (f x) sm))

get :: Monad m => IxStateT m si si si
get = IxStateT (\si -> Prelude.return (si,si))

put :: Monad m => so -> IxStateT m si so ()
put x = IxStateT (\si -> Prelude.return (x,()))

return :: (Monad m) => a -> IxStateT m si si a
return = ireturn

(>>=) :: (Monad m) => IxStateT m p q a -> (a -> IxStateT m q r b) -> IxStateT m p r b
(>>=) = ibind

(>>) :: (Monad m) => IxStateT m p q a -> IxStateT m q r b -> IxStateT m p r b
v >> w = v IxState.>>= const w

type IxState si so ret = IxStateT Identity si so ret
