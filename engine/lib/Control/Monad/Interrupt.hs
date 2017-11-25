module Control.Monad.Interrupt where

import Control.Monad.Cont
import Control.Monad.Reader

import Types
import Imports (traceM)

class (MonadCont m, MonadReader (r m -> m ()) m) => MonadInterrupt r m


catch :: (MonadInterrupt r m) => (a -> r m) -> m a -> m (r m)
catch f m = do
  callCC $ \k -> do
    a <- local (const k) m
    return (f a)

interrupt :: (MonadInterrupt r m) => ((a -> m b) -> r m) -> m a
interrupt f = callCC $ \k -> do
  a <- ask
  a (f k)
  undefined

