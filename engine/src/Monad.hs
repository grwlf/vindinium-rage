{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Monad where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List

import Types
import Imports

class (MonadCont m, MonadReader (r m -> m ()) m) => MonadInterrupt r m

-- | Installs the exit point to the reader monad's context of the @m@ action.
-- From there on, the `interrupt` may be called as a part of an action.
install :: (MonadInterrupt r m) => (a -> r m) -> m a -> m (r m)
install f m = do
  callCC $ \k -> do
    a <- local (const k) m
    return (f a)

-- | Interrupts the action with continuation. Operator of the monad @m@ will be
-- able to continue the execution by passing @a@ value to the continuation.
interrupt :: (MonadInterrupt r m) => ((a -> m b) -> r m) -> m a
interrupt f = do
  callCC $ \k -> do
    a <- ask
    a (f k)
    undefined

type Guts self m r a = ReaderT (r self -> self ()) (ContT (r self) m) a

newtype Bot r m a = Bot { unBot :: Guts (Bot r m) m r a }
  deriving(MonadIO, Applicative, Functor, Monad, MonadCont,
           MonadReader ((r (Bot r m)) -> Bot r m ()),
           MonadState s)

data BotResponse m =
    BotInit ((Game,HeroId,UTCTime) -> m ())
  -- ^ Bot is ready to be initialized
  | BotMove Dir ((Game,HeroId,UTCTime) -> m ())
  -- ^ Bot asks for new state, providing some displayable information
  | BotFinish
  -- ^ Bot was terminated

instance (Monad m) => MonadInterrupt r (Bot r m)

class (MonadInterrupt BotResponse m) => MonadBot m

instance (Monad m) => MonadBot (Bot BotResponse m)

runBot :: (MonadIO m) => Bot BotResponse m a -> m (BotResponse (Bot BotResponse m))
runBot m =
  flip runContT return $
  {- FIXME: @flip@ leads to strict evaluation of @error@ -}
  runReaderT
    (unBot (install (const BotFinish) m))
    (error "interrupt before install")

-- | Yields an initialization request from a coroutine
botInit :: (MonadBot m) => m (Game, HeroId, UTCTime)
botInit = interrupt BotInit

-- | Yields @Dir@ from a coroutine, return new Game (and HeroId)
botApplyMove :: (MonadBot m) => Dir -> m (Game, HeroId, UTCTime)
botApplyMove dir = interrupt (BotMove dir)

