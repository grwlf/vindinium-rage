{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Monad where

import qualified Control.Lens as Lens
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List as List
import Control.Monad.Interrupt
import Control.Monad.Rnd
import System.Random.Mersenne.Pure64

import Types
import Imports
-- import Perf
-- import Voronoy


type Guts self m r a = ReaderT (r self -> self ()) (ContT (r self) m) a

newtype Bot r m a = Bot { unBot :: Guts (Bot r m) m r a }
  deriving(MonadIO, Applicative, Functor, Monad, MonadRnd g,
           MonadCont, MonadReader ((r (Bot r m)) -> Bot r m ()),
           MonadState s)

type BotOutput = (Dir,BImage)

data BotResponse m =
    BotInit ((Game,HeroId) -> m ())
  -- ^ Bot is ready to be initialized
  | BotMove BotOutput ((Game,HeroId) -> m ())
  -- ^ Bot asks for new state, providing some displayable information
  | BotFinish
  -- ^ Bot was terminated

instance (Monad m) => MonadInterrupt r (Bot r m)

class (MonadInterrupt BotResponse m) => MonadBot m

instance (Monad m) => MonadBot (Bot BotResponse m)

-- instance (Monad m) => MonadInterrupt r (Bot r (StateT s m))

-- instance (MonadBot m) => MonadBot (StateT s m)

runBot :: (MonadIO m, MonadRnd g m) => Bot BotResponse m a -> m (BotResponse (Bot BotResponse m))
runBot m =
  flip runContT return $
  {- FIXME: @flip@ leads to strict evaluation of @error@ -}
  runReaderT
    (unBot (Control.Monad.Interrupt.catch (const BotFinish) m))
    (error "interrupt before catch")

-- | Yields an initialization request from a coroutine
botInit :: (MonadBot m) => m (Game, HeroId)
botInit = interrupt BotInit

-- | Yields @Dir@ adn debug information @dbg@ from a coroutine, return new Game
-- (and HeroId)
botApplyMoveDbg :: (MonadBot m) => (Dir, BImage) -> m (Game, HeroId)
botApplyMoveDbg (dir,dbg) = interrupt (BotMove (dir,dbg))

-- | Yields @Dir@ from a coroutine, return new Game (and HeroId)
botApplyMove :: (MonadBot m) => Dir -> m (Game, HeroId)
botApplyMove dir = botApplyMoveDbg (dir,mempty)


data BotControl = forall a . BotControl {
    botData :: a
  , botMove :: forall m . (MonadIO m) => a -> Game -> HeroId -> m (Dir,BImage)
  }


