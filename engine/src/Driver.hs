{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Driver where

import Prelude hiding(break)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Control.Lens as Lens

import System.Exit
import System.IO (stderr)
import System.Environment
import Data.IORef

import Imports
import Types
import Client
import Monad
import Sim
import Util


drainTChan :: TChan a -> STM (Maybe a)
drainTChan tc = go Nothing where
  go last = do
    x <- tryReadTChan tc
    case x of
      Nothing -> return last
      Just _ -> go x

-- | Controller implements common interaction between bot functions and the driver.
-- `controller_threaded init step` calls `init` once, then performs `step` in a loop.
--
-- Every move, `step` function starts in new thread as a protection against
-- exception and timeouts
controller_threaded :: forall a g m . (MonadIO m, MonadBot m)
       => ((GameState,UTCTime) -> IO a)
       -> (a -> (GameState,UTCTime) -> TChan (Dir,a) -> IO ())
       -> m ()
controller_threaded botWarmup botMove = do
  (gs0,tstart0) <- botInit
  let g0 = gs0.>stateGame
  let hid0 = gs0.>stateHero.heroId
  bs0 <- liftIO $ botWarmup (gs0,tstart0)
  flip evalStateT (gs0,bs0,tstart0) $ do
    forever $ do
      (gs,bs,tstart) <- get

      bot_channel <- liftIO newTChanIO
      bot_finished <- liftIO newEmptyMVar
      thandle <- liftIO $ forkIO $ do
        catch (do
            botMove bs (gs,tstart) bot_channel
          )
          (\(e::SomeException) -> do
            Text.hPutStrLn stderr $ "Exception '" <> tshow e <> "' from botMove function"
          )
        putMVar bot_finished ()

      liftIO yield {- re-scheduling -}

      whileM_ $
        let
          advance = do
            (bm,bs') <- fromMaybe (Stay,bs) <$> liftIO (atomically $ drainTChan bot_channel)
            case gs.>stateGame.gameFinished of
              False -> do
                (gs',tstart') <- lift $ botApplyMove bm
                put (gs',bs',tstart')
                return False
              True -> do
                lift botFinish
                return True

          kill = do
            liftIO $ killThread thandle

          delay = do
            liftIO $ threadDelay (10^4)
            return True
        in do
        dt <- diffTimeFrom tstart
        timeout <- pure (dt > 0.8)
        finished <- isJust <$> liftIO (tryReadMVar bot_finished)
        case (timeout, finished) of
          (True, _) -> kill >> advance
          (False, True) -> advance
          (False, False) -> delay


-- | Controller implements common interaction between bot functions and the driver.
-- `controller_simple init step` calls `init` once, then performs `step` in a loop.
controller_simple :: (MonadIO m, MonadBot m)
       => (GameState -> IO a)
       -> (a -> GameState -> IO Dir)
       -> m ()
controller_simple botWarmup botMove = do
  (gs0,_) <- botInit
  flip evalStateT gs0 $ do
    a <- liftIO (botWarmup gs0)
    forever $ do
      gs <- get
      x <- liftIO (botMove a gs)
      (gs',_) <- lift $ botApplyMove x
      put gs'


-- | Command-line arguments
data DriverSettings = DriverSettings {
    ds_tag :: String
  -- ^ Game tag, e.g. git revision
  , ds_training :: Integer
  , ds_dump_state :: Bool
  , ds_dump_game :: Bool
  , ds_quiet :: Bool
  } deriving (Show, Eq)

data DriverState m = DriverState {
    _s_gs :: GameState
  -- ^ State returned from the server
  , _s_bot :: ((GameState,UTCTime) -> m ())
  -- ^ Bot step function
  , _s_tstart :: UTCTime
  -- ^ Time when driver started the processing of last request
}

initDriverState gs bot tstart = DriverState gs bot tstart

$(makeLenses ''DriverState)

-- | Network driver to play on the real competition server
driver_net
  :: Key
  -> DriverSettings
  -> (forall m g . (MonadBot m, MonadIO m) => m ())
  -> IO ()
driver_net key DriverSettings{..} bot =
  let
    cls = Client.Settings key ("http://vindinium.org" :: Text)
  in do
  {- Warmup -}
  bot' <- runBot bot >>= \case
      BotInit k -> return k
      _ -> fail "driver_net: expected BotInit"

  {- Initial state query -}
  gs0 <- (case ds_training > 0 of
          False -> startArena cls
          True -> startTraining cls (Just ds_training) Nothing)

  tstart0 <- liftIO getCurrentTime

  {- Main loop -}
  g0 <- pure (gs0.>stateGame)
  hid <- pure (gs0.>stateHero.heroId)
  game_id <- pure (gs0.>stateGame.gameId)

  s_init <- pure $ initDriverState gs0 bot' tstart0

  flip evalStateT s_init $ do
  whileM $ do
    tstart <- use s_tstart
    gs <- use s_gs
    bot <- use s_bot

    r <- lift $ runBot $ bot (gs, tstart)

    case r of
      BotMove dir k -> do
        mb_gs' <- applyMove cls gs dir
        tstart' <- liftIO getCurrentTime
        case mb_gs' of
          Just gs' -> do
            s_gs %= const gs'
            s_tstart %= const tstart'
            s_bot %= const k
            return (Nothing :: Maybe ()) {- loop -}

          Nothing ->
            return (Just ())

      BotFinish ->
        return (Just ())

data DriverSim_State m = DriverSim_State {
    _ds_gs :: GameState
  , _ds_ctl :: HashMap HeroId ((GameState,UTCTime) -> m ())
  }

$(makeLenses ''DriverSim_State)

-- | Simulation driver running local simulation engine
driver_sim
  :: GameState
  -> (forall m g . (MonadBot m, MonadIO m) => HashMap HeroId (m ()))
  -> IO ()

driver_sim ss0 ctl = do

  handle (\(e :: SomeException) -> do
       Text.putStrLn $ "Got an exception: " <> tshow e) $
    do

  {- Warmup -}
  {- FIXME: what about sorting by HeroId ? -}
  ctl' <- forM ctl $ \bot -> do
    runBot bot >>= \case
      BotInit k -> return k
      _ -> fail "driver_sim2: expected BotInit"

  let ds0 = DriverSim_State ss0 ctl'

  flip evalStateT ds0 $ do

  {- Set uniform game length -}
  ds_gs.stateGame.gameMaxTurns %= const 1200

  {- Make sure heroes are not initially crashed -}
  forM_ [HeroId i | i<-[1..4]] $ \hid -> do
    ds_gs.stateGame.gameHeroes.(idx hid).heroCrashed %= const False

  whileM_ $ do

    gs  <- use ds_gs
    hid <- pure (gs.>stateHero.heroId)
    bot <- use (ds_ctl.(idx (gs.>stateHero.heroId)))

    tstart <- liftIO $ getCurrentTime
    r <- lift $ runBot $ bot (gs,tstart)

    case r of
      BotMove dir k -> do
        ds_gs.stateGame %= sim hid dir
        ds_ctl.(idx hid) %= const k
        hid' <- pure (nextHeroId hid)
        ds_gs.stateHero %= const (gs.>stateGame.gameHeroes.(idx hid'))
        return True

      BotFinish -> do
        return False

