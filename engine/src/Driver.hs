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
import Args
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
controller_threaded :: forall a g m. (MonadIO m, MonadBot m)
       => (Game -> IO a)
       -> (a -> Game -> HeroId -> TChan (Dir,a) -> IO ())
       -> m ()
controller_threaded botWarmup botMove = do
  (g0,hid0,tstart0) <- botInit
  bs0 <- liftIO $ botWarmup g0
  flip evalStateT (g0,bs0,tstart0) $ do
    forever $ do
      (g,bs,tstart) <- get

      bot_channel <- liftIO newTChanIO
      bot_finished <- liftIO newEmptyMVar
      handle <- liftIO $ forkIO $ do
        catch (do
            botMove bs g hid0 bot_channel
          )
          (\(e::SomeException) -> do
            Text.hPutStrLn stderr $ "Exception '" <> tshow e <> "' from botMove function"
          )
        putMVar bot_finished ()

      liftIO yield {- re-scheduling -}

      whileM $
        let
          advance = do
            (bm,bs') <- fromMaybe (Stay,bs) <$> liftIO (atomically $ drainTChan bot_channel)
            (g',_,tstart') <- lift $ botApplyMove bm
            put (g',bs',tstart')
            return False

          kill = do
            liftIO $ killThread handle

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
       => (forall m . (MonadIO m) => Game -> m a)
       -> (forall g m . (MonadIO m) => a -> Game -> HeroId -> m Dir)
       -> m ()
controller_simple botWarmup botMove = do
  (g0,hid,_) <- botInit
  flip evalStateT g0 $ do
    a <- liftIO (botWarmup g0)
    forever $ do
      g <- get
      x <- botMove a g hid
      (g',hid',_) <- lift $ botApplyMove x
      put g'

-- | Bot driver function
controller_simple2 :: (MonadIO m, MonadBot m)
       => (forall m . (MonadIO m) => Game -> m a)
       -> (forall m g . (MonadIO m) => a -> Game -> HeroId -> m Dir)
       -> m ()
controller_simple2 botWarmup botMove = do
  (g0,hid,_) <- botInit
  flip evalStateT g0 $ do
    a <- liftIO (botWarmup g0)
    forever $ do
      g <- get
      x <- (botMove a g hid)
      (g',hid',_) <- lift $ botApplyMove x
      traceM $ gameFindKiller g hid g'
      put g'

data DriverState m = DriverState {
    _s_quiet :: Bool
  -- ^ Quiet
  , _s_server :: ServerState
  -- ^ State returned from the server
  , _s_nmove :: Integer
  -- ^ Current move
  , _s_bot :: ((Game,HeroId,UTCTime) -> m ())
  -- ^ Bot step function
  , _s_tag :: String
  -- ^ See 'args_tag'
  , _s_tstart :: UTCTime
  -- ^ Time when driver started the processing of last request
}

initDriverState q ss i bot t tstart = DriverState q ss i bot t tstart

$(makeLenses ''DriverState)

-- | Network driver to play on the real competition server
driver_net
  :: Key
  -> Args
  -> (forall m g . (MonadBot m, MonadIO m) => m ())
  -> IO ()

driver_net key Args{..} bot =
  let
    cls = Client.Settings key ("http://vindinium.org" :: Text)
  in do
  handle (\(e :: SomeException) -> do
       Text.putStrLn $ "Got an exception: " <> tshow e) $
    do
    flip runReaderT cls $ do

      {- Warmup -}
      bot' <- runBot bot >>= \case
          BotInit k -> return k
          _ -> fail "driver_net: expected BotInit"

      {- Initial state query -}
      ss0 <- (case args_training > 0 of
              False -> startArena
              True -> startTraining (Just args_training) Nothing)

      tstart0 <- liftIO getCurrentTime

      {- Main loop -}
      g0 <- pure (ss0.>stateGame)
      hid <- pure (ss0.>stateHero.heroId)
      game_id <- pure (ss0.>stateGame.gameId)
      i <- pure 0

      when args_dump_state $ do
        dumpState args_tag game_id i ss0

      s_init <- pure $ initDriverState args_quiet ss0 i bot' args_tag tstart0

      flip evalStateT s_init $ do
      whileM $ do
        tstart <- use s_tstart
        ss <- use s_server
        bot <- use s_bot

        do
          i <- use s_nmove
          dumpGame args_tag game_id i ss

        r <- lift $ runBot $ bot (ss.>stateGame, ss.>stateHero.heroId, tstart)

        case r of
          BotMove dir k -> do
            ss' <- applyMove ss dir
            tstart' <- liftIO getCurrentTime

            s_server %= const ss'
            s_tstart %= const tstart'
            s_bot %= const k
            s_nmove %= (+1)
            return True

          BotFinish -> do
            when args_dump_state $ do
              dumpState args_tag game_id i ss
            return False

      {- Remove game record -}
      when (not args_dump_game) $ do
        removeGame args_tag game_id

data DriverSim_State m = DriverSim_State {
    _ds_g :: Game
  , _ds_hid :: HeroId
  , _ds_ctl :: HashMap HeroId (((Game,HeroId,UTCTime) -> m ()))
  , _ds_dbg :: HashMap HeroId BImage
  }

$(makeLenses ''DriverSim_State)

-- | Simulation driver running local simulation engine
driver_sim
  :: FilePath
  -> (forall m g . (MonadBot m, MonadIO m) => HashMap HeroId (m ()))
  -> IO ()

driver_sim file ctl = do

  handle (\(e :: SomeException) -> do
       Text.putStrLn $ "Got an exception: " <> tshow e) $
    do

  {- Warmup -}
  {- FIXME: what about sorting by HeroId ? -}
  ctl' <- forM ctl $ \bot -> do
    runBot bot >>= \case
      BotInit k -> return k
      _ -> fail "driver_sim2: expected BotInit"

  {- Initial server state -}
  ss0 <- loadState file

  let ds0 = DriverSim_State (ss0^.stateGame) (HeroId 1) ctl' mempty

  flip evalStateT ds0 $ do

  {- Set uniform game length -}
  ds_g.gameMaxTurns %= const 1200

  {- Make sure heroes are not initially crashed -}
  forM_ [HeroId i | i<-[1..4]] $ \hid -> do
    ds_g.gameHeroes.(idx hid).heroCrashed %= const False

  whileM $ do

    g   <- use ds_g
    hid <- use ds_hid
    bot <- use (ds_ctl.(idx hid))

    tstart <- liftIO $ getCurrentTime
    r <- lift $ runBot $ bot (g,hid,tstart)

    case r of
      BotMove dir k -> do
        ds_g %= sim hid dir
        ds_hid %= nextHeroId
        ds_ctl.(idx hid) %= const k
        return True

      BotFinish -> do
        return False

