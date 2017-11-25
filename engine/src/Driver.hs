{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Client
import Types
import Util
import Sigint
import Perf
import Args
import Monad
import Sim

-- | Bot driver function
controller_simple :: (MonadIO m, MonadBot m, MonadRnd g m)
       => (forall m . (MonadIO m) => Game -> m a)
       -> (forall g m . (MonadIO m, MonadRnd g m) => a -> Game -> HeroId -> m (Dir,BImage))
       -> m ()
controller_simple botWarmup botMove = do
  (g0,hid) <- botInit
  flip evalStateT g0 $ do
    a <- liftIO (botWarmup g0)
    forever $ do
      g <- get
      x <- botMove a g hid
      (g',hid') <- lift $ botApplyMoveDbg x
      put g'

-- | Bot driver function
controller_simple2 :: (MonadIO m, MonadBot m, MonadRnd g m)
       => (forall m . (MonadIO m) => Game -> m a)
       -> (forall m g . (MonadIO m, MonadRnd g m) => a -> Game -> HeroId -> m (Dir,BImage))
       -> m ()
controller_simple2 botWarmup botMove = do
  (g0,hid) <- botInit
  flip evalStateT g0 $ do
    a <- liftIO (botWarmup g0)
    forever $ do
      g <- get
      x <- (botMove a g hid)
      (g',hid') <- lift $ botApplyMoveDbg x
      traceM $ gameFindKiller g hid g'
      put g'

data DriverState m = DriverState {
    _s_quiet :: Bool
  -- ^ Quiet
  , _s_server :: ServerState
  , _s_nmove :: Integer
  -- ^ Current move
  , _s_bot :: ((Game,HeroId) -> m ())
  -- ^ Bot step function
  , _s_tag :: String
  -- ^ See 'args_tag'
  , _s_perf_got_state :: PerfCnt
  , _s_perf_got_dir :: PerfCnt
  , _s_perf_1sec :: Maybe PerfCnt
  , _s_dbg :: BImage
}

initDriverState q ss i bot t perf = DriverState q ss i bot t perf 0 Nothing mempty

$(makeLenses ''DriverState)

initRndGen = pureMT . maybe 33 read <$> lookupEnv "SEED"

-- | Network driver to play on the real competition server
driver_net
  :: Key
  -> Args
  -> (forall m g . (MonadRnd g m, MonadBot m, MonadIO m) => m ())
  -> IO ()

driver_net key Args{..} bot =
  let
    cls = Client.Settings key ("http://vindinium.org" :: Text)
  in do
  handle (\(e :: SomeException) -> do
       Text.putStrLn $ "Got an exception: " <> tshow e) $
    do
    mv1sec <- estimate1Sec
    rnd0 <- initRndGen

    flip evalRndT rnd0 $ do
      flip runReaderT cls $ do

        {- Warmup -}
        bot' <- runBot bot >>= \case
            BotInit k -> return k
            _ -> fail "driver_net: expected BotInit"

        out [ "Starting Vindinium bot, training: " <> tshow args_training <> " " <>
              "Tag " <> tpack args_tag
            ]

        {- Initial state query -}
        ss0 <- (case args_training > 0 of
                False -> startArena
                True -> startTraining (Just args_training) Nothing)

        perf_got_state <- perfcnt

        {- Main loop -}
        g0 <- pure (ss0.>stateGame)
        hid <- pure (ss0.>stateHero.heroId)
        game_id <- pure (ss0.>stateGame.gameId)
        i <- pure 0

        when args_dump_state $ do
          dumpState args_tag game_id i ss0

        s_init <- pure $ initDriverState args_quiet ss0 i bot' args_tag perf_got_state

        flip evalStateT s_init $ do
        loop $ do
          ss <- use s_server
          bot <- use s_bot

          when (not args_quiet) $ do
            dbg <- use s_dbg
            clearTerminal
            out [ tpack args_tag ]
            out [ view stateViewUrl ss ]
            out [ drawGame' (ss.>stateGame) [dbg] ]
            out ["Me ", tshow (ss.>stateHero.heroId)]
            out [ printHeroStats (ss.>stateGame) ]

          when args_dump_game $ do
            i <- use s_nmove
            dumpGame args_tag game_id i ss

          r <- lift $ lift $ runBot $ bot (ss.>stateGame, ss.>stateHero.heroId)

          (s_perf_got_dir %=) =<< const <$> perfcnt

          do {
            sta <- use s_perf_got_state;
            sto <- use s_perf_got_dir;
            Perf.create "loop" sta sto;
            old1sec <- use s_perf_1sec;
            new1sec <- liftIO (tryTakeMVar mv1sec);
            let
              s1sec = msum [new1sec, old1sec]
            in do
            s_perf_1sec %= const s1sec;
            case s1sec of
              Nothing -> return ()
              Just x ->
                let
                  cnt = ((sto-sta)*100) `div` x
                in do
                Perf.counter "busy,%" cnt
          }

          case r of
            BotMove (dir,dbg) k -> do
              ss' <- applyMove ss dir
              s_server %= const ss'
              s_bot %= const k
              s_nmove %= (+1)
              s_dbg %= const dbg

              (s_perf_got_state %=) =<< const <$> perfcnt

            BotFinish -> do
              when args_dump_state $ do
                dumpState args_tag game_id i ss
              break ()

        {- Print perf counters -}
        when (args_dump_perf) $ do
          dumpPerf args_tag game_id

  Perf.print

data DriverSim_State m = DriverSim_State {
    _ds_g :: Game
  , _ds_hid :: HeroId
  , _ds_ctl :: HashMap HeroId (((Game,HeroId) -> m ()))
  , _ds_dbg :: HashMap HeroId BImage
  }

$(makeLenses ''DriverSim_State)

-- | Simulation driver running local simulation engine
driver_sim
  :: FilePath
  -> (forall m g . (MonadRnd g m, MonadBot m, MonadIO m) => HashMap HeroId (m ()))
  -> IO ()

driver_sim file ctl = do

  rnd0 <- initRndGen

  handle (\(e :: SomeException) -> do
       Text.putStrLn $ "Got an exception: " <> tshow e) $
    do

  flip evalRndT rnd0 $ do

    {- Warmup -}
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

    loop $ do

      g   <- use ds_g
      hid <- use ds_hid
      bot <- use (ds_ctl.(idx hid))

      when (hid == HeroId 1) $ do
        dbg <- toList <$> use ds_dbg
        clearTerminal
        out [ drawGame' g dbg ]
        err [ printHeroStats g ]

      r <- lift $ lift $ runBot $ bot (g,hid)

      case r of
        BotMove (dir,dbg) k -> do
          ds_g %= sim hid dir
          ds_hid %= nextHeroId
          ds_ctl.(idx hid) %= const k
          ds_dbg.(idx hid) %= const dbg

        BotFinish -> do
          break ()

