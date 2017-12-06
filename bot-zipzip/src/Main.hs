{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import System.Exit
import System.IO (hReady, stdin)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.PQueue.Prio.Max as MaxPQueue

import Imports
import Types
import Driver
import Brain
import Cli
import Util

import Bot.Random
-- import TF

key :: Key
key = Key "xkwsliww"

data Args = Args {
    args_ds :: DriverSettings
  , args_replay_mb :: Maybe String
  , args_sim_mb :: Maybe String
  , args_turn :: Integer
  } deriving(Show)

getArgs :: IO Args
getArgs = execParser (info ((
  Args
    <$> (
      DriverSettings
      <$> option str (long "tag" <> value "")
      <*> (read <$> option str (long "training" <> short 'T' <> value "0"))
      <*> switch (long "dump-state")
      <*> switch (long "dump-game")
      <*> switch (long "quiet" <> short 'q'))
    <*> (optional (option str (long "replay" <> short 'r')))
    <*> (optional (option str (long "sim" <> short 's')))
    <*> (read <$> option str (long "turn" <> short 't' <> value "0"))
  ) <**> helper) idm)

data BotState = BotState {
    _bs_perf :: [[NominalDiffTime]]
  , _bs_clust :: !(MVar Clustering)
  , _bs_dump :: Bool
  }

$(makeLenses ''BotState)

process :: x -> TChan x -> StateT (TChan x, x) IO () -> IO ()
process x chan m = execStateT m (chan,x) >>= atomically . writeTChan chan . snd

report :: StateT (TChan x, x) IO ()
report = get >>= \(chan,x) -> lift (atomically (writeTChan chan x))


main :: IO ()
main = do
  unbufferStdin

  Args{..} <- getArgs
  DriverSettings{..} <- pure args_ds

  case (args_replay_mb, args_sim_mb) of

    {- Simulation mode -}

    (Nothing, Just args_sim) ->
      let
        hids = map HeroId [1..4]
      in do
      gs0 <- loadState args_sim args_turn
      driver_sim gs0 $
        HashMap.fromList $ hids `zip` [
              controller_simple (return . warmup) $ \bs gs -> do
                delay 0.3
                clearTerminal
                drawGameState "?" gs []
                return (fst $ move bs gs)
            , controller_simple Bot.Random.init Bot.Random.step
            , controller_simple Bot.Random.init Bot.Random.step
            , controller_simple Bot.Random.init Bot.Random.step
            ]


    {- Replay mode -}

    (Just args_replay, Nothing) ->
      let
        replay_path =
          case args_replay of
            (_:[]) -> Nothing
            x -> Just [x]
      in do
      out [ "Starting replay mode", tshow replay_path ]
      drawGameFinder "./data" replay_path (0,args_turn) $ \gs ->
        let
          cls = force $ warmup gs
          (dir,plans) = move cls gs
          g = gs.>stateGame
          h = gs.>stateHero
        in do
        clearTerminal
        drawGameState "?" gs [ drawGamePlans plans ]
        out [ describePlans plans ]
        return ()

    {- Online mode -}

    (Nothing, Nothing) -> do
      out [ "Starting Vindinium bot, training mode:", tshow ds_training,
            "Tag", tpack ds_tag ]

      driver_net key args_ds $
        let
          mkbs0 (gs,_) = BotState <$> pure mempty <*> warmupIO gs <*> pure ds_dump_game
        in do
        controller_threaded mkbs0 $ \bs (gs,tstart) chan ->
          let
            g = gs.>stateGame
            h = gs.>stateHero
            perf = bs.>bs_perf
            out_perf p = out [ Text.unwords $ map (rshow "%-5.0f " . (*100)) p ]

            send dir p = do
              _2._1 %= const dir
              _2._2.bs_perf %= const (p:perf)
              report

            toggle_dump = _2._2.bs_dump %= not
          in
          process (Stay,bs) chan $ do
            case g.>gameFinished of
              False -> do

                gpath <- dumpGame ds_tag gs

                p1 <- diffTimeFrom tstart
                send Stay [p1]

                (dir,plans) <- moveIO (bs.>bs_clust) gs
                p2 <- diffTimeFrom tstart
                send dir [p1,p2]

                when (not ds_quiet) $
                  let
                    bimg = drawGamePlans plans
                  in do
                  clearTerminal
                  out [ printHeader ds_tag g h ]
                  blankLine
                  out [ drawGame g [bimg] ]
                  blankLine
                  out [ printHeroStats g ]

                  p3 <- diffTimeFrom tstart
                  send dir [p1,p2,p3]

                  when (bs.>bs_dump) $ do
                    out [ "This game will be saved to", "'" <> tpack gpath <> "'" ]

                  out [ "Perf:" ]
                  out_perf [ p1,p2,p3 ]
                  forM_ (take 4 perf) $ \p -> do
                    out_perf p

                has_stdin <- liftIO $ hReady stdin
                when has_stdin $ do
                  c <- liftIO getChar
                  case c of
                    's' -> toggle_dump
                    _ -> out ["Press 's' to dump the game"]

              True -> do
                {- Remove game dump -}
                when (not (bs.>bs_dump)) $ do
                  removeGame ds_tag (gs.>stateGame.gameId) (gs.>stateHero.heroId)

    {- Invalid mode -}

    (_, _) -> do
      out [ "Invlaid arguments combination, see --help" ]

