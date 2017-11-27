{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import System.Exit

import qualified Data.HashMap.Strict as HashMap
import qualified Data.PQueue.Prio.Max as MaxPQueue

import Imports
import Types
import Driver
import Brain
import Cli
import Util


data Args = Args {
    args_ds :: DriverSettings
  , args_replay :: String
  } deriving(Show)

argsParser :: Parser Args
argsParser =
  Args
    <$> (
    DriverSettings
      <$> option str (long "tag" <> value "")
      <*> (read <$> option str (long "training" <> short 't' <> value "0"))
      <*> switch (long "dump-state")
      <*> switch (long "dump-game")
      <*> switch (long "quiet" <> short 'q'))
    <*> (
    option str (long "replay" <> short 'r' <> value ""))

getArgs = execParser (info (argsParser <**> helper) idm)

main :: IO ()
main = do

  Args{..} <- getArgs
  DriverSettings{..} <- pure args_ds

  case not (null args_replay) of

    True -> do

      out [ "Starting replay mode" ]

      drawGameFinder args_replay Nothing $ \gs -> do
        out ["DEBUG HERE"]
        return ()

    False -> do

      out [ "Starting Vindinium bot, training:", tshow ds_training,
            "Tag", tpack ds_tag
          ]

      driver_net (Key "vhkdc75e") args_ds $ do

        controller_threaded warmupIO $ \bs gs chan ->
          let
            g = gs.>stateGame
            h = gs.>stateHero
          in do
          case g.>gameFinished of
            False -> do

              dumpGame ds_tag gs

              plans <- moveIO bs gs chan

              when (not ds_quiet) $
                let

                  bimg = HashMap.fromList $
                    flip map (take 5 $ MaxPQueue.toList plans) $ \(rew,Plan{..}) ->
                      (goPos, Left clrDef_White)

                in do
                clearTerminal
                out [ "Tag:", "'" <> tpack ds_tag <> "'" ]
                out [ "Hero:", h.>heroName,
                      "(" <> printHero (h.>heroId) <> ")" ]
                blankLine
                out [ drawGame g [bimg] ]
                out [ printHeroStats g ]

            True -> do
              {- Remove game dump -}
              when (not ds_dump_game) $ do
                removeGame ds_tag (gs.>stateGame.gameId) (gs.>stateHero.heroId)

