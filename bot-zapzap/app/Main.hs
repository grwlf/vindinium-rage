{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import System.Exit

import Imports
import Types
import Driver
import Brain
import Cli


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

      drawGameFinder args_replay Nothing $ \ss -> do
        out ["DEBUG HERE"]
        return ()

    False -> do

      out [ "Starting Vindinium bot, training:", tshow ds_training,
            "Tag", tpack ds_tag
          ]

      driver_net (Key "vhkdc75e") args_ds $ do

        controller_threaded warmupIO $ \bs g hid chan -> do

          moveIO bs g hid chan

          when (not ds_quiet) $ do
            clearTerminal
            out [ "Tag:", "'" <> tpack ds_tag <> "'" ]
            out [ "Hero:", g.>gameHeroes.(idx hid).heroName, "(" <> printHero hid <> ")" ]
            blankLine
            out [ drawGame g [] ]
            out [ printHeroStats g ]

