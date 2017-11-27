{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import System.Exit

import Imports
import Types
import Driver
import Args
import Brain
import Cli

argsParser :: Parser Args
argsParser = Args
  <$> option str (long "tag" <> value "")
  <*> (read <$> option str (long "training" <> short 't' <> value "0"))
  <*> switch (long "dump-state")
  <*> switch (long "dump-game")
  <*> switch (long "quiet" <> short 'q')

getArgs = execParser (info (argsParser <**> helper) idm)

main :: IO ()
main = do

  args@Args{..} <- getArgs

  out [ "Starting Vindinium bot, training:", tshow args_training,
        "Tag", tpack args_tag
      ]

  driver_net (Key "vhkdc75e") args $ do

    controller_threaded warmupIO $ \bs g hid chan -> do

      moveIO bs g hid chan

      when (not args_quiet) $ do
        clearTerminal
        out [ "Tag:", "'" <> tpack args_tag <> "'" ]
        out [ "Hero:", g.>gameHeroes.(idx hid).heroName, "(" <> tshow hid <> ")" ]
        blankLine
        out [ drawGame g [] ]
        out [ printHeroStats g ]

