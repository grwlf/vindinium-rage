{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import System.Exit
import Imports

import Types
import Driver
import Args
import Brain

argsParser :: Parser Args
argsParser = Args
  <$> option str (long "tag" <> value "")
  <*> (read <$> option str (long "training" <> short 't' <> value "0"))
  <*> switch (long "dump-state")
  <*> switch (long "dump-game")
  <*> switch (long "dump-perf")
  <*> switch (long "quiet" <> short 'q')
  <*> switch (long "version" <> short 'v')

getArgs = execParser (info (argsParser <**> helper) idm)

main = do
  args <- getArgs
  driver_net (Key "vhkdc75e") args (
    controller_simple warmupIO moveIO)

