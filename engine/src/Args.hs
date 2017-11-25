{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Args where

import Development.GitRev
import Options.Applicative
import System.Exit
import Imports

-- | Command-line arguments
data Args = Args {
    args_tag :: String
  -- ^ Game tag, really a git revision
  , args_training :: Integer
  , args_no_dump_state :: Bool
  , args_dump_game :: Bool
  , args_dump_perf :: Bool
  , args_quiet :: Bool
  , args_version :: Bool
  } deriving (Show, Eq)


argsParser :: Parser Args
argsParser = Args
  <$> option str (long "tag" <> value "")
  <*> (read <$> option str (long "training" <> short 't' <> value "0"))
  <*> switch (long "no-dump-state")
  <*> switch (long "dump-game")
  <*> switch (long "dump-perf")
  <*> switch (long "quiet" <> short 'q')
  <*> switch (long "version" <> short 'v')


fixTag a@Args{..}
  | args_tag == "" = a { args_tag = (printf "%04d" (read $(gitCommitCount)::Int)) <> "_" <> (take 7 $(gitHash)) <> (if $(gitDirty) then "@" else "") }
  | otherwise = a

getArgs = fixTag <$> execParser (info (argsParser <**> helper) idm)
