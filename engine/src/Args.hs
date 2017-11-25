{-# LANGUAGE RecordWildCards #-}
module Args where

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
