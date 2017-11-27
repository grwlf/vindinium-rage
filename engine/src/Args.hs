{-# LANGUAGE RecordWildCards #-}
module Args where

-- | Command-line arguments
data Args = Args {
    args_tag :: String
  -- ^ Game tag, e.g. git revision
  , args_training :: Integer
  , args_dump_state :: Bool
  , args_dump_game :: Bool
  , args_quiet :: Bool
  } deriving (Show, Eq)
