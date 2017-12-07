module Bot.Random where

import Types
import Imports
import Driver
import Monad
import System.Random (Random(..), randomIO)

init _ = return ()

step _ _ =
  toEnum <$> randomRIO (fromEnum (minBound :: Dir),
                        fromEnum (maxBound :: Dir))

run :: (MonadBot m, MonadIO m) => m ()
run = controller_simple Bot.Random.init Bot.Random.step
