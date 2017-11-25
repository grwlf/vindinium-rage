{-# LANGUAGE OverloadedStrings #-}
module Main where

import Types
import Driver2
import Args
import Brain

main = do
  args <- getArgs
  driver_net (Key "vhkdc75e") args (
    controller_simple warmupIO moveIO)

