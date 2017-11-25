module Sigint where

import System.Exit
import System.Posix.Signals
import Control.Concurrent
import qualified Control.Exception as E

installSigint :: ExitCode -> IO ()
installSigint ec = do
  tid <- myThreadId
  installHandler keyboardSignal (Catch (E.throwTo tid ec)) Nothing
  return ()
