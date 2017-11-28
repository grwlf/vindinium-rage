{-# LANGUAGE Rank2Types #-}
-- | The import module selectively re-exports almost all functions used by the
-- project
module Imports (
    module Control.Arrow
  , module Control.Applicative
  , module Control.Concurrent
  , module Control.Concurrent.STM
  , module Control.Concurrent.STM.TChan
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.State.Strict
  , module Control.Monad.Reader
  , module Control.Monad.Writer.Strict
  , module Control.Monad.Identity
  , module Control.Monad.Cont
  , module Control.Lens
  , module Control.DeepSeq
  , module Data.Bits
  , module Data.Ratio
  , module Data.Tuple
  , module Data.Binary
  , module Data.List
  , module Data.Map.Strict
  , module Data.Maybe
  , module Data.Hashable
  , module Data.Monoid
  , module Data.Set
  , module Data.Function
  , module Data.Foldable
  , module Data.Text
  , module Data.HashSet
  , module Data.HashMap.Strict
  , module Data.Aeson
  , module Data.Time
  , module Data.Time.Clock
  , module Data.Time.Calendar
  , module Data.PQueue.Prio.Min
  , module Data.PQueue.Prio.Max
  , module Debug.Trace
  , module Prelude
  , module System.FilePath
  , module System.Directory
  , module Text.Printf
  , module Text.Show.Pretty
  , module GHC.Generics
  , module Imports
)

where

import Control.Arrow ((&&&),(***))
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM (STM(..), atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan, tryReadTChan, newTChanIO)
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict (MonadWriter(..), tell, execWriter)
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Lens (Lens', Lens, makeLenses, (<%=), (%=), (%%=), (^.), zoom, set, view, over, use, uses, _1, _2, _3, _4, _5, _6)
import Control.DeepSeq
import Data.Bits
import Data.Ratio
import Data.Tuple
import Data.List hiding (break)
import Data.Map.Strict (Map, (!))
import Data.Set (Set,member)
import Data.HashSet (HashSet)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.Binary hiding (get,put)
import Data.Hashable
import Data.Monoid ((<>))
import Data.Foldable
import Data.Function
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time(diffUTCTime,getCurrentTime,UTCTime,NominalDiffTime)
import Data.Aeson(FromJSON(..),ToJSON(..),(.:),(.:?),(.=))
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Max (MaxPQueue)
import Debug.Trace hiding(traceM)
import Prelude hiding(break,print)
import System.Directory (removeDirectoryRecursive,createDirectoryIfMissing,renameFile,getDirectoryContents)
import System.FilePath
import Text.Printf
import Text.Show.Pretty hiding(String)
import GHC.Generics (Generic)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens
import qualified Control.Lens.Getter

{- Debug -}

trace1 :: (Show a) => a -> a
trace1 a = trace (ppShow a) a

trace1S :: (Show a) => String -> a -> a
trace1S str a = trace (str <> " " <> ppShow a) a

traceM :: (Monad m, Show a) => a -> m ()
traceM a = trace (ppShow a) (return ())

trace' :: (Show a) => a -> b -> b
trace' a b = trace (ppShow a) b

assert :: (Monad m, Show x) => x -> Bool -> m ()
assert x b = if not b then error (show x) else return ()


{- Text -}

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

rshow :: (Real a) => String -> a -> Text
rshow mask r = tpack $ printf mask ((fromRational $ toRational r) :: Double)

rshow_ :: (Real a) => a -> Text
rshow_ r = tpack $ printf "%-2.1f" ((fromRational $ toRational r) :: Double)

tpack :: String -> Text
tpack = Text.pack

tunpack :: Text -> String
tunpack = Text.unpack

{- Containers -}

hset1 :: (Hashable a) => a -> HashSet a
hset1 = HashSet.singleton

hmap1 :: (Hashable k) => k -> a -> HashMap k a
hmap1 k a = HashMap.singleton k a

ilength :: (Foldable t) => t a -> Integer
ilength l = toInteger $ length l

nlength :: (Num n, Foldable t) => t a -> n
nlength l = fromInteger $ toInteger $ length l

{- Lenses -}

(.>) :: s -> Control.Lens.Getter.Getting a s a -> a
(.>) = (^.)
infixl 8 .>

idx name = Lens.lens get set where
  get m = case HashMap.lookup name m of
            Just x -> x
            Nothing -> error $ "Key " <> show name <> " not found in map"
  set = (\hs mhv -> HashMap.insert name mhv hs)

{- Time -}

diffTimeFrom :: (MonadIO m) => UTCTime -> m NominalDiffTime
diffTimeFrom tstart = do
  t <- liftIO $ getCurrentTime
  return $ diffUTCTime t tstart

{- Control -}

whileM_ :: (Monad m) => m Bool -> m ()
whileM_ m = do
  x <- m
  case x of
    True -> whileM_ m
    False -> return ()

whileM :: (Monad m) => m (Maybe x) -> m x
whileM m = do
  mb_x <- m
  case mb_x of
    Nothing -> whileM m
    Just x -> return x

