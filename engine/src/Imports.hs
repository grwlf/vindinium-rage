{-# LANGUAGE Rank2Types #-}
module Imports (
    module Control.Arrow
  , module Control.Applicative
  , module Control.Concurrent
  , module Control.Concurrent.STM
  , module Control.Exception
  , module Control.Monad
  , module Control.Monad.Trans
  , module Control.Monad.State.Strict
  , module Control.Monad.Reader
  , module Control.Monad.Writer.Strict
  , module Control.Monad.Identity
  , module Control.Monad.Cont
  , module Control.Monad.Rnd
  , module Control.Break
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
  , module Data.Time.Clock
  , module Data.Time.Calendar
  , module Data.PQueue.Prio.Min
  , module Data.PQueue.Prio.Max
  , module Debug.Trace
  , module Prelude
  , module System.Random
  , module System.Random.Mersenne.Pure64
  , module System.Directory
  , module System.FilePath
  , module Text.Printf
  , module Text.Heredoc
  , module Text.Show.Pretty
  , module GHC.Generics
  , module Imports
)

where

import Control.Arrow ((&&&),(***))
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (assert)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Writer.Strict (MonadWriter(..), tell, execWriter)
import Control.Monad.Cont
import Control.Monad.Identity
import Control.Monad.Rnd
import Control.Break
import Control.Lens (Lens', Lens, makeLenses, (<%=), (%=), (%%=), (^.), zoom, set, view, use, uses, _1, _2, _3, _4, _5, _6)
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
import Data.Aeson(FromJSON(..),ToJSON(..),(.:),(.:?),(.=))
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Max (MaxPQueue)
import Debug.Trace hiding(traceM)
import Prelude hiding(break,print)
import System.Random
import System.Random.Mersenne.Pure64
import System.Directory
import System.FilePath
import Text.Printf
import Text.Heredoc
import Text.Show.Pretty hiding(String)
import GHC.Generics (Generic)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Control.Lens as Lens
import qualified Control.Lens.Getter

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

trace1 :: (Show a) => a -> a
trace1 a = trace (ppShow a) a

traceM :: (Monad m, Show a) => a -> m ()
traceM a = trace (ppShow a) (return ())

trace' :: (Show a) => a -> b -> b
trace' a b = trace (ppShow a) b

tpack :: String -> Text
tpack = Text.pack
tunpack :: Text -> String
tunpack = Text.unpack


assert :: (Monad m, Show x) => x -> Bool -> m ()
assert x b = if not b then error (show x) else return ()

(.>) :: s -> Control.Lens.Getter.Getting a s a -> a
(.>) = (^.)
infixl 8 .>

hset1 :: (Hashable a) => a -> HashSet a
hset1 = HashSet.singleton

hmap1 :: (Hashable k) => k -> a -> HashMap k a
hmap1 k a = HashMap.singleton k a

ilength :: (Foldable t) => t a -> Integer
ilength l = toInteger $ length l

nlength :: (Num n, Foldable t) => t a -> n
nlength l = fromInteger $ toInteger $ length l

rshow :: (Real a) => a -> Text
rshow r = tpack $ printf "%-2.1f" ((fromRational $ toRational r) :: Double)

idx name = Lens.lens get set where
  get m = case HashMap.lookup name m of
            Just x -> x
            Nothing -> error $ "Key " <> show name <> " not found in map"
  set = (\hs mhv -> HashMap.insert name mhv hs)

