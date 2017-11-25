{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}
module Perf where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

import qualified Data.Aeson as Aeson

import System.CPUTime.Rdtsc
import System.IO.Unsafe
import System.IO
import Data.IORef
import Data.Word
import Data.Ratio
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import GHC.IO (evaluate)
import MA

{-
-- | Simple moving average
data SMA num = SMA {
    avg_curr :: num
  , avg_n :: Integer
  } deriving(Show, Read)

initialSMA :: (Fractional num) => SMA num
initialSMA = SMA 0 0

singletonSMA :: (Num num) => num -> SMA num
singletonSMA x = SMA x 1

current :: (Fractional s) => SMA s -> s
current (SMA c n) = c

meld :: (Fractional s) => SMA s -> s -> SMA s
meld (SMA c n) s = SMA (c + (s-c)/(fromInteger (n+1))) (n + 1)

combineSMA :: (Fractional num) => SMA num -> SMA num -> SMA num
combineSMA a@(SMA v 1) b = meld b v
combineSMA a b@(SMA v 1) = meld a v
combineSMA _ _ = error "combineSMA: Only defined for singletons for now"
-}

type PerfCnt = Word64

perfcnt :: (MonadIO m) => m PerfCnt
perfcnt = liftIO rdtsc

data Perf = Perf {
    perfCMA :: !(CMA Rational)
  , perfMax :: !(Word64)
} deriving(Show)

combinePerf p1 p2 = Perf (combineCMA (perfCMA p1) (perfCMA p2)) (max (perfMax p1) (perfMax p2))

type PerfMap = Map String Perf

{-# NOINLINE thePerfMap #-}
thePerfMap :: IORef PerfMap
thePerfMap = unsafePerformIO $ do
  newIORef (Map.empty)

counter :: (MonadIO m) => String -> PerfCnt -> m ()
counter nm val = do
  let perf = Perf (singletonCMA (fromInteger $ toInteger val)) val
  liftIO $ atomicModifyIORef' thePerfMap $ (Map.insertWith combinePerf nm perf) &&& const 0
  return ()

create :: (MonadIO m) => String -> PerfCnt -> PerfCnt -> m ()
create nm sta sto = do
  let dt = sto - sta
  counter nm dt
  return ()

set :: (MonadIO m) => String -> m a -> m a
set nm m = do
  t1 <- perfcnt
  a <- (liftIO . evaluate) =<< m
  t2 <- perfcnt
  create nm t1 t2
  return a

set_ :: (MonadIO m) => String -> m a -> m ()
set_ nm m = set nm m >> return ()

print :: (MonadIO m) => m ()
print = liftIO $ do
  pm <- readIORef thePerfMap
  forM_ (Map.toList pm) $ \(nm,Perf{..}) -> do
    putStrLn $ nm ++ ": Average " ++ show (round $ (current perfCMA) :: Integer) ++ " Entries " ++ show (cma_n perfCMA)
    putStrLn $ nm ++ ": Maximum " ++ show perfMax
    putStrLn ""

report :: (MonadIO m) => m String
report = liftIO $ do
  pm <- readIORef thePerfMap
  unlines <$> do
  execWriterT $ do
  forM_ (Map.toList pm) $ \(nm,Perf{..}) -> do
    tell [ nm ++ ": Average " ++ show (round $ (current perfCMA) :: Integer) ++ " Entries " ++ show (cma_n perfCMA) ]
    tell [ nm ++ ": Maximum " ++ show perfMax ]
    tell [ "" ]

