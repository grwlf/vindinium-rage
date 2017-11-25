-- | Moving Average
module MA where

import Imports

-- | Cumulative moving average
data CMA num = CMA {
    cma_curr :: num
  , cma_n :: Integer
  } deriving(Show, Read)

initialCMA :: (Fractional num) => CMA num
initialCMA = CMA 0 0

singletonCMA :: (Num num) => num -> CMA num
singletonCMA x = CMA x 1

current :: (Fractional s) => CMA s -> s
current (CMA c n) = c

meldCMA :: forall s . (Fractional s) => s -> CMA s -> CMA s
meldCMA s (CMA c n) = CMA (c + (s-c)/(fromInteger (n+1))) (n + 1)

combineCMA :: (Fractional num) => CMA num -> CMA num -> CMA num
combineCMA a@(CMA v 1) b = meldCMA v b
combineCMA a b@(CMA v 1) = meldCMA v a
combineCMA _ _ = error "combineCMA: Only defined for singletons"

-- testCMA :: Double
testCMA x = do
  -- fromRational $ do
  (current *** (\l -> sum l / (fromInteger $ toInteger $ length l))) $ do
  fst $ flip runRnd (mkStdGen 0) $ do
  flip execStateT (initialCMA :: CMA Double, ([] :: [Double])) $ do
  forM_ [0..x] $ \i -> do
    -- r <- getRandomR (1,9)
    modify $ (meldCMA (fromInteger i)) *** (fromInteger i:)


data SMA num = SMA {
    sma_curr :: num
  , sma_n :: Integer
  , sma_cnt :: Integer
  } deriving(Show,Read)


singletonSMA :: (Fractional num) => Integer -> num -> SMA num
singletonSMA n x = SMA x n 1

meldSMA :: (Fractional num) => num -> SMA num -> SMA num
meldSMA x (SMA c n cnt) = SMA ((((fromInteger n)-1)*c + x)/(fromInteger n)) n ((cnt+1)`min`n)

fullSMA :: SMA num -> Bool
fullSMA SMA{..} = sma_cnt == sma_n

currentSMA :: SMA num -> num
currentSMA SMA{..} = sma_curr

