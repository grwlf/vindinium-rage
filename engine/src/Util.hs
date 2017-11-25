{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util where

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.IO
import Imports
import Types
import Perf

dateString :: IO String
dateString = do
  t <- getCurrentTime
  let (y, m, d) = toGregorian (utctDay t)
  return $ printf "%d-%02d-%02d-%s" y m d (show $ utctDayTime t)

dumpGame :: (MonadIO m) => String -> GameId -> Integer -> ServerState -> m ()
dumpGame tag GameId{..} nmove cs = liftIO $ do
  let gn = printf "game_%s.%s" (Text.unpack gameid) tag
  let f = "data" </> gn </> (printf "%03d.json" nmove)
  createDirectoryIfMissing False ("data" </> gn)
  ByteString.writeFile (f++".tmp") (Aeson.encode (cs^.stateJSON))
  renameFile (f++".tmp") f

dumpState :: (MonadIO m) => String -> GameId -> Integer -> ServerState -> m ()
dumpState tag GameId{..} nmove cs = liftIO $ do
  let gn = printf "state_%s.%s" (Text.unpack gameid) tag
  let f = "data" </> gn </> (printf "%03d.json" nmove)
  createDirectoryIfMissing False ("data" </> gn)
  ByteString.writeFile (f++".tmp") (Aeson.encode (cs^.stateJSON))
  renameFile (f++".tmp") f

dumpPerf :: (MonadIO m) => String -> GameId -> m ()
dumpPerf tag GameId{..} = liftIO $ do
  let gn = printf "perf_%s.%s" (Text.unpack gameid) tag
  let f = "data" </> gn </> "perf.txt"
  createDirectoryIfMissing False ("data" </> gn)
  perftext <- Perf.report
  writeFile (f++".tmp") perftext
  renameFile (f++".tmp") f

loadState :: (MonadIO m) => FilePath -> m ServerState
loadState f = liftIO $ do
  -- let f = "data" </> "hstates" </> nm
  fromMaybe (error $ "Faield to decode state " ++ f) <$> Aeson.decode <$> ByteString.readFile f

save :: (MonadIO m, Show a) => String -> a -> m ()
save nm a = liftIO $ do
  let f = "data" </> (printf "%s.txt" nm)
  writeFile (f++".tmp") (ppShow a)
  renameFile (f++".tmp") f

loadDef :: (MonadIO m, Read a) => String -> a -> m a
loadDef nm def = liftIO $ do
  let f = "data" </> (printf "%s.txt" nm)
  either (\(_::SomeException) -> def) read <$> (try $ readFile f)

load :: (MonadIO m, Read a) => String -> m a
load nm = loadDef nm (error $ "Utils.load: failed to load " <> nm)


saveBin :: (MonadIO m, Binary a) => String -> a -> m ()
saveBin nm a = liftIO $ do
  let f = "data" </> (printf "%s.bin" nm)
  ByteString.writeFile (f++".tmp") (Binary.encode a)
  renameFile (f++".tmp") f

loadBinDef :: (MonadIO m, Binary a) => String -> a -> m a
loadBinDef nm def = liftIO $ do
  let f = "data" </> (printf "%s.bin" nm)
  either (\(_::SomeException) -> def) decode <$> (try $ ByteString.readFile f)

loadBin :: (MonadIO m, Binary a) => String -> m a
loadBin nm = loadBinDef nm (error $ "Utils.loadBin: failed to load " <> nm)



out :: (MonadIO m) => [Text] -> m ()
out args = do
  liftIO $ do
    Text.putStrLn (Text.concat args)

err :: (MonadIO m) => [Text] -> m ()
err args = do
  liftIO $ do
    Text.hPutStrLn stderr (Text.concat args)

clearTerminal :: (MonadIO m) => m ()
clearTerminal = liftIO $ do
  putStrLn "\027[2J"
  putStrLn "\027[1;1H"



findMaps :: (MonadIO m) => m [FilePath]
findMaps = liftIO $ do
  let d = "data"
  map (</> "000.json") <$> map (d </>) <$> filter ("game"`isPrefixOf`) <$> getDirectoryContents d


listMaps :: Maybe [FilePath] -> IO ()
listMaps mgs = do
  hSetBuffering stdin NoBuffering
  let d = "data"
  gs <-
    case mgs of
      Just x -> return x
      Nothing -> do
        map (</> "000.json") <$> map (d </>) <$> filter ("game"`isPrefixOf`) <$> getDirectoryContents d
  let imax = length gs

  flip evalStateT (0::Int) $ do
    forever $ do
      i <- get
      i' <- liftIO getChar >>= return . \case
        'j' -> (i+1)`min`(imax-1)
        'k' -> (i-1)`max`0
        _ -> i
      put i'

      let fn = gs !! i'
      ss <- loadState fn
      clearTerminal
      out [ tpack fn <> " " <> tshow i' <> " of " <> tshow imax ]
      out [printGame (ss.>stateGame)]


estimate1Sec :: (MonadIO m) => m (MVar PerfCnt)
estimate1Sec = liftIO $ do
  ans <- newEmptyMVar
  forkIO $ do
    forever $ do
      t1 <- perfcnt
      threadDelay (10^6)
      t2 <- perfcnt
      Perf.create "sleep1" t1 t2
      tryPutMVar ans (t2-t1)
  return ans

data AppendHandle = AppendHandle FilePath Handle

appendHandle :: (MonadIO m) => FilePath -> m AppendHandle
appendHandle f =
  AppendHandle ("data"</>f) <$> (liftIO $ openFile ("data"</>f) AppendMode)

append :: (MonadIO m) => AppendHandle -> Text -> m ()
append (AppendHandle f h) t = liftIO $ hPutStrLn h (tunpack t) >> hFlush h


evalRndM :: (MonadIO m) => RndT PureMT m a -> m a
evalRndM m = liftIO newPureMT >>= evalRndT m

