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

dateString :: IO String
dateString = do
  t <- getCurrentTime
  let (y, m, d) = toGregorian (utctDay t)
  return $ printf "%d-%02d-%02d-%s" y m d (show $ utctDayTime t)

dumpGame :: (MonadIO m) => String -> GameState -> m FilePath
dumpGame tag gs = liftIO $ do
  let nmove = (gs.>stateGame.gameTurn `div` 4)
  let gid = gameid (gs.>stateGame.gameId)
  let hid = gs.>stateHero.heroId
  let gn = printf "game_%s.%s-%d" tag (Text.unpack gid) (hid_int hid)
  let f = "data" </> gn </> (printf "%04d.json" nmove)
  createDirectoryIfMissing True ("data" </> gn)
  ByteString.writeFile (f++".tmp") (Aeson.encode (gs^.stateJSON))
  renameFile (f++".tmp") f
  return ("data" </> gn)

removeGame :: (MonadIO m) => String -> GameId -> HeroId -> m ()
removeGame tag gid hid = liftIO $ do
  let gn = printf "game_%s.%s-%d" tag
            (Text.unpack $ gameid gid)
            (hid_int hid)
  removeDirectoryRecursive ("data" </> gn)

dumpState :: (MonadIO m) => String -> GameId -> HeroId -> Integer -> GameState -> m ()
dumpState tag GameId{..} hid nmove ss = liftIO $ do
  let gn = printf "state_%s.%s-%d" tag (Text.unpack gameid) (hid_int hid)
  let f = "data" </> gn </> (printf "%04d.json" nmove)
  createDirectoryIfMissing True ("data" </> gn)
  ByteString.writeFile (f++".tmp") (Aeson.encode (ss^.stateJSON))
  renameFile (f++".tmp") f

loadState :: (MonadIO m) => FilePath -> m GameState
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

findMaps :: (MonadIO m) => m [FilePath]
findMaps = liftIO $ do
  let d = "data"
  map (</> "0000.json") <$> map (d </>) <$> filter ("game"`isPrefixOf`) <$> getDirectoryContents d



