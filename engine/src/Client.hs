{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Client where

import Network.HTTP.Client
import Network.HTTP.Types

import qualified Data.Map as Map
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringL
import qualified Data.Aeson as Aeson
import qualified Control.Lens as Lens
import Control.Monad.Rnd

import Imports
import Types

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

class (MonadIO m, MonadReader Settings m) => Client m

instance (MonadIO m) => Client (ReaderT Settings m)
instance (Client m) => Client (StateT s m)
instance (Client m) => Client (Break r m)
instance (Client m) => Client (ContT x m)

request :: (Client m) => Text -> Aeson.Value -> m ServerState
request url val =
  let
    decodeBody body = either err id (Aeson.eitherDecodeStrict $ ByteStringL.toStrict body) where
      err e = error $ "request: unable to decode state: " ++ e

    injectKey (Aeson.Object a) k =
      let
        (Aeson.Object b) = Aeson.object [("key", toJSON k)]
      in
        Aeson.Object (a <> b)
  in do
  Settings{..} <- ask
  initReq <- liftIO $ parseUrl $ tunpack url
  let req = initReq
              { method = "POST"
              , requestHeaders =
                  [ (hContentType, "application/json")
                  , (hAccept,      "application/json")
                  , (hUserAgent,   "vindinium-bot")
                  ]
              , requestBody = (RequestBodyLBS . Aeson.encode) (injectKey val settingsKey)
              , responseTimeout = responseTimeoutNone
              }
  liftIO $ withManager defaultManagerSettings $ \mgr -> do
    bs <- responseBody <$> httpLbs req mgr
    st' <- pure $ decodeBody bs
    return st'

startTraining :: (Client m) => Maybe Integer -> Maybe Board -> m ServerState
startTraining mi mb = do
    url <- startUrl "training"
    request url (Aeson.object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                       <> maybe [] (\b -> [("map",  toJSON b)]) mb))

applyMove :: (Client m) => ServerState -> Dir -> m ServerState
applyMove s d = request (s^.statePlayUrl) (Aeson.object [("dir", toJSON d)])

startArena :: (Client m) => m ServerState
startArena = do
    url <- startUrl "arena"
    request url (Aeson.object [])

startUrl :: (Client m) => Text -> m Text
startUrl v = liftM (\x -> x <> "/api/" <> v) $ asks settingsUrl

