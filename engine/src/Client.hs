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

import Imports
import Types

data Settings = Settings {
    settingsKey :: Key
  , settingsUrl :: Text
} deriving (Show, Eq)

request :: (MonadIO m) => Settings -> Text -> Aeson.Value -> m ServerState
request Settings{..} url val =
  let
    decodeBody body = either err id (Aeson.eitherDecodeStrict $ ByteStringL.toStrict body) where
      err e = error $ "request: unable to decode state: " ++ e

    injectKey (Aeson.Object a) k =
      let
        (Aeson.Object b) = Aeson.object [("key", toJSON k)]
      in
        Aeson.Object (a <> b)
  in do
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

startTraining :: (MonadIO m) => Settings -> Maybe Integer -> Maybe Board -> m ServerState
startTraining ss mi mb = do
    url <- pure $ startUrl ss "training"
    request ss url (Aeson.object ( maybe [] (\i -> [("turns", toJSON i)]) mi
                       <> maybe [] (\b -> [("map",  toJSON b)]) mb))

applyMove :: (MonadIO m) => Settings -> ServerState -> Dir -> m ServerState
applyMove ss s d = request ss (s^.statePlayUrl) (Aeson.object [("dir", toJSON d)])

startArena :: (MonadIO m) => Settings -> m ServerState
startArena ss = do
    url <- pure $ startUrl ss "arena"
    request ss url (Aeson.object [])

startUrl :: Settings -> Text -> Text
startUrl ss v = (settingsUrl ss) <> "/api/" <> v

