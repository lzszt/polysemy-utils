{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Effects.Ntfy
  ( Notification (..),
    Ntfy (..),
    notify,
    runNtfy,
    fakeNtfy,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Network.HTTP.Client.Conduit
  ( RequestBody (RequestBodyBS),
  )
import Network.HTTP.Simple
import Network.HTTP.Types.Header (HeaderName)
import Polysemy

data Notification = Notification
  { notificationTitle :: Maybe String,
    notificationTag :: Maybe String,
    notificationActions :: Maybe String,
    notificationClick :: Maybe String,
    notificationTopic :: String,
    notificationMessage :: String
  }
  deriving (Show)

data Ntfy m a where
  Notify :: Notification -> Ntfy m ()

makeSem ''Ntfy

maybeSetHeader :: HeaderName -> (a -> [ByteString]) -> Maybe a -> Request -> Request
maybeSetHeader headerName f = maybe id (setRequestHeader headerName . f)

runNtfy :: (Members '[Embed IO] r) => String -> Sem (Ntfy : r) a -> Sem r a
runNtfy ntfyBaseUrl = interpret $ \case
  Notify Notification {..} -> do
    let req =
          maybeSetHeader "Title" (pure . Char8.pack) notificationTitle $
            maybeSetHeader "Tags" (pure . Char8.pack) notificationTag $
              maybeSetHeader "Actions" (pure . Char8.pack) notificationActions $
                maybeSetHeader "Click" (pure . Char8.pack) notificationClick $
                  setRequestBody (RequestBodyBS (Char8.pack notificationMessage)) $
                    parseRequestThrow_ $
                      "POST " <> ntfyBaseUrl <> "/" <> notificationTopic

    _ <- httpNoBody req
    pure ()

fakeNtfy :: Sem (Ntfy : r) a -> Sem r a
fakeNtfy = interpret $ \(Notify _) -> pure ()