module Web.Controller.NotifyController where

import           Control.Applicative ((<|>))
import           Control.Exception (finally)
import           Control.Concurrent.STM (atomically, readTChan)
import           Data.ByteString.Builder (byteString)
import qualified Data.Text.Encoding as TE
import           Network.HTTP.Types (hCacheControl, hConnection, hContentType, status200)
import           Network.Wai (responseStream)
import           System.Timeout (timeout)
import           IHP.Controller.Response (respondAndExit)
import           Web.Prelude
import           Web.Service.NotifyHub
import           Web.Types

instance Controller NotifyController where
  action NotifyAction = do
    let clientId =
          fromMaybe "default" $
            paramOrNothing @Text "clientId"
              <|> paramOrNothing @Text "client"
    let headers =
          [ (hContentType, "text/event-stream")
          , (hCacheControl, "no-cache")
          , (hConnection, "keep-alive")
          ]
    respondAndExit $ responseStream status200 headers $ \send flush -> do
      chan <- registerClient clientId
      let sendEvent payload = send (byteString ("data: " <> payload <> "\n\n"))
          sendPing = send (byteString ": ping\n\n")
          loop = forever $ do
            message <- timeout (30 * 1000000) (atomically $ readTChan chan)
            case message of
              Just payload -> do
                sendEvent payload
                flush
              Nothing -> do
                sendPing
                flush
      sendEvent (TE.encodeUtf8 ("connected" :: Text))
      flush
      loop `finally` unregisterClient clientId
