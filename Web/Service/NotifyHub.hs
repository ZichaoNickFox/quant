{-# LANGUAGE OverloadedStrings #-}
module Web.Service.NotifyHub
  ( ClientId
  , registerClient
  , unregisterClient
  , publishToClient
  , publishToAll
  ) where

import           Control.Concurrent.STM
import qualified Data.ByteString as BS
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           IHP.Prelude
import           System.IO.Unsafe (unsafePerformIO)

type ClientId = Text

data Topic = Topic
  { topicChan :: TChan BS.ByteString
  , topicCount :: Int
  }

type Hub = TVar (Map ClientId Topic)

hub :: Hub
hub = unsafePerformIO (newTVarIO Map.empty)
{-# NOINLINE hub #-}

registerClient :: ClientId -> IO (TChan BS.ByteString)
registerClient clientId = atomically $ do
  topics <- readTVar hub
  case Map.lookup clientId topics of
    Just topic -> do
      dup <- dupTChan (topicChan topic)
      let updated = topic { topicCount = topicCount topic + 1 }
      writeTVar hub (Map.insert clientId updated topics)
      pure dup
    Nothing -> do
      chan <- newBroadcastTChan
      dup <- dupTChan chan
      writeTVar hub (Map.insert clientId (Topic chan 1) topics)
      pure dup

unregisterClient :: ClientId -> IO ()
unregisterClient clientId = atomically $ do
  topics <- readTVar hub
  case Map.lookup clientId topics of
    Nothing -> pure ()
    Just topic
      | topicCount topic <= 1 -> writeTVar hub (Map.delete clientId topics)
      | otherwise -> do
          let updated = topic { topicCount = topicCount topic - 1 }
          writeTVar hub (Map.insert clientId updated topics)

publishToClient :: ClientId -> BS.ByteString -> IO ()
publishToClient clientId payload = atomically $ do
  topics <- readTVar hub
  forM_ (Map.lookup clientId topics) $ \topic ->
    writeTChan (topicChan topic) payload

publishToAll :: BS.ByteString -> IO ()
publishToAll payload = atomically $ do
  topics <- readTVar hub
  forM_ (Map.elems topics) $ \topic ->
    writeTChan (topicChan topic) payload
