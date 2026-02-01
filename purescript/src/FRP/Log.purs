module FRP.Log
  ( pushWithLog
  , subscribeWithLog
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import FRP.Event (Event, subscribe)

pushWithLog :: (Unit -> Effect Unit) -> String -> Effect Unit
pushWithLog push label = do
  log label
  push unit

subscribeWithLog :: forall a. Show a => Event a -> String -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribeWithLog event label handler =
  subscribe event \a -> do
    log (label <> " " <> show a)
    handler a
