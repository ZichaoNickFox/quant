module FRP.Log
  ( pushWithLog
  , subscribeWithLog
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import FRP.Event (Event, subscribe)

pushWithLog :: forall a. Show a => (a -> Effect Unit) -> String -> a -> Effect Unit
pushWithLog push label value = do
  log ("[p]" <> label <> " p = " <> show value)
  push value

subscribeWithLog :: forall a. Show a => Event a -> String -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribeWithLog event label handler =
  subscribe event \a -> do
    log ("[s]" <> label <> " p = " <> show a)
    handler a
