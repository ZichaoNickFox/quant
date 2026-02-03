module FRP.Log
  ( pushWithLog
  , pushWithLogUnit
  , subscribeWithLog
  ) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import FRP.Event (Event, subscribe)

pushWithLogUnit :: (Unit -> Effect Unit) -> String -> Effect Unit
pushWithLogUnit push label = do
  log ("[push]" <> label)
  push unit

pushWithLog :: forall a. Show a => (a -> Effect Unit) -> String -> a -> Effect Unit
pushWithLog push label value = do
  log ("[push]" <> label <> " " <> show value)
  push value

subscribeWithLog :: forall a. Show a => Event a -> String -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribeWithLog event label handler =
  subscribe event \a -> do
    log ("[subs]" <> label <> " " <> show a)
    handler a
