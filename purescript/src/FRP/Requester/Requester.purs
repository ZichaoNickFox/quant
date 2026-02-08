module FRP.Requester.Requester
  ( createFRP
  ) where

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, create)
import FRP.Log (pushWithLog, subscribeWithLog)
import Prelude

createFRP
  :: forall req resp
   . Show req
  => Show resp
  => String
  -> String
  -> (req -> Aff resp)
  -> Effect
       { requestEvent :: Event req
       , requestPush :: req -> Effect Unit
       , responseEvent :: Event resp
       }
createFRP requestLabel responseLabel request = do
  { event: requestEvent, push: requestPush } <- create
  { event: responseEvent, push: responsePush } <- create

  _ <- subscribeWithLog requestEvent requestLabel \req -> do
    launchAff_ do
      resp <- request req
      liftEffect $ pushWithLog responsePush responseLabel resp

  pure { requestEvent, requestPush, responseEvent }
