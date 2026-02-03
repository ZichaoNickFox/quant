module FRP.Requester
  ( createRequester
  , createColdWarmRequester
  ) where

import Prelude

import Affjax.ResponseFormat as RF
import Affjax.Web as AX
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, create, subscribe)
import FRP.Log (pushWithLog, subscribeWithLog)

createRequester
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
createRequester requestLabel responseLabel request = do
  { event: requestEvent, push: requestPush } <- create
  { event: responseEvent, push: responsePush } <- create

  _ <- subscribeWithLog requestEvent requestLabel \req -> do
    launchAff_ do
      resp <- request req
      liftEffect $ pushWithLog responsePush responseLabel resp

  pure { requestEvent, requestPush, responseEvent }

createColdWarmRequester
  :: forall resp
   . Show resp
  => DecodeJson resp
  => String
  -> Event Unit
  -> Effect
       { requestPush :: Unit -> Effect Unit
       , responseEvent :: Event (Either String resp)
       }
createColdWarmRequester route notifyEvent = do
  { event: requestEvent, push: requestPush } <- create
  { event: responseEvent, push: responsePush } <- create

  _ <- subscribeWithLog requestEvent ("[" <> route <> "] request") \_ -> do
    launchAff_ do
      res <- AX.get RF.json route
      let
        resp =
          case res of
            Left err -> Left ("Request error: " <> AX.printError err)
            Right r ->
              case decodeJson r.body of
                Left err -> Left ("decode error: " <> show err)
                Right v -> Right v
      liftEffect $ pushWithLog responsePush ("[" <> route <> "] response=") resp

  _ <- subscribe notifyEvent \_ ->
    requestPush unit

  pure { requestPush, responseEvent }
