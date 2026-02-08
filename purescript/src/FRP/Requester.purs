module FRP.Requester
  ( createColdWarmRequester
  , createRequester
  ) where

import Affjax.ResponseFormat as RF

import Affjax.Web as AX
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (drop, take)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, create, subscribe)
import FRP.Log (pushWithLog, subscribeWithLog)
import Prelude
import Proto.SseStatus (SseStatus(..))

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
  => String
  -> (Json -> Maybe resp)
  -> Event SseStatus
  -> Effect
       { requestPush :: Unit -> Effect Unit
       , responseEvent :: Event (Either String resp)
       }
createColdWarmRequester route decode notifyEvent = do
  { event: requestEvent, push: requestPush } <- create
  { event: responseEvent, push: responsePush } <- create

  let logRoute = if take 1 route == "/" then drop 1 route else route

  _ <- subscribeWithLog requestEvent ("[" <> logRoute <> "] request") \_ -> do
    launchAff_ do
      res <- AX.get RF.json route
      let
        resp =
          case res of
            Left err -> Left ("Request error: " <> AX.printError err)
            Right r ->
              case decode r.body of
                Nothing -> Left "Decode error"
                Just v -> Right v
      liftEffect $ pushWithLog responsePush ("[" <> logRoute <> "] response") resp

  _ <- subscribeWithLog notifyEvent ("[" <> logRoute <> "] notify") \status ->
    case status of
      Success -> pushWithLog requestPush ("[" <> logRoute <> "] request") unit
      _ -> pure unit

  pure { requestPush: requestPush , responseEvent }
