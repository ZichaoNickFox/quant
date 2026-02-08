module FRP.Requester.ColdWarmRequester
  ( createFRP
  ) where

import Affjax.ResponseFormat as RF
import Affjax.Web as AX
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (drop, take)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, create)
import FRP.Log (pushWithLog, subscribeWithLog)
import Prelude
import Proto.SseStatus (SseStatus(..))

createFRP
  :: forall resp
   . Show resp
  => String
  -> (Json -> Maybe resp)
  -> Event SseStatus
  -> Effect
       { requestPush :: Unit -> Effect Unit
       , responseEvent :: Event (Either String resp)
       }
createFRP route decode notifyEvent = do
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

  pure { requestPush, responseEvent }
