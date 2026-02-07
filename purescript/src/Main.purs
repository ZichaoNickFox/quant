module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Effect (Effect)
import FFI.SSE (attachEventSource)
import FRP as FRP
import DataPage as DataPage
import NotePage as NotePage
import StrategyPage as StrategyPage
import Proto.SseStatus (SseStatus(..), sseStatusFromString)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as HE
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  -- event
  { event: domEvent, push: domPush } <- FRP.create
  { event: beginEvent, push: beginPush } <- FRP.create

  -- subscribe
  _ <- FRP.subscribeWithLog domEvent "[dom]" \_ -> do
    notifyEvent <- bindNotifyEvent
    DataPage.createFRP beginEvent notifyEvent
    win <- window
    doc <- document win
    hasNoteTree <- hasSelector doc "[data-tree-root][data-owner-type='note']"
    when hasNoteTree do
      NotePage.createFRP beginEvent notifyEvent
    hasStrategyTree <- hasSelector doc "[data-tree-root][data-owner-type='strategy']"
    when hasStrategyTree do
      StrategyPage.createFRP beginEvent notifyEvent
    FRP.pushWithLog beginPush "[begin]" unit
    pure unit
  _ <- FRP.subscribeWithLog beginEvent "[begin]" \_ -> do
    pure unit

  win <- window
  doc <- document win
  bindDomEvent doc domPush
  pure unit

bindDomEvent :: HTMLDoc.HTMLDocument -> (Unit -> Effect Unit) -> Effect Unit
bindDomEvent doc domPush = do
  onInit <- eventListener (\_ -> do
    FRP.pushWithLog domPush "[dom]" unit
    )
  addEventListener HE.domcontentloaded onInit false (HTMLDoc.toEventTarget doc)
  pure unit

bindNotifyEvent :: Effect (FRP.Event SseStatus)
bindNotifyEvent = do
  { event, push } <- FRP.create
  attachEventSource "/sse/notify" \raw ->
    case jsonParser raw of
      Left _ -> pure unit
      Right json ->
        case (decodeJson json :: Either _ { status :: String, reason :: Maybe String }) of
          Right payload ->
            case sseStatusFromString payload.status of
              Just (Failed _) -> push (Failed (fromMaybe "" payload.reason))
              Just v -> push v
              Nothing -> pure unit
          Left _ -> pure unit
  pure event

hasSelector :: HTMLDoc.HTMLDocument -> String -> Effect Boolean
hasSelector doc selector = do
  mNode <- querySelector (QuerySelector selector) (HTMLDoc.toParentNode doc)
  pure case mNode of
    Just _ -> true
    Nothing -> false
