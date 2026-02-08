module Main where

import Data.Argonaut.Parser (jsonParser)

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import DataPage as DataPage
import Effect (Effect)
import FFI.SSE (attachEventSource)
import FRP as FRP
import Prelude
import Proto.SseStatus (SseStatus, decodeMaybeSseStatus)
import StrategyPage as StrategyPage
import Web.DOM.Element (getAttribute)
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
    mountCurrentPage beginEvent notifyEvent
    FRP.pushWithLog beginPush "[begin]" unit
    pure unit
  _ <- FRP.subscribeWithLog beginEvent "[begin]" \_ -> do
    pure unit

  win <- window
  doc <- document win
  bindDomEvent doc domPush
  pure unit

mountCurrentPage :: FRP.Event Unit -> FRP.Event SseStatus -> Effect Unit
mountCurrentPage beginEvent notifyEvent = do
  win <- window
  doc <- document win
  mRoot <- querySelector (QuerySelector "#app-root[data-app-page]") (HTMLDoc.toParentNode doc)
  case mRoot of
    Nothing -> pure unit
    Just root -> do
      mPage <- getAttribute "data-app-page" root
      case mPage of
        Just "data" -> DataPage.createFRP beginEvent notifyEvent
        Just "strategy" -> StrategyPage.createFRP beginEvent notifyEvent
        Just "backtest" -> DataPage.renderBacktestShell
        Just "runtime" -> DataPage.renderRuntimeShell
        _ -> pure unit

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
        case decodeMaybeSseStatus json of
          Just status -> push status
          Nothing -> pure unit
  pure event
