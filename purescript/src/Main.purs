module Main where

import Prelude

import FRP as FRP
import Data (combineDataFRP)
import Effect (Effect)
import Effect.Console (log)
import FFI.SSE (attachEventSource)
import FRP.Event (Event, create)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as HE
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  { event: domEvent, push: domPush } <- create
  { event: frpEvent, push: frpPush } <- create
  let beginEvent = combineBeginEvent domEvent frpEvent
  _ <- FRP.subscribeWithLog beginEvent "[FRP][event][begin]" \_ -> pure unit
  win <- window
  doc <- document win
  bindDomEvent doc domPush
  notifyEvent <- bindNotifyEvent
  combineDataFRP beginEvent notifyEvent
  FRP.pushWithLog frpPush "[FRP][push][frp]"

bindDomEvent :: HTMLDoc.HTMLDocument -> (Unit -> Effect Unit) -> Effect Unit
bindDomEvent doc domPush = do
  onInit <- eventListener (\_ -> do
    FRP.pushWithLog domPush "[FRP][push][dom]"
    )
  addEventListener HE.domcontentloaded onInit false (HTMLDoc.toEventTarget doc)
  pure unit

bindNotifyEvent :: Effect (Event Unit)
bindNotifyEvent = do
  { event, push } <- create
  attachEventSource "/sse/notify" (\_ -> push unit)
  pure event

bindRuntimeEvent :: Effect (Event Unit)
bindRuntimeEvent = do
  { event, push } <- create
  attachEventSource "/sse/runtime" (\_ -> push unit)
  pure event

combineBeginEvent :: Event Unit -> Event Unit -> Event Unit
combineBeginEvent domEvent frpEvent =
  FRP.take1 $
    FRP.map (const unit) $
      FRP.combineLatest2 domEvent frpEvent
