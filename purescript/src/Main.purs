module Main where

import Prelude

import Effect (Effect)
import Data (runDataFRP)
import FRP.Event (Event, create)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (Window, document, location)
import Web.HTML.Location as Location
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Event.EventTypes as HE
import Web.Socket.WebSocket as WS
import Web.Socket.Event.EventTypes as WSE

main :: Effect Unit
main = do
  win <- window
  doc <- document win
  initEvent <- initRenderEvent doc
  wsEvent <- wsNotifyEvent win
  runDataFRP initEvent wsEvent

initRenderEvent :: HTMLDoc.HTMLDocument -> Effect (Event Unit)
initRenderEvent doc = do
  { event, push } <- create
  onInit <- eventListener (\_ -> push unit)
  addEventListener HE.domcontentloaded onInit false (HTMLDoc.toEventTarget doc)
  addEventListener (EventType "ihp:afterRender") onInit false (HTMLDoc.toEventTarget doc)
  pure event

wsNotifyEvent :: Window -> Effect (Event Unit)
wsNotifyEvent win = do
  { event, push } <- create
  loc <- location win
  proto <- Location.protocol loc
  host <- Location.host loc
  let wsProto = if proto == "https:" then "wss:" else "ws:"
      url = wsProto <> "//" <> host <> "/ws/notify"
  ws <- WS.create url []
  onMsg <- eventListener (\_ -> push unit)
  addEventListener WSE.onMessage onMsg false (WS.toEventTarget ws)
  pure event
