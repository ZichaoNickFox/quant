module FRP.Component.TextArea
  ( autoResize
  , autoResizeDeferred
  , createFRP
  , TextAreaConfig
  , TextAreaEvents
  , TextAreaHandle
  ) where

import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Prelude
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, setAttribute, toEventTarget)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML.HTMLTextAreaElement (HTMLTextAreaElement)
import Web.HTML.HTMLTextAreaElement as HTMLTextArea

foreign import autoResize :: HTMLTextAreaElement -> Effect Unit
foreign import autoResizeDeferred :: HTMLTextAreaElement -> Effect Unit

type TextAreaEvents =
  { onInit :: Effect Unit
  , onInput :: Effect Unit
  }

type TextAreaConfig =
  { name :: String
  , className :: String
  , rows :: Int
  , value :: String
  , events :: Maybe TextAreaEvents
  }

type TextAreaHandle =
  { element :: Element
  , init :: Effect Unit
  }

createFRP :: Document -> TextAreaConfig -> Effect TextAreaHandle
createFRP doc cfg = do
  el <- createElement "textarea" doc
  let events = fromMaybe { onInit: pure unit, onInput: pure unit } cfg.events
  setAttribute "name" cfg.name el
  setAttribute "class" cfg.className el
  setAttribute "rows" (show cfg.rows) el
  setAttribute "style" "overflow:hidden; resize:none;" el
  case HTMLTextArea.fromElement el of
    Just textarea -> do
      HTMLTextArea.setValue cfg.value textarea
      listener <- eventListener \_ -> do
        autoResize textarea
        events.onInput
      addEventListener (EventType "input") listener false (toEventTarget el)
      let init = do
            autoResize textarea
            autoResizeDeferred textarea
            events.onInit
      pure { element: el, init }
    Nothing ->
      pure { element: el, init: events.onInit }
