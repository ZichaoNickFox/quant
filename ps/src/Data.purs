module Data (runDataFRP) where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as RF
import Control.Monad (when)
import Data.Argonaut as A
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import Web.DOM.Document (Document)
import Web.DOM.ParentNode (querySelectorAll)
import Web.DOM.Element (Element, getAttribute)
import Web.DOM.NodeList (toArray)
import Web.DOM.Node (setTextContent)
import Web.Event.EventTarget (addEventListener)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as E
import Web.HTML.Window (document)
import Proto.Symbols (SymbolCountsResponse(..))

-- decoding
instance decodeSymbolsResponse :: DecodeJson SymbolCountsResponse where
  decodeJson v = do
    obj <- A.decodeJson v
    complete <- obj A..: "complete"
    countsVal <- obj A..: "counts"
    pure $ SymbolCountsResponse { complete, counts: countsVal }

fetchSymbols :: Boolean -> Aff SymbolCountsResponse
fetchSymbols skipCheck = do
  let qs = if skipCheck then "?skipCheck=true" else ""
  res <- AX.get { url: "/api/symbols" <> qs, responseFormat: RF.json }
  case decodeJson res.body of
    Left err -> do liftEffect $ log $ "decode symbols error: " <> show err
                   pure $ SymbolCountsResponse { complete: false, counts: Map.empty }
    Right v  -> pure v

setLoading :: Array Element -> Effect Unit
setLoading = traverse_ \el -> setTextContent "..." el

updateSymbolCounts :: SymbolCountsResponse -> Element -> Effect Unit
updateSymbolCounts (SymbolCountsResponse r) el = do
  mType <- getAttribute "data-symbol-type" el
  case mType of
    Nothing -> pure unit
    Just st -> do
      let val = fromMaybe 0 (Map.lookup st r.counts)
      setTextContent (st <> ": " <> show val) el

renderSymbols :: SymbolCountsResponse -> Array Element -> Effect Unit
renderSymbols resp els = traverse_ (updateSymbolCounts resp) els

-- FRP wiring: build refresh events -> responses behavior -> render
runDataFRP :: Effect Unit
runDataFRP = do
  win <- window
  doc <- document win
  els <- querySelectorAll "[data-frp-symbol-count]" doc >>= toArray
  when (not (null els)) do
    setLoading els
    -- refresh requests bus
    { event: refreshes, push: pushRefresh } <- create
    -- responses bus
    { event: responses, push: pushResponse } <- create

    -- keep latest response as a behavior (could be sampled elsewhere)
    let _resp :: Behavior SymbolCountsResponse
        _resp = step (SymbolCountsResponse { complete: false, counts: Map.empty }) responses

    -- when a refresh is requested, fetch then push response
    _ <- subscribe refreshes \skipCheck ->
      launchAff_ do
        resp <- fetchSymbols skipCheck
        liftEffect do
          pushResponse resp
          renderSymbols resp els

    -- initial kick
    pushRefresh false

    -- websocket notify to re-fetch (skip cache check)
    let proto = if window.location.protocol == "https:" then "wss:" else "ws:"
        url = proto <> "//" <> window.location.host <> "/ws/notify"
    ws <- WS.new url []
    addEventListener message (\_ -> pushRefresh true) false ws

    -- re-run on DOMContentLoaded (ihp partial renders)
    addEventListener E.domContentLoaded (const $ pushRefresh false) false doc
