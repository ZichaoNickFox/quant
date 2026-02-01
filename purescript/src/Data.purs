module Data (runDataFRP) where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as RF
import Data.Argonaut.Decode (decodeJson)
import Data.Array (mapMaybe, null)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as FO
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import FRP.Event (Event, create, subscribe)
import Web.DOM.ParentNode (QuerySelector(..), querySelectorAll)
import Web.DOM.Element (Element, fromNode, getAttribute, toNode)
import Web.DOM.NodeList (toArray)
import Web.DOM.Node (setTextContent)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument as HTMLDoc
import Proto.Symbols (SymbolCountsResponse(..), SymbolCountsResponseJson(..))

fetchSymbols :: Boolean -> Aff SymbolCountsResponse
fetchSymbols skipCheck = do
  let qs = if skipCheck then "?skipCheck=true" else ""
  res <- AX.get RF.json ("/api/symbols" <> qs)
  case res of
    Left err -> do
      liftEffect $ log $ "fetch symbols error: " <> AX.printError err
      pure $ SymbolCountsResponse { complete: false, counts: FO.empty }
    Right r ->
      case decodeJson r.body of
        Left err -> do liftEffect $ log $ "decode symbols error: " <> show err
                       pure $ SymbolCountsResponse { complete: false, counts: FO.empty }
        Right (SymbolCountsResponseJson v) -> pure v

setLoading :: Array Element -> Effect Unit
setLoading = traverse_ \el -> setTextContent "..." (toNode el)

updateSymbolCounts :: SymbolCountsResponse -> Element -> Effect Unit
updateSymbolCounts (SymbolCountsResponse r) el = do
  mType <- getAttribute "data-symbol-type" el
  case mType of
    Nothing -> pure unit
    Just st -> do
      let val = fromMaybe 0 (FO.lookup st r.counts)
      setTextContent (st <> ": " <> show val) (toNode el)

renderSymbols :: SymbolCountsResponse -> Array Element -> Effect Unit
renderSymbols resp els = traverse_ (updateSymbolCounts resp) els

-- FRP wiring: build refresh events -> responses behavior -> render
runDataFRP :: Event Unit -> Event Unit -> Effect Unit
runDataFRP initEvent wsEvent = do
  win <- window
  doc <- document win
  nodes <- querySelectorAll (QuerySelector "[data-frp-symbol-count]") (HTMLDoc.toParentNode doc) >>= toArray
  let els = mapMaybe fromNode nodes
  when (not (null els)) do
    -- refresh requests bus
    { event: refreshes, push: pushRefresh } <- create
    -- responses bus
    { event: _, push: pushResponse } <- create

    -- when a refresh is requested, fetch then push response
    _ <- subscribe refreshes \skipCheck ->
      launchAff_ do
        resp <- fetchSymbols skipCheck
        liftEffect do
          pushResponse resp
          renderSymbols resp els

    _ <- subscribe initEvent \_ -> do
      setLoading els
      pushRefresh false
    
    -- websocket notify to re-fetch (skip cache check)
    _ <- subscribe wsEvent \_ -> pushRefresh true

    -- NOTE: init event handles first render (DOMContentLoaded/ihp:afterRender)
    pure unit
