module Data (combineDataFRP) where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as RF
import Data.Argonaut.Decode (decodeJson)
import Data.Array (mapMaybe, length, null)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Foreign.Object as FO
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (log)
import FRP.Event (Event, create)
import FRP as FRP
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
  res <- AX.get RF.json ("/APISymbols" <> qs)
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
combineDataFRP :: Event Unit -> Event Unit -> Effect Unit
combineDataFRP initEvent wsEvent = do
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
    _ <- FRP.subscribeWithLog refreshes "[FRP][subs][refreshes] skipCheck=" \skipCheck -> do
      launchAff_ do
        resp <- fetchSymbols skipCheck
        liftEffect do
          FRP.pushWithLog (\_ -> pushResponse resp) "[FRP][push][response]"
          renderSymbols resp els

    _ <- FRP.subscribeWithLog initEvent "[FRP][subs][initEvent]" \_ -> do
      setLoading els
      FRP.pushWithLog (\_ -> pushRefresh false) "[FRP][push][refreshes] skipCheck=false"
    
    -- websocket notify to re-fetch (skip cache check)
    _ <- FRP.subscribeWithLog wsEvent "[FRP][subs][wsEvent]" \_ -> do
      FRP.pushWithLog (\_ -> pushRefresh true) "[FRP][push][refreshes] skipCheck=true"

    -- NOTE: init event handles first render (DOMContentLoaded/ihp:afterRender)
    pure unit
