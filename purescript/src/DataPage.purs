module DataPage (createFRP) where

import Prelude

import Data.Array (filter, length, mapMaybe, null)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP as FRP
import Proto.SseStatus (SseStatus)
import Proto.Symbols (APISymbolsResponse(..), SymbolInfo(..))
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element, fromNode, getAttribute, setAttribute, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

setLoading :: Array Element -> Effect Unit
setLoading = traverse_ \el -> setTextContent "..." (toNode el)

renderSymbolCounts :: Array SymbolInfo -> Array Element -> Effect Unit
renderSymbolCounts symbols =
  traverse_ \el -> do
    mType <- getAttribute "data-symbol-type" el
    case mType of
      Nothing -> pure unit
      Just st -> do
        let val = length (filter (\(SymbolInfo s) -> s.symbolType == st) symbols)
        setTextContent (st <> ": " <> show val) (toNode el)

renderSymbolList :: HTMLDoc.HTMLDocument -> Array SymbolInfo -> Effect Unit
renderSymbolList htmlDoc symbols = do
  mList <- querySelector (QuerySelector "#symbol-list") (HTMLDoc.toParentNode htmlDoc)
  case mList of
    Nothing -> pure unit
    Just listEl -> do
      setTextContent "" (toNode listEl)
      let doc' = HTMLDoc.toDocument htmlDoc
      traverse_ (appendOption doc' listEl) symbols
  where
    appendOption doc listEl (SymbolInfo s) = do
      opt <- createElement "option" doc
      setAttribute "value" (s.symbolType <> "|" <> s.code) opt
      setTextContent s.name (toNode opt)
      appendChild (toNode opt) (toNode listEl)

-- FRP wiring: build refresh events -> responses behavior -> render
createFRP :: FRP.Event Unit -> FRP.Event SseStatus -> Effect Unit
createFRP initEvent notifyEvent = do
  win <- window
  doc <- document win
  nodes <- querySelectorAll (QuerySelector "[data-frp-symbol-count]") (HTMLDoc.toParentNode doc) >>= toArray
  let els = mapMaybe fromNode nodes
  when (not (null els)) do
    { requestPush: pushApiSymbolsRequest, responseEvent: apiSymbolsResponses } <-
      FRP.createColdWarmRequester
        "/APISymbols"
        notifyEvent

    _ <- FRP.subscribeWithLog apiSymbolsResponses "[APISymbols] Response" \resp -> do
      case resp of
        Left _ -> pure unit
        Right (APISymbolsResponse r) -> do
          renderSymbolCounts r.symbols els
          renderSymbolList doc r.symbols

    _ <- FRP.subscribeWithLog initEvent "[init]" \_ -> do
      setLoading els
      FRP.pushWithLog pushApiSymbolsRequest "[APISymbols] request" unit

    -- NOTE: init event handles first render (DOMContentLoaded/ihp:afterRender)
    pure unit
