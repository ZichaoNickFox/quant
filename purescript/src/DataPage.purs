module DataPage
  ( createFRP
  , renderBacktestShell
  , renderRuntimeShell
  ) where

import Data.Array (filter, length, mapMaybe, null)

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FFI.DOM (setInnerHTMLById)
import FRP as FRP
import FRP.Requester.ColdWarmRequester as ColdWarmRequester
import Prelude
import Proto.SseStatus (SseStatus)
import Proto.Symbols (APISymbolsResponse(..), SymbolInfo(..), decodeMaybeAPISymbolsResponse)
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element, fromNode, getAttribute, setAttribute, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

renderDataShell :: Effect Unit
renderDataShell = do
  _ <- setInnerHTMLById "app-root" """
    <div data-data-page='1'>
      <div style='display: flex; align-items: center; gap: 12px;'>
        <span data-frp-symbol-count data-symbol-type='stock' data-placeholder='...'>Stock: 0</span>
        <span data-frp-symbol-count data-symbol-type='fund' data-placeholder='...'>Fund: 0</span>
        <span data-frp-symbol-count data-symbol-type='index' data-placeholder='...'>Index: 0</span>
      </div>
      <div style='display: flex; align-items: center; gap: 12px;'>
        <div data-symbol-picker='1'>
          <input type='text' list='symbol-list' oninput="
            const v = this.value;
            const i = v.indexOf('|');
            const root = this.closest('[data-symbol-picker]');
            if (i > 0) {
              root.querySelector('[name=symbolType]').value = v.slice(0, i);
              root.querySelector('[name=symbolCode]').value = v.slice(i + 1);
            }
          " onfocus='this.value=&quot;&quot;;' />
          <input type='hidden' name='symbolType' />
          <input type='hidden' name='symbolCode' />
          <datalist id='symbol-list'></datalist>
          <button type='button'>Favorite</button>
        </div>
      </div>
      <div id='lw-chart' style='height: 320px; border: 1px dashed #2e8b57; border-radius: 6px; margin-top: 12px;'>
        <div style='padding: 8px; color: #2e8b57;'>lightweight-charts placeholder</div>
      </div>
    </div>
  """
  pure unit

renderBacktestShell :: Effect Unit
renderBacktestShell = do
  _ <- setInnerHTMLById "app-root" """
    <div class='container mt-3'>
      <h3>回测中心</h3>
      <p>选择策略、设置参数、运行回测、展示收益图。</p>
    </div>
  """
  pure unit

renderRuntimeShell :: Effect Unit
renderRuntimeShell = do
  _ <- setInnerHTMLById "app-root" """
    <div class='container mt-3'>
      <h2>Runtime</h2>
      <p>Runtime page.</p>
    </div>
  """
  pure unit

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
  renderDataShell
  win <- window
  doc <- document win
  nodes <- querySelectorAll (QuerySelector "[data-frp-symbol-count]") (HTMLDoc.toParentNode doc) >>= toArray
  let els = mapMaybe fromNode nodes
  when (not (null els)) do
    { requestPush: pushApiSymbolsRequest, responseEvent: apiSymbolsResponses } <-
      ColdWarmRequester.createFRP
        "/APISymbols"
        decodeMaybeAPISymbolsResponse
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
