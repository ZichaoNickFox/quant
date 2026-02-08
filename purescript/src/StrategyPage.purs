module StrategyPage
  ( createFRP
  ) where

import Affjax.ResponseFormat as RF

import Affjax.Web as AX
import Common.CellsWidget as CellsWidget
import Common.TreeWidget as TreeWidget
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FFI.DOM (getQueryParam, setInnerHTMLById)
import FRP as FRP
import Prelude
import Proto.SseStatus (SseStatus)
import Proto.Strategy (StrategyInfo(..), decodeMaybeStrategyInfo)
import Web.DOM.Element (toNode)
import Web.DOM.Node (setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

requestStrategy :: String -> Aff (Maybe StrategyInfo)
requestStrategy url = do
  response <- AX.get RF.json url
  pure case response of
    Left _ -> Nothing
    Right ok -> decodeMaybeStrategyInfo ok.body

renderStrategyShell :: String -> String -> Effect Unit
renderStrategyShell strategyOwnerId strategyTitle = do
  _ <- setInnerHTMLById "app-root" ("""
    <div class='row'>
      <div class='col-3 border-end' style='border-right: 1px solid #ced4da !important;'>
        <div
          data-tree-root='1'
          data-owner-type='strategy'
          data-owner-id='""" <> strategyOwnerId <> """'
          style='min-height: 220px;'
        ></div>
      </div>
      <div class='col-9'>
        <h3 data-cell-title='1'>""" <> strategyTitle <> """</h3>
        <div data-cell-create='1' data-owner-type='strategy'>
          <input type='hidden' name='ownerType' value='strategy' />
          <input type='hidden' name='ownerId' value='""" <> strategyOwnerId <> """' data-cell-create-owner-id='1' />
        </div>
        <div data-cell-list='1'></div>
      </div>
    </div>
  """)
  pure unit

createPageStrategyTree :: Effect Unit
createPageStrategyTree = do
  treeWidget <- TreeWidget.createFRP "strategy" "[data-tree-root][data-owner-type='strategy']"
  cellsWidget <- CellsWidget.createFRP
    { ownerType: "strategy"
    }
  cellsWidget.loadByOwnerIdPush Nothing
  _ <- FRP.subscribe treeWidget.onNodeSelected \node -> do
    launchAff_ do
      mStrategy <- requestStrategy ("/StrategyRead?strategyId=" <> node.payload.ownerId)
      liftEffect $
        setTitle case mStrategy of
          Just (StrategyInfo strategy) -> strategy.name
          Nothing -> "策略"
    cellsWidget.loadByOwnerIdPush (Just node.payload.ownerId)
  _ <- FRP.subscribe cellsWidget.onMutatedSubs \_ ->
      treeWidget.refreshPush
  pure unit

setTitle :: String -> Effect Unit
setTitle title = do
  win <- window
  doc <- document win
  mTitle <- querySelector (QuerySelector "[data-cell-title='1']") (HTMLDoc.toParentNode doc)
  case mTitle of
    Nothing -> pure unit
    Just titleEl -> setTextContent title (toNode titleEl)

createFRP :: FRP.Event Unit -> FRP.Event SseStatus -> Effect Unit
createFRP initEvent _notifyEvent = do
  _ <- FRP.subscribeWithLog initEvent "[strategy:init]" \_ -> do
    mStrategyId <- getQueryParam "strategyId"
    launchAff_ do
      let readUrl = case mStrategyId of
            Nothing -> "/StrategyRead"
            Just strategyId -> "/StrategyRead?strategyId=" <> strategyId
      mStrategy <- requestStrategy readUrl
      let strategyOwnerId = case mStrategy of
            Just (StrategyInfo strategy) -> strategy.id
            Nothing -> ""
      liftEffect do
        renderStrategyShell strategyOwnerId "策略"
        createPageStrategyTree
  pure unit
