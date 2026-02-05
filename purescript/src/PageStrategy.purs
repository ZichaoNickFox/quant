module PageStrategy
  ( createPageStrategyTree
  , initFRP
  ) where

import Prelude

import Common.CellsWidget as CellsWidget
import Common.TreeWidget as TreeWidget
import Common.TreeWidget (createTreeWidgetByOwnerType)
import Effect (Effect)
import FRP as FRP
import Proto.SseStatus (SseStatus)

createPageStrategyTree :: Effect Unit
createPageStrategyTree = do
  treeWidget <- createTreeWidgetByOwnerType "strategy" "[data-tree-root][data-owner-type='strategy']"
  cellsWidget <- CellsWidget.createCellsWidgetByOwnerType
    { ownerType: "strategy"
    , detailTitlePrefix: "策略详情 - "
    }
  _ <- FRP.subscribe treeWidget.onTreeUpdate \node ->
    cellsWidget.actionPush (CellsWidget.LoadByTreeNode node)
  _ <- FRP.subscribe cellsWidget.onCellsUpdate \cellsUpdate ->
    case cellsUpdate of
      CellsWidget.CellsMutated ->
        treeWidget.actionPush TreeWidget.RefreshTree
      CellsWidget.CellsRendered ->
        pure unit
  pure unit

initFRP :: FRP.Event Unit -> FRP.Event SseStatus -> Effect Unit
initFRP initEvent _notifyEvent = do
  _ <- FRP.subscribeWithLog initEvent "[strategy:init]" \_ ->
    createPageStrategyTree
  pure unit
