module StrategyPage
  ( createFRP
  ) where

import Prelude

import Common.CellsWidget as CellsWidget
import Common.TreeWidget as TreeWidget
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP as FRP
import Proto.SseStatus (SseStatus)

createPageStrategyTree :: Effect Unit
createPageStrategyTree = do
  treeWidget <- TreeWidget.createFRP "strategy" "[data-tree-root][data-owner-type='strategy']"
  cellsWidget <- CellsWidget.createFRP
    { ownerType: "strategy"
    , detailTitlePrefix: ""
    }
  _ <- FRP.subscribe treeWidget.onNodeSelected \node ->
    cellsWidget.loadByOwnerIdPush (Just node.payload.ownerId)
  _ <- FRP.subscribe cellsWidget.onMutatedSubs \_ ->
      treeWidget.refreshPush
  pure unit

createFRP :: FRP.Event Unit -> FRP.Event SseStatus -> Effect Unit
createFRP initEvent _notifyEvent = do
  _ <- FRP.subscribeWithLog initEvent "[strategy:init]" \_ ->
    createPageStrategyTree
  pure unit
