module PageNote
  ( createPageNoteTree
  , initFRP
  ) where

import Prelude

import Common.CellsWidget as CellsWidget
import Common.TreeWidget as TreeWidget
import Common.TreeWidget (createTreeWidgetByOwnerType)
import Effect (Effect)
import FRP as FRP
import Proto.SseStatus (SseStatus)

createPageNoteTree :: Effect Unit
createPageNoteTree = do
  treeWidget <- createTreeWidgetByOwnerType "note" "[data-tree-root][data-owner-type='note']"
  cellsWidget <- CellsWidget.createCellsWidgetByOwnerType
    { ownerType: "note"
    , detailTitlePrefix: "笔记详情 - "
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
  _ <- FRP.subscribeWithLog initEvent "[note:init]" \_ ->
    createPageNoteTree
  pure unit
