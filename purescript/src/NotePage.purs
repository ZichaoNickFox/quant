module NotePage
  ( createFRP
  ) where

import Prelude

import Common.CellsWidget as CellsWidget
import Common.TreeWidget as TreeWidget
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP as FRP
import Proto.SseStatus (SseStatus)

createPageNoteTree :: Effect Unit
createPageNoteTree = do
  treeWidget <- TreeWidget.createFRP "note" "[data-tree-root][data-owner-type='note']"
  cellsWidget <- CellsWidget.createFRP
    { ownerType: "note"
    , detailTitlePrefix: "笔记详情 - "
    }
  _ <- FRP.subscribe treeWidget.onNodeSelected \node ->
    cellsWidget.loadByOwnerIdPush (Just node.payload.ownerId)
  _ <- FRP.subscribe cellsWidget.onMutatedSubs \_ ->
    treeWidget.refreshPush
  pure unit

createFRP :: FRP.Event Unit -> FRP.Event SseStatus -> Effect Unit
createFRP initEvent _notifyEvent = do
  _ <- FRP.subscribeWithLog initEvent "[note:init]" \_ ->
    createPageNoteTree
  pure unit
