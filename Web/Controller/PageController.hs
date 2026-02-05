module Web.Controller.PageController where

import           Control.Concurrent.Async
import           Control.Monad (void)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Web.Prelude
import           Web.Types
import           Web.View.BacktestView
import           Web.View.DataView
import           Web.View.NoteView
import           Web.View.RuntimeView
import           Web.View.StrategyView
import           Web.Repo.CellRepo (loadCells)
import           Web.Repo.PageRepo (getOrCreateNote, getOrCreateStrategy)

instance Controller PageController where
  action PageDataAction = do
    logController Info $ "[PageController][PageDataAction] " <> requestUrl
    let mbSelectedSymbol = SelectedSymbol
          <$> paramOrNothing @SymbolType "symbolType"
          <*> paramOrNothing @Text "symbolCode"
    render DataView { mbSelectedSymbol }

  action PageNoteAction = do
    note <- getOrCreateNote
    let (Id noteUuid) = get #id note
    noteCells <- loadCells CellOwnerTypeNote noteUuid
    render NoteView { noteId = get #id note, noteCells }

  action PageStrategyAction = do
    strategy <- getOrCreateStrategy
    let (Id strategyUuid) = get #id strategy
    strategyCells <- loadCells CellOwnerTypeStrategy strategyUuid
    render StrategyView { strategyId = get #id strategy, strategyCells }

  action PageBacktestAction = do
    render BacktestView

  action PageRuntimeAction = do
    render RuntimeView
