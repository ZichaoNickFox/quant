module Web.Controller.PageController where

import           Control.Concurrent.Async
import           Control.Monad (void)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Web.Prelude
import           Web.Repository.SymbolRepository
import           Web.Types
import           Web.View.DataView
import qualified Web.View.Backtest.Index as BacktestView
import qualified Web.View.Note.Index as NoteView
import qualified Web.View.Strategy.Index as StrategyView
import           Web.View.RuntimeView

instance Controller PageController where
  action PageDataAction = do
    logController Info $ "[PageController][PageDataAction] " <> requestUrl
    let mbSelectedSymbol = SelectedSymbol
          <$> paramOrNothing @SymbolType "symbolType"
          <*> paramOrNothing @Text "symbolCode"
    typeSymbolsMap <- getTypeSymbolsMapFromDB
    render DataView { typeSymbolsMap, mbSelectedSymbol }
  action PageNoteAction = do
    render NoteView.IndexView
  action PageStrategyAction = do
    render StrategyView.IndexView
  action PageBacktestAction = do
    render BacktestView.IndexView
  action PageRuntimeAction = do
    render RuntimeView
