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

instance Controller PageController where
  action PageDataAction = do
    logController Info $ "[PageController][PageDataAction] " <> requestUrl
    let mbSelectedSymbol = SelectedSymbol
          <$> paramOrNothing @SymbolType "symbolType"
          <*> paramOrNothing @Text "symbolCode"
    render DataView { mbSelectedSymbol }

  action PageNoteAction = do
    render NoteView

  action PageStrategyAction = do
    render StrategyView

  action PageBacktestAction = do
    render BacktestView

  action PageRuntimeAction = do
    render RuntimeView
