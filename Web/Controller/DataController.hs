module Web.Controller.DataController where

import           Control.Concurrent.Async
import           Control.Monad (void)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Web.Prelude
import           Web.Repository.SymbolRepository
import           Web.Types
import           Web.View.DataView

instance Controller DataController where
  action DataAction = do
    logController Info $ "[DataController][DataAction] " <> requestUrl
    let mbSelectedSymbol = SelectedSymbol
          <$> paramOrNothing @SymbolType "symbolType"
          <*> paramOrNothing @Text "symbolCode"
    typeSymbolsMap <- getTypeSymbolsMapFromDB 
    render DataView { typeSymbolsMap, mbSelectedSymbol }
