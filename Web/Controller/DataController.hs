module Web.Controller.DataController where

import           Control.Concurrent.Async
import           Control.Monad (void)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Prelude
import           Web.Prelude
import           Web.Service.SymbolService
import           Web.Types
import           Web.View.DataView

instance Controller DataController where
  action DataAction = autoRefresh do
    let mbSelectedSymbol = SelectedSymbol
          <$> paramOrNothing @SymbolType "symbolType"
          <*> paramOrNothing @Text "symbolCode"
    logController Info $ "[DataController][DataAction] " <> tshow mbSelectedSymbol 
    typeSymbolsMap <- getTypeSymbolsMapFromDB 
    render DataView { typeSymbolsMap, mbSelectedSymbol }
  
  action DataActionGetSymbols = do
    logController Info "[DataController][DataActionGetSymbols]"
    let mbSelectedSymbolType = paramOrNothing @SymbolType "symbolType"
    updateSymbolToDB (fromJust mbSelectedSymbolType)
    redirectTo DataAction