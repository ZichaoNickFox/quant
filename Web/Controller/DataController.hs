module Web.Controller.DataController where

import           Control.Concurrent.Async
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Generated.Types
import           IHP.Controller.Param
import           IHP.ControllerPrelude hiding (Symbol)
import           IHP.Fetch
import           IHP.Log
import           IHP.QueryBuilder
import           Prelude
import           Service.SymbolService
import           Web.Types
import           Web.View.Data.DataView

instance Controller DataController where
  action DataAction =do
    let mbSelectedSymbolCode = paramOrNothing @Text "symbolCode"
    symbolsByType <- getSymbolsFromLocal 
    render DataView { symbolsByType, mbSelectedSymbolCode }
  
  action DataActionGetSymbols = do
    let mbSelectedSymbolType = paramOrNothing @SymbolType "symbolType"
    updateSymbolsToLocal (fromJust mbSelectedSymbolType)
    redirectTo DataAction