module Web.Controller.DataController where

import           Control.Concurrent.Async
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Prelude
import           Web.Prelude
import           Web.Service.SymbolService
import           Web.Types
import           Web.View.Data.DataView

instance Controller DataController where
  action DataAction =do
    let mbSelectedSymbolCode = paramOrNothing @Text "symbolCode"
    symbolsByType <- getSymbolsFromDB 
    render DataView { symbolsByType, mbSelectedSymbolCode }
  
  action DataActionGetSymbols = do
    let mbSelectedSymbolType = paramOrNothing @SymbolType "symbolType"
    updateSymbolToDB (fromJust mbSelectedSymbolType)
    redirectTo DataAction