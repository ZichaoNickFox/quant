module Web.Controller.DataController where

import           Control.Concurrent.Async
import           Data.API
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
import           Web.Types
import           Web.View.Data.DataView

instance Controller DataController where
  action DataAction = autoRefresh do
    let symbolTypes = [minBound .. maxBound] :: [SymbolType]
        mbSelectedSymbolCode = paramOrNothing @Text "symbolCode"
    symbolsByType <- M.fromList <$> do
      forM symbolTypes $ \symbolType -> do
        symbols <- query @Symbol
          |> filterWhere (#symbolType, symbolType)
          |> fetch
        pure (symbolType, symbols)
    render DataView { symbolsByType, mbSelectedSymbolCode }
  
  action DataActionGetSymbols = do
    let mbSelectedSymbolType = paramOrNothing @SymbolType "symbolType"
    async $ do
      symbols <- getSymbols (fromJust mbSelectedSymbolType)
      createMany symbols
    redirectTo DataAction