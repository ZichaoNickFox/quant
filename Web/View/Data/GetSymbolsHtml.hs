module Web.View.Data.GetSymbolsHtml where

import Data.Map
import Generated.Types
import IHP.ViewPrelude hiding (Symbol, (!))
import Web.Routes
import Web.Types

getSymbolsHtml :: Map SymbolType [Symbol] -> Html
getSymbolsHtml symbolsByType = [hsx|
    <div style="display: flex; align-items: center; gap: 12px;">
      {forEach (keys symbolsByType) symbolsOfType}
    </div>
  |]
  where
    symbolsOfType :: SymbolType -> Html
    symbolsOfType symbolType =
      let symbols = symbolsByType ! symbolType
      in  [hsx|
            <span>{show symbolType}: {length symbols}</span>

            <form method="get" action={DataActionGetSymbols}>
              <input type="hidden" name="symbolType" value={inputValue symbolType} />
              <button type="submit">Update</button>
            </form>
          |]