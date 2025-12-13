module Web.View.Data.SelectSymbolHtml where

import qualified Data.Map as M
import           Data.Maybe
import           Data.Text hiding (concat)
import           Generated.Types
import           IHP.ViewPrelude hiding (concat, Symbol)
import           Web.Routes
import           Web.Types

selectSymbolHtml :: Map SymbolType [Symbol] -> Html
selectSymbolHtml symbolsByType =
  [hsx|
    <div style="display: flex; align-items: center; gap: 12px;">
      <form method="get" action={DataAction}>
        <input
          type="text"
          name="symbolCode"
          list="symbol-list"
          placeholder="输入代码或选择"
          onfocus="this.value='';"
        />

        <datalist id="symbol-list">
          {forEach symbols renderSymbolOption}
        </datalist>

        <button type="submit">Select</button>
      </form>
    </div>
  |]
  where symbols = mconcat $ M.elems symbolsByType

renderSymbolOption :: Symbol -> Html
renderSymbolOption (Symbol {name, code}) = 
  [hsx| <option value={code}>{name}</option> |]