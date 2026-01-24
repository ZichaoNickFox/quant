module Web.View.Data.SelectSymbolHtml where

import qualified Data.Map as M
import           Data.Maybe
import           Data.Text hiding (concat)
import           Web.Prelude
import           Web.Types

selectSymbolHtml :: TypeSymbolsMap -> Html
selectSymbolHtml typeSymbolsMap =
  [hsx|
    <div style="display: flex; align-items: center; gap: 12px;">
      <form method="get" action={DataAction}>
        <input type="text" list="symbol-list" oninput="
            const v = this.value;
            const i = v.indexOf('|');
            if (i > 0) {
              this.form.symbolType.value = v.slice(0, i);
              this.form.symbolCode.value = v.slice(i + 1);
            }
          " onfocus="this.value='';" />
        <input type="hidden" name="symbolType" />
        <input type="hidden" name="symbolCode" />
        <datalist id="symbol-list">
          {forEach symbols renderSymbolOption}
        </datalist>
        <button type="submit">Select</button>
      </form>
    </div>
  |]
  where symbols = concat $ M.elems typeSymbolsMap

renderSymbolOption :: Symbol -> Html
renderSymbolOption symbol = 
  [hsx| <option value={inputValue (symbol.symbolType :: SymbolType) <> "|" <> symbol.code}>{symbol.name}</option> |]