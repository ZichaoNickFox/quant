module Web.View.DataView where

import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Text hiding (length)
import           Web.Prelude
import           Web.View.DebugHtml
import           Web.Types

data DataView = DataView
  { typeSymbolsMap :: TypeSymbolsMap
  , mbSelectedSymbol :: Maybe SelectedSymbol}

instance View DataView where
  html DataView { typeSymbolsMap, mbSelectedSymbol } =
    [hsx|
      <div style="display: flex; align-items: center; gap: 12px;">
        {forEach (M.keys typeSymbolsMap) symbolsOfType}
      </div>
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
      {maybe mempty renderSelected mbSelectedSymbol}
    |]
    where
      renderSelected (SelectedSymbol st code) =
        [hsx|<div class="mt-3">Selected: {show st} {code}</div>|]
      symbolsOfType :: SymbolType -> Html
      symbolsOfType symbolType =
        let symbols = typeSymbolsMap M.! symbolType
        in  [hsx|
              <span>{show symbolType}: {length symbols}</span>
              <form method="get" action={DataGetSymbolsAction}>
                <input type="hidden" name="symbolType" value={inputValue symbolType} />
                <button type="submit">Update</button>
              </form>
            |]
      symbols = Web.Prelude.concat $ M.elems typeSymbolsMap
      renderSymbolOption :: Symbol -> Html
      renderSymbolOption symbol = 
        [hsx| <option value={inputValue (symbol.symbolType :: SymbolType) <> "|" <> symbol.code}>{symbol.name}</option> |]
