module Web.View.DataView where

import           Data.Maybe
import           Data.Text hiding (length)
import           Web.Prelude
import           Web.Types

data DataView = DataView
  { mbSelectedSymbol :: Maybe SelectedSymbol}

instance View DataView where
  html DataView { mbSelectedSymbol } =
    [hsx|
      <div style="display: flex; align-items: center; gap: 12px;">
        {forEach allSymbolTypes symbolsOfType}
      </div>
      <div style="display: flex; align-items: center; gap: 12px;">
        <form method="get" action={PageDataAction}>
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
          </datalist>
          <button type="submit">Select</button>
        </form>
      </div>
      {maybe mempty renderSelected mbSelectedSymbol}
    |]
    where
      renderSelected (SelectedSymbol st code) =
        [hsx|<div class="mt-3">Selected: {show st} {code}</div>|]
      allSymbolTypes :: [SymbolType]
      allSymbolTypes = [minBound .. maxBound]
      symbolsOfType :: SymbolType -> Html
      symbolsOfType symbolType =
        [hsx|
          <span data-frp-symbol-count data-symbol-type={inputValue symbolType} data-placeholder="...">
            {show symbolType}: 0
          </span>
        |]
