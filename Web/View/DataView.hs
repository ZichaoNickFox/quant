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
        <div data-symbol-picker="1">
          <input type="text" list="symbol-list" oninput="
              const v = this.value;
              const i = v.indexOf('|');
              const root = this.closest('[data-symbol-picker]');
              if (i > 0) {
                root.querySelector('[name=symbolType]').value = v.slice(0, i);
                root.querySelector('[name=symbolCode]').value = v.slice(i + 1);
              }
            " onfocus="this.value='';" />
          <input type="hidden" name="symbolType" />
          <input type="hidden" name="symbolCode" />
          <datalist id="symbol-list">
          </datalist>
          <button type="button">Faviorate</button>
        </div>
      </div>
      <div id="lw-chart" style="height: 320px; border: 1px dashed #2e8b57; border-radius: 6px; margin-top: 12px;">
        <div style="padding: 8px; color: #2e8b57;">lightweight-charts placeholder</div>
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
