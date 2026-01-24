module Web.View.DataView where

import Data.Maybe
import Data.Text hiding (length)
import Web.Prelude
import Web.View.Data.GetSymbolsHtml
import Web.View.Data.SelectSymbolHtml
import Web.View.Data.CandleChartHtml
import Web.View.DebugHtml
import Web.Types

data DataView = DataView
  { typeSymbolsMap :: TypeSymbolsMap
  , mbSelectedSymbol :: Maybe SelectedSymbol}

instance View DataView where
  html DataView { typeSymbolsMap, mbSelectedSymbol } =
    [hsx|
      {getSymbolsHtml typeSymbolsMap}
      {selectSymbolHtml typeSymbolsMap}
      {candleChartHtml <$> mbSelectedSymbol}
    |]