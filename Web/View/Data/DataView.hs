module Web.View.Data.DataView where

import Data.Map
import Data.Maybe
import Data.Text hiding (length)
import Generated.Types
import IHP.ViewPrelude hiding (Symbol)
import IHP.ViewSupport
import Web.View.Data.GetSymbolsHtml
import Web.View.Data.SelectSymbolHtml
import Web.View.Data.CandleChartHtml
import Web.View.DebugHtml

data DataView = DataView
  { symbolsByType :: Map SymbolType [Symbol]
  , mbSelectedSymbolCode :: Maybe Text}

instance View DataView where
  html DataView { symbolsByType, mbSelectedSymbolCode } =
    [hsx|
      {getSymbolsHtml symbolsByType}
      {selectSymbolHtml symbolsByType}
      {candleChartHtml mbSelectedSymbolCode}
    |]