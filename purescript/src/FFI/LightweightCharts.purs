module FFI.LightweightCharts
  ( addCandlestickSeries
  , CandlestickSeries
  , Chart
  , createChart
  , fitContent
  , setDataFromJson
  ) where

import Effect (Effect)

import Prelude
import Web.DOM.Element (Element)

foreign import data Chart :: Type
foreign import data CandlestickSeries :: Type

foreign import _createChart :: Element -> Effect Chart
foreign import _addCandlestickSeries :: Chart -> Effect CandlestickSeries
foreign import _setDataFromJson :: CandlestickSeries -> String -> Effect Unit
foreign import _fitContent :: Chart -> Effect Unit

createChart :: Element -> Effect Chart
createChart = _createChart

addCandlestickSeries :: Chart -> Effect CandlestickSeries
addCandlestickSeries = _addCandlestickSeries

setDataFromJson :: CandlestickSeries -> String -> Effect Unit
setDataFromJson = _setDataFromJson

fitContent :: Chart -> Effect Unit
fitContent = _fitContent
