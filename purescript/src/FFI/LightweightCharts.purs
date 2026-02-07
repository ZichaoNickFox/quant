module FFI.LightweightCharts
  ( Chart
  , CandlestickSeries
  , createChart
  , addCandlestickSeries
  , setDataFromJson
  , fitContent
  ) where

import Prelude

import Effect (Effect)
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
