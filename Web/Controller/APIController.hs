module Web.Controller.APIController where

import IHP.ControllerPrelude
import Web.Types

instance Controller APIController where
  action CandlesAction = do
    symbolCode <- param @Text "symbolCode"
    symbolType <- param @Text "symbolType"
    dateframe <- param @Int "dataframe"
    begin <- param @Datetime "begin"
    end <- param @Datetime "end"
    
    renderJson (map toChartCandle candles)

toChartCandle :: Candle -> A.Value
toChartCandle c =
  A.object
    [ "time"  A..= get #time c
    , "open"  A..= get #open c
    , "high"  A..= get #high c
    , "low"   A..= get #low c
    , "close" A..= get #close c
    ]