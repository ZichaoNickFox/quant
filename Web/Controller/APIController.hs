module Web.Controller.APIController where

import           Data.Text
import           Data.Time                     (LocalTime, addLocalTime, getCurrentTime, getCurrentTimeZone,
                                                secondsToNominalDiffTime, utcToLocalTime)
import qualified Data.Aeson                    as A
import           Web.Prelude
import           Web.Repository.CandleRepository
import           Web.Repository.SymbolRepository
import           Web.Types

instance Controller APIController where
  action CandlesAction = do
    logController Info $ "[APIController][CandlesAction]" <> requestUrl

    let symbolCode = param @Text "symbolCode"
        symbolType = param @SymbolType "symbolType"
        timeframe  = param @Int "timeframe"
        fromParam  = paramOrNothing @LocalTime "from"
        toParam    = paramOrNothing   @LocalTime "to"

    maybeSymbol <- getSymbolByCodeType symbolType symbolCode
    case maybeSymbol of
      Nothing -> do
        setErrorMessage ("Symbol not found" :: Text)
        renderJson ([] :: [A.Value])
      Just symbol -> do
        let (Id sid) = get #id symbol
        now <- getCurrentTime
        tz  <- getCurrentTimeZone
        let todayLocal = utcToLocalTime tz now
            defaultTo   = todayLocal
            defaultFrom = addLocalTime (secondsToNominalDiffTime (-30 * 86400)) defaultTo
            fromDt = fromMaybe defaultFrom fromParam
            toDt   = fromMaybe defaultTo toParam
        candles <- getCandlesWindow sid timeframe fromDt toDt
        renderJson (toChartCandle <$> candles)

-- Lightweight JSON for Candle
toChartCandle :: Candle -> A.Value
toChartCandle c =
  A.object
    [ "time"   A..= get #datetime c
    , "open"   A..= get #open c
    , "high"   A..= get #high c
    , "low"    A..= get #low c
    , "close"  A..= get #close c
    , "volume" A..= get #volume c
    , "amount" A..= get #amount c
    , "symbolId" A..= get #symbolId c
    , "timeframe" A..= get #timeframe c
    ]
