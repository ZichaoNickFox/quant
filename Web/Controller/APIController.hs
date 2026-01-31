module Web.Controller.APIController where

import           Data.Text
import           Data.Time                     (LocalTime, addLocalTime, getCurrentTime, getCurrentTimeZone,
                                                secondsToNominalDiffTime, utcToLocalTime)
import qualified Data.Aeson                    as A
import           Web.Prelude
import           Web.Repository.CandleRepository
import           Web.Repository.SymbolRepository
import           Web.Sync.Policy.TwoPhase
import           Web.Sync.Symbols
import           Web.Sync.Candles
import           Web.Types

instance Controller APIController where
  action CandlesAction = do
    logController Info $ "[APIController][CandlesAction]" <> requestUrl

    let symbolCode = param @Text "symbolCode"
        symbolType = param @SymbolType "symbolType"
        timeframe  = param @Int "timeframe"
        fromParam  = paramOrNothing @LocalTime "from"
        toParam    = paramOrNothing   @LocalTime "to"
        skipCheck  = fromMaybe False (paramOrNothing @Bool "skipCheck")
        fallbackDays = -30
    now <- getCurrentTime
    tz  <- getCurrentTimeZone
    let defaultTo   = utcToLocalTime tz now
        defaultFrom = addLocalTime (secondsToNominalDiffTime (fallbackDays * 86400)) defaultTo
        fromDt = fromMaybe defaultFrom fromParam
        toDt   = fromMaybe defaultTo toParam
        ctx = CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt }
    res <- serveTwoPhase ctx
    renderJson res

  -- Return symbol counts (complete immediately; skipCheck reserved for consistency)
  action SymbolsAction = do
    logController Info $ "[APIController][SymbolsAction]" <> requestUrl
    res <- serveTwoPhase SymbolsCtx
    renderJson res

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
