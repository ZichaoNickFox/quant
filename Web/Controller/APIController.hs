module Web.Controller.APIController where

import qualified Data.Aeson                    as A
import           Data.Text
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time                     (LocalTime, addLocalTime, getCurrentTime, getCurrentTimeZone,
                                                secondsToNominalDiffTime, utcToLocalTime)
import           Web.Prelude
import           Web.Service.CandlesCtx (CandlesCtx(..))
import           Web.Service.Process.ServiceProcess (beginServiceProcess)
import           Web.Service.SymbolsCtx (SymbolsCtx(..))
import           Web.Types

instance Controller APIController where
  action APICandlesAction = do
    logController Info $ "[APIController][CandlesAction]" <> requestUrl

    let symbolCode = param @Text "symbolCode"
        symbolType = param @SymbolType "symbolType"
        timeframe  = param @Int "timeframe"
        fromParam  = paramOrNothing @LocalTime "from"
        toParam    = paramOrNothing   @LocalTime "to"
        fallbackDays = -30
    now <- getCurrentTime
    tz  <- getCurrentTimeZone
    let defaultTo   = utcToLocalTime tz now
        defaultFrom = addLocalTime (secondsToNominalDiffTime (fallbackDays * 86400)) defaultTo
        fromDt = fromMaybe defaultFrom fromParam
        toDt   = fromMaybe defaultTo toParam
        clientId = fromMaybe "default" $
          (getHeader "X-Client-Id" >>= (Just . decodeUtf8))
            <|> paramOrNothing @Text "clientId"
    let ctx = CandlesCtx
          { symbolType
          , symbolCode
          , timeframe
          , fromDt
          , toDt
          , clientId
          }
    res <- beginServiceProcess ctx
    renderJson res

  -- Return symbol counts (complete immediately; skipCheck reserved for consistency)
  action APISymbolsAction = do
    logController Info $ "[APIController][SymbolsAction]" <> requestUrl
    let clientId = fromMaybe "default" $
          (getHeader "X-Client-Id" >>= (Just . decodeUtf8))
            <|> paramOrNothing @Text "clientId"
    let ctx = SymbolsCtx { clientId }
    res <- beginServiceProcess ctx
    renderJson res
