module Web.Service.CandleService
  ( getCandlesWithAutoFill
  , getCandlesWithAutoFillDefault
  , CoveragePolicy (..)
  , defaultCoveragePolicy
  , DownloadCandleJob(..)
  ) where

import           Data.List                      (maximum, minimum)
import           Data.Time
import           Data.UUID                      (UUID)
import           Generated.Types
import           Web.Fetcher.CandleFetcher
import           Web.Prelude
import           Web.Repository.CandleRepository
import           Web.Repository.SymbolRepository

-- | Return candles for a window. If DB coverage is incomplete, fetch missing data, upsert, and return refreshed data.
-- | Bool indicates whether a fetch was performed (True = fetched/filled).
getCandlesWithAutoFill
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => SymbolType -- ^ symbolType
  -> Text       -- ^ symbolCode
  -> Int        -- ^ timeframe
  -> LocalTime  -- ^ from
  -> LocalTime  -- ^ to
  -> IO ([Candle], Bool)
getCandlesWithAutoFill symbolType symbolCode timeframe fromDt toDt = do
  symbol <- getSymbolByCodeType symbolType symbolCode >>= \case
    Just s  -> pure s
    Nothing -> fail $ "Symbol not found: " <> show symbolType <> " (" <> show symbolCode <> ")"

  let sid = get #id symbol
  candles <- getCandlesWindow sid timeframe fromDt toDt
  if cover candles fromDt toDt
    then pure (candles, False)
    else do
      fetched <- fetchAndStore sid symbolCode timeframe fromDt toDt
      when (null fetched) $ logWarn ("[CandleService] fetch returned empty for " <> symbolCode)
      refreshed <- getCandlesWindow sid timeframe fromDt toDt
      pure (refreshed, True)

-- | Check if existing candles cover the requested window.
cover :: [Candle] -> LocalTime -> LocalTime -> Bool
cover [] _ _ = False
cover cs fromDt toDt =
  let dts = fmap (get #datetime) cs
   in minimum dts <= fromDt && maximum dts >= toDt

-- | Coverage policy: how to choose a default window and tolerances.
data CoveragePolicy = CoveragePolicy
  { defaultWindow      :: Symbol -> Int -> IO (LocalTime, LocalTime) -- ^ Given symbol & timeframe, default (from,to)
  , startToleranceDays :: Integer                                   -- ^ Allow missing up to N days before from
  , endToleranceBars   :: Int                                        -- ^ Allow missing up to N bars after to
  }

-- | Default: from list_date to today (00:00), tolerate 3 days at start, 1 bar at end.
defaultCoveragePolicy :: CoveragePolicy
defaultCoveragePolicy = CoveragePolicy
  { defaultWindow = \symbol _ -> do
      now <- getCurrentTime
      tz  <- getCurrentTimeZone
      let today = localDay (utcToLocalTime tz now)
          toDt  = LocalTime today (TimeOfDay 0 0 0)
          fromDt = get #listDate symbol
      pure (fromDt, toDt)
  , startToleranceDays = 3
  , endToleranceBars   = 1
  }

-- | Convenience: use policy-derived default window (list_date → today) then auto-fill.
getCandlesWithAutoFillDefault
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => CoveragePolicy
  -> SymbolType
  -> Text
  -> Int
  -> IO ([Candle], Bool)
getCandlesWithAutoFillDefault policy symbolType symbolCode timeframe = do
  symbol <- getSymbolByCodeType symbolType symbolCode >>= \case
    Just s  -> pure s
    Nothing -> fail $ "Symbol not found: " <> show symbolType <> " (" <> show symbolCode <> ")"
  (fromDt, toDt) <- defaultWindow policy symbol timeframe
  getCandlesWithAutoFill symbolType symbolCode timeframe fromDt toDt

fetchAndStore
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => UUID -> Text -> Int -> LocalTime -> LocalTime -> IO [Candle]
fetchAndStore sid code timeframe fromDt toDt = do
  let fmt = "%Y-%m-%d %H:%M:%S"
      toText = pack . formatTime defaultTimeLocale fmt
      range = CandleRange
        { symbolId = sid
        , symbolCode = code
        , timeframe = timeframe
        , fromDatetime = toText fromDt
        , toDatetime = toText toDt
        }
  fetched <- getCandles range
  upsertCandles fetched
  pure fetched
