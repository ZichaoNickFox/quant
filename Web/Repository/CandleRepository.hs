{-# LANGUAGE ImplicitParams #-}
module Web.Repository.CandleRepository
  ( getCandlesWindow
  , hasCoverage
  , upsertCandles
  ) where

import           Data.String.Conversions        (cs)
import           Database.PostgreSQL.Simple     (executeMany)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Generated.Types
import           Prelude                        (readFile)
import           Web.Prelude
import           Web.Repository.SymbolRepository (clampCandleWindow, getSymbolByCodeType)

getCandlesWindow
  :: (?modelContext :: ModelContext)
  => UUID -> Int -> LocalTime -> LocalTime -> IO [Candle]
getCandlesWindow sid timeframe fromDt toDt =
  query @Candle
    |> filterWhere (#symbolId, sid)
    |> filterWhere (#timeframe, timeframe)
    |> filterWhereGreaterThanOrEqualTo (#datetime, fromDt)
    |> filterWhereLessThanOrEqualTo (#datetime, toDt)
    |> orderBy #datetime
    |> fetch

-- Check whether we have full coverage for a given window (existence of first and last)
hasCoverage
  :: (?modelContext :: ModelContext)
  => SymbolType -> Text -> Int -> LocalTime -> LocalTime -> IO Bool
hasCoverage symbolType symbolCode timeframe fromDt toDt = do
  mbSym <- getSymbolByCodeType symbolType symbolCode
  case mbSym of
    Nothing -> pure False
    Just sym -> do
      let (fromC, toC) = clampCandleWindow sym fromDt toDt
          (Id sid)     = get #id sym
      first <- query @Candle
                |> filterWhere (#symbolId, sid)
                |> filterWhere (#timeframe, timeframe)
                |> filterWhereGreaterThanOrEqualTo (#datetime, fromC)
                |> filterWhereLessThanOrEqualTo (#datetime, toC)
                |> orderBy #datetime
                |> fetchOneOrNothing
      lastC <- query @Candle
                |> filterWhere (#symbolId, sid)
                |> filterWhere (#timeframe, timeframe)
                |> filterWhereGreaterThanOrEqualTo (#datetime, fromC)
                |> filterWhereLessThanOrEqualTo (#datetime, toC)
                |> orderByDesc #datetime
                |> fetchOneOrNothing
      pure (isJust first && isJust lastC)

-- UPSERT candles by primary key (symbol_id, timeframe, datetime)
upsertCandles
  :: (?modelContext :: ModelContext)
  => [Candle] -> IO ()
upsertCandles [] = pure ()
upsertCandles candles = do
  content <- readFile "Web/Repository/CandleUpsert.sql"
  let sql = Query (cs content)
  let params c =
        ( get #symbolId c
        , get #timeframe c
        , get #datetime c
        , get #open c
        , get #close c
        , get #high c
        , get #low c
        , get #volume c
        , get #amount c
        )
  withDatabaseConnection $ \conn -> do
    _ <- executeMany conn sql (map params candles)
    pure ()
