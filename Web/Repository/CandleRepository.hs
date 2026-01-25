{-# LANGUAGE ImplicitParams #-}
module Web.Repository.CandleRepository
  ( getCandlesWindow
  , upsertCandles
  ) where

import           Data.String.Conversions        (cs)
import           Database.PostgreSQL.Simple     (executeMany)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Generated.Types
import           Prelude                        (readFile)
import           Web.Prelude

getCandlesWindow
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => UUID -> Int -> LocalTime -> LocalTime -> IO [Candle]
getCandlesWindow sid timeframe fromDt toDt =
  query @Candle
    |> filterWhere (#symbolId, sid)
    |> filterWhere (#timeframe, timeframe)
    |> filterWhereGreaterThanOrEqualTo (#datetime, fromDt)
    |> filterWhereLessThanOrEqualTo (#datetime, toDt)
    |> orderBy #datetime
    |> fetch

-- UPSERT candles by primary key (symbol_id, timeframe, datetime)
upsertCandles
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
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
