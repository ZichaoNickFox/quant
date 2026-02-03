module Web.Repo.SymbolRepo
  ( getSymbolByCodeType
  , getSymbolsByTypeMap
  , upsertSymbols
  , clampCandleWindow
  ) where

import           Control.Monad
import           Data.String.Conversions (cs)
import qualified Data.Map                   as M
import           Data.Time                  (LocalTime)
import           Database.PostgreSQL.Simple (executeMany)
import           Database.PostgreSQL.Simple.Types (Query (..))
import           Generated.Types
import           Prelude (readFile)
import           Web.Prelude
import           Web.Types

getSymbolByCodeType
  :: (?modelContext :: ModelContext)
  => SymbolType -> Text -> IO (Maybe Symbol)
getSymbolByCodeType symbolType code =
  query @Symbol
    |> filterWhere (#code, code)
    |> filterWhere (#symbolType, symbolType)
    |> fetchOneOrNothing

getSymbolsByType
  :: (?modelContext :: ModelContext)
  => SymbolType -> IO [Symbol]
getSymbolsByType symbolType =
  query @Symbol
    |> filterWhere (#symbolType, symbolType)
    |> fetch

-- Clamp a requested candle window to the symbol's listing lifespan.
-- delist_date may be NULL; in that case upper bound is unchanged.
clampCandleWindow :: Symbol -> LocalTime -> LocalTime -> (LocalTime, LocalTime)
clampCandleWindow sym fromDt toDt =
  let from' = max fromDt (get #listDate sym)
      to'   = maybe toDt (\d -> min toDt d) (get #delistDate sym)
  in (from', to')

getSymbolsByTypeMap
  :: (?modelContext :: ModelContext)
  => IO TypeSymbolsMap
getSymbolsByTypeMap = do
  let symbolTypes = [minBound .. maxBound] :: [SymbolType]
  M.fromList <$> forM symbolTypes (\symbolType -> do
    symbols <- getSymbolsByType symbolType
    pure (symbolType, symbols))

upsertSymbols :: (?modelContext :: ModelContext) => [Symbol] -> IO ()
upsertSymbols [] = pure ()
upsertSymbols symbols = do
  content <- readFile "Web/Repo/SymbolUpsert.sql"
  let sql = Query (cs content)
  let params s =
        ( get #code s
        , get #name s
        , get #symbolType s
        , get #listDate s
        , get #delistDate s
        )
  withDatabaseConnection $ \conn -> do
    _ <- executeMany conn sql (map params symbols)
    pure ()
