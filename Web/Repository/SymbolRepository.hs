module Web.Repository.SymbolRepository
  ( getSymbolByCodeType
  , getTypeSymbolsMapFromDB
  , getSymbolCountByType
  , clampCandleWindow
  ) where

import           Control.Monad
import qualified Data.Map                   as M
import           Data.Time                  (LocalTime)
import           Generated.Types
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

getSymbolCountByType
  :: (?modelContext :: ModelContext)
  => SymbolType -> IO Int
getSymbolCountByType symbolType =
  query @Symbol
    |> filterWhere (#symbolType, symbolType)
    |> fetchCount

-- Clamp a requested candle window to the symbol's listing lifespan.
-- delist_date may be NULL; in that case upper bound is unchanged.
clampCandleWindow :: Symbol -> LocalTime -> LocalTime -> (LocalTime, LocalTime)
clampCandleWindow sym fromDt toDt =
  let from' = max fromDt (get #listDate sym)
      to'   = maybe toDt (\d -> min toDt d) (get #delistDate sym)
  in (from', to')

getTypeSymbolsMapFromDB
  :: (?modelContext :: ModelContext)
  => IO TypeSymbolsMap
getTypeSymbolsMapFromDB = do
  let symbolTypes = [minBound .. maxBound] :: [SymbolType]
  M.fromList <$> forM symbolTypes (\symbolType -> do
    symbols <- query @Symbol
      |> filterWhere (#symbolType, symbolType)
      |> fetch
    pure (symbolType, symbols))
