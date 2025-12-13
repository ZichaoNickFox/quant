module Service.SymbolService
  ( getSymbolsFromLocal
  , updateSymbolsToLocal
  ) where

import           Control.Exception
import           Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import           Generated.Types
import           IHP.ControllerSupport
import           IHP.Fetch
import           IHP.HaskellSupport
import           IHP.ModelSupport
import           IHP.QueryBuilder
import           Prelude
import           Service.Provider.SymbolProvider

getSymbolsFromLocal
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) =>
  IO (M.Map SymbolType [Symbol])
getSymbolsFromLocal = do
  let symbolTypes = [minBound .. maxBound] :: [SymbolType]
  symbolsByType <- M.fromList <$> do
    forM symbolTypes $ \symbolType -> do
      symbols <- query @Symbol
        |> filterWhere (#symbolType, symbolType)
        |> fetch
      pure (symbolType, symbols)
  return symbolsByType

updateSymbolsToLocal
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => SymbolType -> IO ()
updateSymbolsToLocal symbolType = do
  symbols <- provideSymbols symbolType
  sql <- provideSymbolsSQL
  withDatabaseConnection $ \conn -> do
    _ <- PG.executeMany conn sql symbols
    pure ()