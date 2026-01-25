module Web.Repository.SymbolRepository
  ( getSymbolByCodeType
  , getTypeSymbolsMapFromDB
  ) where

import           Control.Monad
import qualified Data.Map                   as M
import           Generated.Types
import           Web.Prelude
import           Web.Types

getSymbolByCodeType
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => SymbolType -> Text -> IO (Maybe Symbol)
getSymbolByCodeType symbolType code =
  query @Symbol
    |> filterWhere (#code, code)
    |> filterWhere (#symbolType, symbolType)
    |> fetchOneOrNothing

getTypeSymbolsMapFromDB
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => IO TypeSymbolsMap
getTypeSymbolsMapFromDB = do
  let symbolTypes = [minBound .. maxBound] :: [SymbolType]
  M.fromList <$> forM symbolTypes (\symbolType -> do
    symbols <- query @Symbol
      |> filterWhere (#symbolType, symbolType)
      |> fetch
    pure (symbolType, symbols))
