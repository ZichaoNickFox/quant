module Web.Service.SymbolService
  ( getSymbolsFromDB
  , UpdateSymbolJob(..)
  , updateSymbolToDB
  ) where

import           Control.Exception
import           Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import           Web.Prelude
import           Web.Provider.SymbolProvider
import           Web.Job.UpdateSymbolJob
import           Web.Types

getSymbolsFromDB
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) =>
  IO (M.Map SymbolType [Symbol])
getSymbolsFromDB = do
  let symbolTypes = [minBound .. maxBound] :: [SymbolType]
  symbolsByType <- M.fromList <$> do
    forM symbolTypes $ \symbolType -> do
      symbols <- query @Symbol
        |> filterWhere (#symbolType, symbolType)
        |> fetch
      pure (symbolType, symbols)
  return symbolsByType

updateSymbolToDB
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => SymbolType -> IO ()
updateSymbolToDB symbolType = do
  newRecord @UpdateSymbolJob
    |> set #symbolType symbolType
    |> create
  return ()