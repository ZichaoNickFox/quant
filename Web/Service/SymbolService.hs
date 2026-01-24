module Web.Service.SymbolService
  ( getTypeSymbolsMapFromDB
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

getTypeSymbolsMapFromDB
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) =>
  IO TypeSymbolsMap
getTypeSymbolsMapFromDB = do
  let symbolTypes = [minBound .. maxBound] :: [SymbolType]
  symbolTypeCodesMap <- M.fromList <$> do
    forM symbolTypes $ \symbolType -> do
      symbols <- query @Symbol
        |> filterWhere (#symbolType, symbolType)
        |> fetch
      pure (symbolType, symbols)
  return symbolTypeCodesMap

updateSymbolToDB
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => SymbolType -> IO ()
updateSymbolToDB symbolType = do
  newRecord @UpdateSymbolJob
    |> set #symbolType symbolType
    |> create
  return ()