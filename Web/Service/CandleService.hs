module Service.CandleService
  ( DownloadCandleJob(..)
  ) where

import           Control.Exception
import           Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import           Generated.Types
import           Prelude
import           Web.Prelude
import           Web.Provider.SymbolProvider

getCandlesFromLocal
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller) =>
  IO TypeNamesMap
getCandlesFromLocal = undefined

updateCandlesToLocal
  :: (?context :: ControllerContext, ?modelContext :: ModelContext, ?theAction :: controller)
  => SymbolType -> IO ()
updateCandlesToLocal symbolType = undefined