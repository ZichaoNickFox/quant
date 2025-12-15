module Web.Job.UpdateSymbolJob where
import Data.String.Conversions (cs)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Generated.Types
import IHP.FrameworkConfig
import IHP.ModelSupport
import IHP.Job.Types
import IHP.Log
import IHP.Prelude
import Prelude (readFile)
import Web.Provider.SymbolProvider
import Web.Types

instance Job UpdateSymbolJob where
  perform updateSymbolJob = do
    let symbolType = get #symbolType updateSymbolJob
    symbols <- downloadSymbols symbolType
    sql <- Query . cs <$> readFile "Service/Provider/SymbolProvider.sql"
    withDatabaseConnection $ \conn -> do
      _ <- executeMany conn sql symbols
      pure ()