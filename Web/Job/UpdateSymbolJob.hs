module Web.Job.UpdateSymbolJob where

import Data.String.Conversions (cs)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (Query(..))
import Prelude (readFile)
import Web.Prelude
import Web.Provider.SymbolProvider
import Web.Types

instance Job UpdateSymbolJob where
  perform updateSymbolJob = do
    logInfo ("[UpdateSymbolJob] perform updateSymbolJob" :: Text)
    let symbolType = get #symbolType updateSymbolJob
    symbols <- downloadSymbols symbolType
    sql <- Query . cs <$> readFile "Web/Provider/SymbolProvider.sql"
    withDatabaseConnection $ \conn -> do
      _ <- executeMany conn sql symbols
      pure ()
  maxAttempts = 1