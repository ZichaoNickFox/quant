module Web.Workers
  ( workers
  ) where

import Web.Prelude
import Web.Job.UpdateSymbolJob
import Web.Types

instance Worker WebApplication where
    workers _ =
      [ worker @UpdateSymbolJob
      -- Generator Marker
      ]