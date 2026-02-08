module Web.Service.ColdWarm.Policy.TTLPolicy
  ( TTLPolicy(..)
  ) where

import           Web.Prelude

class TTLPolicy ctx where
  ttlKey :: ctx -> Text
  ttlSeconds :: ctx -> Int
