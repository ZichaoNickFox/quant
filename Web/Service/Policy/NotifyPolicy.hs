module Web.Service.Policy.NotifyPolicy
  ( NotifyPolicy(..)
  ) where

import           Proto.SseStatus (SseStatus)
import           Web.Prelude

class NotifyPolicy ctx where
  notifySse :: ctx -> SseStatus -> IO ()
