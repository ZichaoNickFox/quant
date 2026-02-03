{-# LANGUAGE TypeFamilies #-}
module Web.Service.Policy.FetchPolicy
  ( FetchPolicy(..)
  ) where

import           Web.Prelude hiding (FetchResult)

class FetchPolicy req where
  type FetchResult req
  fetchTask :: (?modelContext :: ModelContext, ?context :: context, LoggingProvider context) => req -> IO (FetchResult req)
