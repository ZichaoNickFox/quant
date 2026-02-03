{-# LANGUAGE TypeFamilies #-}
module Web.Service.Policy.RepoPolicy
  ( RepoPolicy(..)
  ) where

import           Web.Prelude hiding (FetchResult)
import           Web.Service.Policy.FetchPolicy (FetchPolicy(..))

class RepoPolicy ctx where
  type RepoPayload ctx
  readFromRepo :: (?modelContext :: ModelContext) => ctx -> IO (RepoPayload ctx)
  upsertFromFetch :: (?modelContext :: ModelContext, FetchPolicy ctx) => ctx -> FetchResult ctx -> IO ()
