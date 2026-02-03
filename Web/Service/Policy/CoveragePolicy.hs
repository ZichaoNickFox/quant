module Web.Service.Policy.CoveragePolicy
  ( CoveragePolicy(..)
  ) where

import Web.Prelude

class CoveragePolicy req where
  coveredByRequest :: req -> req -> Bool
  coveredByRepo :: (?modelContext :: ModelContext) => req -> IO Bool
