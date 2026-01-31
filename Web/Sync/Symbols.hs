{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Sync.Symbols where

import qualified Data.Aeson                   as A
import qualified Data.Aeson.Key               as K
import qualified Data.Map                    as M
import           Data.Kind (Constraint)
import qualified Proto.Symbols               as Proto
import           Web.Prelude
import           Web.Repository.SymbolRepository (getSymbolCountByType)
import           Web.Sync.Policy.TwoPhase
import           Web.Sync.Policy.TTL
import           Web.Types

type SyncCtx :: Constraint
type SyncCtx = (?modelContext :: ModelContext)

data SymbolsCtx = SymbolsCtx

-- TTL-based fill: stale if now >= last_refreshed_at + ttl_seconds
instance FillStrategy SymbolsCtx IO where
  needsFill _ = needsFillTTL "symbols"
  markFresh _ = markFreshTTL "symbols"

instance AsyncFill SymbolsCtx IO where
  enqueueFill _ = do
    _ <- (newRecord @UpdateSymbolJob |> set #symbolType Stock |> create)
    pure ()

instance TwoPhaseResponder SymbolsCtx Proto.SymbolCountsResponse IO where
  fetchData _ = do
    let sts = [minBound .. maxBound] :: [SymbolType]
    pairs <- forM sts $ \st -> do
      c <- getSymbolCountByType st
      pure (tshow st, c)
    pure $ Proto.SymbolCountsResponse { complete = True, counts = M.fromList pairs }

  toResponse _ completeFlag Proto.SymbolCountsResponse { counts } =
    A.object (("complete" A..= completeFlag) : [ K.fromText k A..= v | (k,v) <- M.toList counts ])
