{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Web.Sync.Candles where

import qualified Data.Aeson                   as A
import qualified Proto.Candles               as Proto
import           Web.Prelude
import           Web.Repository.CandleRepository
import           Web.Repository.SymbolRepository (getSymbolByCodeType)
import           Web.Sync.Policy.TwoPhase
import           Web.Types

type SyncCtx = (?modelContext :: ModelContext)

-- Context carries request parameters
data CandlesCtx = CandlesCtx
  { symbolType :: SymbolType
  , symbolCode :: Text
  , timeframe  :: Int
  , fromDt     :: LocalTime
  , toDt       :: LocalTime
  }

-- FillStrategy: need fill if DB coverage is missing the requested window
instance FillStrategy CandlesCtx IO where
  needsFill CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt } = do
    mbSymbol <- getSymbolByCodeType symbolType symbolCode
    case mbSymbol of
      Nothing -> pure True
      Just sym -> do
        covered <- hasCoverage symbolType symbolCode timeframe fromDt toDt
        pure (not covered)
  markFresh _ = pure () -- no-op; could update a coverage table if available

instance AsyncFill CandlesCtx IO where
  enqueueFill CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt } = do
    -- TODO: enqueue a concrete job that downloads this window; placeholder no-op
    pure ()

instance TwoPhaseResponder CandlesCtx Proto.CandlesResponse IO where
  fetchData CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt } = do
    mbSymbol <- getSymbolByCodeType symbolType symbolCode
    case mbSymbol of
      Nothing -> pure $ Proto.CandlesResponse False (show symbolType) symbolCode timeframe []
      Just sym -> do
        let uuid = case sym of Symbol { id = Id u } -> u
        cs <- getCandlesWindow uuid timeframe fromDt toDt
        let protoCandles = fmap (\(c :: Candle) -> Proto.Candle
              { time   = tshow (datetime c :: LocalTime)
              , open   = open c   :: Float
              , high   = high c   :: Float
              , low    = low c    :: Float
              , close  = close c  :: Float
              , volume = Just (volume c :: Float)
              , amount = Just (amount c :: Float)
              }) cs
        pure $ Proto.CandlesResponse True (tshow symbolType) symbolCode timeframe protoCandles

  toResponse _ completeFlag (Proto.CandlesResponse _ st code tf pts) =
    A.object
      [ "complete" A..= completeFlag
      , "symbolType" A..= st
      , "symbolCode" A..= code
      , "timeframe" A..= tf
      , "data" A..= pts
      ]
