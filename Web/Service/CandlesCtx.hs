{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Web.Service.CandlesCtx
  ( CandlesCtx(..)
  ) where

import           Data.Text (pack)
import           Data.Time (LocalTime, defaultTimeLocale, formatTime)
import           Data.Typeable (Typeable)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import qualified Proto.Candles as Proto
import           Proto.SseStatus (SseStatus(..))
import           Web.Prelude hiding (Success)
import           Web.Fetcher.CandleFetcher (CandleRange(..), fetchCandles)
import           Web.Repo.CandleRepo (getCandlesWindow, hasCoverage, upsertCandles)
import           Web.Repo.SymbolRepo (clampCandleWindow, getSymbolByCodeType)
import           Web.Service.Policy.CoveragePolicy
import           Web.Service.Policy.RepoPolicy
import           Web.Service.Policy.NotifyPolicy
import           Web.Service.Policy.RespondPolicy
import           Web.Service.Policy.FetchPolicy (FetchPolicy(..))
import           Web.Types
import           Web.Service.Infrastructure.NotifyHub (publishToClient)

data CandlesCtx = CandlesCtx
  { symbolType :: SymbolType
  , symbolCode :: Text
  , timeframe  :: Int
  , fromDt     :: LocalTime
  , toDt       :: LocalTime
  , clientId   :: Text
  } deriving (Eq, Show, Typeable)

------------------
instance CoveragePolicy CandlesCtx where
  coveredByRequest a b =
    get #clientId a == get #clientId b
      && get #symbolType a == get #symbolType b
      && get #symbolCode a == get #symbolCode b
      && get #timeframe a == get #timeframe b
      && get #fromDt a <= get #fromDt b
      && get #toDt a >= get #toDt b
  coveredByRepo CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt } = do
    mbSymbol <- getSymbolByCodeType symbolType symbolCode
    case mbSymbol of
      Nothing -> pure False
      Just _ -> hasCoverage symbolType symbolCode timeframe fromDt toDt

------------------
instance FetchPolicy CandlesCtx where
  type FetchResult CandlesCtx = [Candle]
  fetchTask CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt } = do
    mbSymbol <- getSymbolByCodeType symbolType symbolCode
    case mbSymbol of
      Nothing -> pure []
      Just sym -> do
        let (fromC, toC) = clampCandleWindow sym fromDt toDt
            (Id sid) = get #id sym
            toText = pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S"
            candleRange = CandleRange
              { symbolId = sid
              , symbolCode = symbolCode
              , timeframe = timeframe
              , fromDatetime = toText fromC
              , toDatetime = toText toC
              }
        fetchCandles candleRange

------------------
instance RepoPolicy CandlesCtx where
  type RepoPayload CandlesCtx = Proto.CandlesResponse
  readFromRepo CandlesCtx { symbolType, symbolCode, timeframe, fromDt, toDt } = do
    mbSymbol <- getSymbolByCodeType symbolType symbolCode
    case mbSymbol of
      Nothing -> pure $ Proto.CandlesResponse True (show symbolType) symbolCode timeframe []
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
  upsertFromFetch _ candles = upsertCandles candles

------------------
instance RespondPolicy CandlesCtx where
  type RespondPayload CandlesCtx = Proto.CandlesResponse
  respondHttp _ complete (Proto.CandlesResponse _ st code tf pts) =
    A.object
      ( [ "complete" A..= complete
        , "symbolType" A..= st
        , "symbolCode" A..= code
        , "timeframe" A..= tf
        , "data" A..= pts
        ]
      )
  respondSse _ status =
    case status of
      Success ->
        A.object [ "status" A..= ("success" :: Text) ]
      Duplicated ->
        A.object [ "status" A..= ("duplicated" :: Text) ]
      Failed reason ->
        A.object
          [ "status" A..= ("failed" :: Text)
          , "reason" A..= reason
          ]

instance NotifyPolicy CandlesCtx where
  notifySse ctx status =
    publishToClient (get #clientId ctx) (LBS.toStrict (A.encode (respondSse ctx status)))
