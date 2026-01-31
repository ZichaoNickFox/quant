module Web.Sync.Policy.TTL where

import           Web.Prelude
import           Web.Types
import           Data.Time (utc, localTimeToUTC, utcToLocalTime)

-- | TTL 判定：last_refreshed_at + ttl_seconds <= now => 需要填充
needsFillTTL :: (?modelContext :: ModelContext) => Text -> IO Bool
needsFillTTL datasetKey = do
  now <- getCurrentTime
  let key = Id datasetKey :: Id DataFreshness 
  mMeta <- fetchOneOrNothing $ query @DataFreshness |> filterWhere (#datasetKey, key)
  pure case mMeta of
    Nothing   -> True
    Just meta ->
      let refreshedAtUtc = localTimeToUTC utc (get #lastRefreshedAt meta)
       in addUTCTime (fromIntegral (get #ttlSeconds meta)) refreshedAtUtc <= now

markFreshTTL :: (?modelContext :: ModelContext) => Text -> IO ()
markFreshTTL datasetKey = do
  now <- getCurrentTime
  let key = Id datasetKey :: Id DataFreshness 
  _ <- sqlExec
    "INSERT INTO data_freshness (dataset_key, last_refreshed_at) VALUES (?, ?)\n\
    \ON CONFLICT (dataset_key) DO UPDATE SET last_refreshed_at = EXCLUDED.last_refreshed_at"
    (key, utcToLocalTime utc now)
  pure ()
