module Web.Service.Process.TTLProcess
  ( needTTL
  , upsertTTL
  ) where

import           Web.Prelude
import           Web.Types hiding (ttlSeconds)
import           Data.Time (utc, localTimeToUTC, utcToLocalTime)
import           Web.Service.Policy.TTLPolicy (TTLPolicy(..))

needTTL :: (TTLPolicy ctx, ?modelContext :: ModelContext) => ctx -> IO Bool
needTTL ctx = do
  now <- getCurrentTime
  let key = Id (ttlKey ctx) :: Id DataFreshness
  mMeta <- fetchOneOrNothing $ query @DataFreshness |> filterWhere (#datasetKey, key)
  pure case mMeta of
    Nothing   -> True
    Just meta ->
      let refreshedAtUtc = localTimeToUTC utc (get #lastRefreshedAt meta)
          ttl = fromIntegral (ttlSeconds ctx)
       in addUTCTime ttl refreshedAtUtc <= now

upsertTTL :: (TTLPolicy ctx, ?modelContext :: ModelContext) => ctx -> IO ()
upsertTTL ctx = do
  now <- getCurrentTime
  let key = Id (ttlKey ctx) :: Id DataFreshness
  _ <- sqlExec
    "INSERT INTO data_freshness (dataset_key, last_refreshed_at, ttl_seconds) VALUES (?, ?, ?)\n\
    \ON CONFLICT (dataset_key) DO UPDATE SET last_refreshed_at = EXCLUDED.last_refreshed_at, ttl_seconds = EXCLUDED.ttl_seconds"
    (key, utcToLocalTime utc now, ttlSeconds ctx)
  pure ()
