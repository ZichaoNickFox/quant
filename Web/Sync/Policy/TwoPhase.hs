module Web.Sync.Policy.TwoPhase where

import qualified Data.Aeson as A
import           Web.Prelude

-- | Generic fill condition (e.g., TTL, coverage check)
class Monad m => FillStrategy ctx m where
  needsFill :: (?modelContext :: ModelContext) => ctx -> m Bool      -- 是否需要异步补全
  markFresh :: (?modelContext :: ModelContext) => ctx -> m ()        -- 补全完成后如何标记为新鲜

-- | 触发异步补全（去重逻辑由实现负责）
class Monad m => AsyncFill ctx m where
  enqueueFill :: (?modelContext :: ModelContext) => ctx -> m ()

-- | 将当前数据拿出来并转 JSON，complete 由流程注入
class Monad m => TwoPhaseResponder ctx payload m | ctx -> payload m where
  fetchData  :: (?modelContext :: ModelContext) => ctx -> m payload
  toResponse :: (?modelContext :: ModelContext) => ctx -> Bool -> payload -> A.Value

-- | 通用二阶段：判断补全 -> 可选触发 -> 返回数据 + complete 标志
serveTwoPhase
  :: ( FillStrategy ctx m
     , AsyncFill ctx m
     , TwoPhaseResponder ctx payload m )
  => (?modelContext :: ModelContext)
  => ctx -> m A.Value
serveTwoPhase ctx = do
  stale <- needsFill ctx
  when stale (enqueueFill ctx)
  payload <- fetchData ctx
  let completeFlag = not stale
  pure (toResponse ctx completeFlag payload)
