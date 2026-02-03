module Web.Service.Process.ServiceProcess
  ( beginServiceProcess
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM
import           Control.Exception (SomeException, displayException, try)
import           Control.Monad (void)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS
import           Data.Dynamic (Dynamic, fromDynamic, toDyn)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable, typeRep)
import           System.IO.Unsafe (unsafePerformIO)
import           Web.Prelude hiding (Success)
import           Web.Service.Policy.CoveragePolicy (CoveragePolicy(..))
import           Web.Service.Policy.FetchPolicy (FetchPolicy(..))
import           Web.Service.Policy.NotifyPolicy (NotifyPolicy(..))
import           Web.Service.Policy.RepoPolicy (RepoPolicy(..))
import           Web.Service.Policy.RespondPolicy (RespondPolicy(..))
import           Proto.SseStatus (SseStatus (..))

data ActiveState ctx = ActiveState
  { activeRequests :: TVar [ctx]
  }

activeStore :: IORef (M.Map TypeRep Dynamic)
{-# NOINLINE activeStore #-}
activeStore = unsafePerformIO (newIORef M.empty)

getActiveStateSingleton
  :: forall ctx
   . Typeable ctx
  => IO (ActiveState ctx)
getActiveStateSingleton = do
  let key = typeRep (Proxy @ctx)
  store <- readIORef activeStore
  case M.lookup key store >>= fromDynamic of
    Just st -> pure st
    Nothing -> do
      activeRequests <- newTVarIO []
      let st = ActiveState { activeRequests }
      atomicModifyIORef' activeStore (\m -> (M.insert key (toDyn st) m, ()))
      pure st

beginServiceProcess
  :: ( ?modelContext :: ModelContext
     , ?context :: context
     , LoggingProvider context
     , CoveragePolicy ctx
     , FetchPolicy ctx
     , RepoPolicy ctx
     , RespondPolicy ctx
     , NotifyPolicy ctx
     , Eq ctx
     , Typeable ctx
     , HasField "clientId" ctx Text
     , RespondPayload ctx ~ RepoPayload ctx
     )
  => ctx
  -> IO A.Value
beginServiceProcess ctx = do
  needFetch <- not <$> coveredByRepo ctx
  payload <- readFromRepo ctx
  let complete = not needFetch
      response = respondHttp ctx complete payload
  when needFetch $ do
    st <- getActiveStateSingleton
    shouldFetch <- atomically $ do
      active <- readTVar (activeRequests st)
      if any (\r -> coveredByRequest r ctx) active
        then pure False
        else modifyTVar' (activeRequests st) (ctx :) >> pure True
    if shouldFetch
      then void $ forkIO $ do
        result <- try (do
          fetchRes <- fetchTask ctx
          upsertFromFetch ctx fetchRes
          ) :: IO (Either SomeException ())
        let status = case result of
              Right _ -> Success
              Left err -> Failed (T.pack (displayException err))
        notifySse ctx status
        atomically $ modifyTVar' (activeRequests st) (filter (/= ctx))
      else notifySse ctx Duplicated
  pure response
