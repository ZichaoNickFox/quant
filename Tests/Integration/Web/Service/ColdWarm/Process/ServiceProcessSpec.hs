{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Service.ColdWarm.Process.ServiceProcessSpec (tests) where

import           Config (config)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, tryReadMVar)
import           Control.Exception (finally, throwIO)
import           Data.Aeson (Value(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import           Data.Maybe (isJust)
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Unique (hashUnique, newUnique)
import           IHP.Hspec (withIHPApp)
import           IHP.Log.Types (newLogger)
import           IHP.Test.Mocking (withContext)
import           Prelude
import           System.Timeout (timeout)
import           Test.Hspec
import           Web.FrontController ()
import           Web.Prelude hiding (throwIO, Success)
import           Proto.SseStatus (SseStatus(..))
import           Web.Service.ColdWarm.Policy.CoveragePolicy (CoveragePolicy(..))
import           Web.Service.ColdWarm.Policy.FetchPolicy (FetchPolicy(..))
import           Web.Service.ColdWarm.Policy.NotifyPolicy (NotifyPolicy(..))
import           Web.Service.ColdWarm.Policy.RepoPolicy (RepoPolicy(..))
import           Web.Service.ColdWarm.Policy.RespondPolicy (RespondPolicy(..))
import           Web.Service.ColdWarm.Process.ServiceProcess (beginServiceProcess)
import           Web.Types (WebApplication (..))

data TestCtx = TestCtx
  { clientId :: Text
  , coveredByRepoRef :: IORef Bool
  , payloadRef :: IORef A.Value
  , fetchBlock :: MVar ()
  , fetchShouldFailRef :: IORef Bool
  , fetchDelayUsRef :: IORef Int
  , upsertCount :: IORef Int
  , notifyRef :: IORef [SseStatus]
  }

instance Eq TestCtx where
  a == b = clientId a == clientId b

instance CoveragePolicy TestCtx where
  coveredByRequest a b = clientId a == clientId b
  coveredByRepo ctx = readIORef (coveredByRepoRef ctx)

instance FetchPolicy TestCtx where
  type FetchResult TestCtx = [Int]
  fetchTask ctx = do
    takeMVar (fetchBlock ctx)
    delayUs <- readIORef (fetchDelayUsRef ctx)
    when (delayUs > 0) (threadDelay delayUs)
    shouldFail <- readIORef (fetchShouldFailRef ctx)
    if shouldFail then throwIO (userError "fetch failed") else pure [1, 2]

instance RepoPolicy TestCtx where
  type RepoPayload TestCtx = A.Value
  readFromRepo ctx = readIORef (payloadRef ctx)
  upsertFromFetch ctx _ = modifyIORef' (upsertCount ctx) (+1)

instance RespondPolicy TestCtx where
  type RespondPayload TestCtx = A.Value
  respondHttp _ complete payload =
    A.object [ "complete" .= complete, "payload" .= payload ]
  respondSse _ status =
    A.object [ "status" .= status ]

instance NotifyPolicy TestCtx where
  notifySse ctx status = modifyIORef' (notifyRef ctx) (<> [status])

mkCtx :: Text -> Bool -> IO TestCtx
mkCtx cid covered = do
  coveredRef <- newIORef covered
  payloadRef <- newIORef (A.String "ok")
  fetchBlock <- newEmptyMVar
  failRef <- newIORef False
  delayRef <- newIORef 0
  upsertRef <- newIORef 0
  notifyRef <- newIORef []
  pure TestCtx
    { clientId = cid
    , coveredByRepoRef = coveredRef
    , payloadRef = payloadRef
    , fetchBlock = fetchBlock
    , fetchShouldFailRef = failRef
    , fetchDelayUsRef = delayRef
    , upsertCount = upsertRef
    , notifyRef = notifyRef
    }

expectComplete :: Bool -> A.Value -> Expectation
expectComplete expected value =
  case value of
    A.Object obj ->
      case KM.lookup "complete" obj of
        Just (A.Bool b) -> b `shouldBe` expected
        _ -> expectationFailure "expected complete bool in response"
    _ -> expectationFailure "expected JSON object response"

waitForStatus :: (SseStatus -> Bool) -> IORef [SseStatus] -> IO ()
waitForStatus p ref = go (50 :: Int)
  where
    go 0 = expectationFailure "timeout waiting for SSE status"
    go n = do
      xs <- readIORef ref
      if any p xs
        then pure ()
        else threadDelay 20000 >> go (n - 1)

isFailed :: SseStatus -> Bool
isFailed status =
  case status of
    Failed _ -> True
    _ -> False

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "ServiceProcess beginServiceProcess" do
    it "returns http response immediately even when fetch is blocked" $ withContext do
      logger <- newLogger def
      let ?context = logger
      unique <- newUnique
      let cid = "client-http-response-" <> tshow (hashUnique unique)
      ctx <- mkCtx cid False
      done <- newEmptyMVar
      _ <- forkIO $ do
        resp <- beginServiceProcess ctx
        putMVar done resp
      let cleanup = do
            putMVar (fetchBlock ctx) ()
            _ <- takeMVar done
            pure ()
      (do
        threadDelay 20000
        result <- tryReadMVar done
        isJust result `shouldBe` True
        ) `finally` cleanup

    it "sets complete true when repo covers" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" True
      resp <- beginServiceProcess ctx
      expectComplete True resp

    it "sets complete false when repo does not cover" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False
      putMVar (fetchBlock ctx) ()
      resp <- beginServiceProcess ctx
      expectComplete False resp

    it "publishes duplicated while request is active" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False

      done <- newEmptyMVar
      _ <- forkIO (beginServiceProcess ctx >> putMVar done ())
      threadDelay 20000
      resp <- beginServiceProcess ctx
      expectComplete False resp
      waitForStatus (== Duplicated) (notifyRef ctx)

      putMVar (fetchBlock ctx) ()
      takeMVar done
      waitForStatus (== Success) (notifyRef ctx)

    it "publishes success when fetch succeeds" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False
      _ <- forkIO (beginServiceProcess ctx >> pure ())
      threadDelay 20000
      putMVar (fetchBlock ctx) ()
      waitForStatus (== Success) (notifyRef ctx)

    it "publishes failed when fetch throws" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False
      writeIORef (fetchShouldFailRef ctx) True
      _ <- forkIO (beginServiceProcess ctx >> pure ())
      threadDelay 20000
      putMVar (fetchBlock ctx) ()
      waitForStatus isFailed (notifyRef ctx)

    it "publishes failed on simulated network timeout" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False
      writeIORef (fetchShouldFailRef ctx) True
      writeIORef (fetchDelayUsRef ctx) 100000
      _ <- forkIO (beginServiceProcess ctx >> pure ())
      threadDelay 20000
      putMVar (fetchBlock ctx) ()
      waitForStatus isFailed (notifyRef ctx)

    it "does not publish duplicated after fetch completes" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False
      done <- newEmptyMVar
      _ <- forkIO (beginServiceProcess ctx >> putMVar done ())
      threadDelay 20000
      putMVar (fetchBlock ctx) ()
      takeMVar done
      writeIORef (coveredByRepoRef ctx) True
      _ <- beginServiceProcess ctx
      xs <- readIORef (notifyRef ctx)
      length (filter (== Duplicated) xs) `shouldBe` 0

    it "can simulate slow fetch with delay" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctx <- mkCtx "client-a" False
      writeIORef (fetchDelayUsRef ctx) 200000
      done <- newEmptyMVar
      _ <- forkIO (beginServiceProcess ctx >> putMVar done ())
      threadDelay 20000
      putMVar (fetchBlock ctx) ()
      waitForStatus (== Success) (notifyRef ctx)
      takeMVar done

    it "does not cross-notify between different clients" $ withContext do
      logger <- newLogger def
      let ?context = logger
      ctxA <- mkCtx "client-a" False
      ctxB <- mkCtx "client-b" False
      doneA <- newEmptyMVar
      doneB <- newEmptyMVar
      _ <- forkIO (beginServiceProcess ctxA >> putMVar doneA ())
      _ <- forkIO (beginServiceProcess ctxB >> putMVar doneB ())
      threadDelay 20000

      _ <- beginServiceProcess ctxA
      waitForStatus (== Duplicated) (notifyRef ctxA)
      xsB <- readIORef (notifyRef ctxB)
      length (filter (== Duplicated) xsB) `shouldBe` 0

      putMVar (fetchBlock ctxA) ()
      putMVar (fetchBlock ctxB) ()
      waitForStatus (== Success) (notifyRef ctxA)
      waitForStatus (== Success) (notifyRef ctxB)
      takeMVar doneA
      takeMVar doneB
