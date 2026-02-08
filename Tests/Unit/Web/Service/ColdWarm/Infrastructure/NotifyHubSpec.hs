{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Web.Service.ColdWarm.Infrastructure.NotifyHubSpec (tests) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (isEmptyMVar, newEmptyMVar, putMVar)
import Control.Concurrent.STM (atomically, readTChan)
import Data.ByteString (ByteString)
import Prelude
import Test.Hspec
import Web.Service.ColdWarm.Infrastructure.NotifyHub

tests :: Spec
tests = do
  describe "NotifyHub (unit)" do
    it "registerClient receives publishToClient" do
      chan <- registerClient "c1"
      publishToClient "c1" "hello"
      msg <- atomically $ readTChan chan
      msg `shouldBe` ("hello" :: ByteString)
      unregisterClient "c1"

    it "publishToAll delivers to all clients" do
      c1 <- registerClient "cA"
      c2 <- registerClient "cB"
      publishToAll "broadcast"
      m1 <- atomically $ readTChan c1
      m2 <- atomically $ readTChan c2
      m1 `shouldBe` ("broadcast" :: ByteString)
      m2 `shouldBe` ("broadcast" :: ByteString)
      unregisterClient "cA"
      unregisterClient "cB"

    it "unregisterClient drops empty topics" do
      c1 <- registerClient "cX"
      unregisterClient "cX"
      -- publish should not block or crash even if client is gone
      publishToClient "cX" "noop"
      -- ensure no message arrives (non-blocking check)
      done <- newEmptyMVar
      _ <- forkIO $ do
        _ <- atomically $ readTChan c1
        putMVar done True
      threadDelay 50000
      empty <- isEmptyMVar done
      empty `shouldBe` True
