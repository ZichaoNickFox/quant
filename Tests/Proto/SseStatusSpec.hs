{-# LANGUAGE OverloadedStrings #-}
module Tests.Proto.SseStatusSpec (tests) where

import qualified Data.Aeson as A
import Prelude
import Proto.SseStatus (SseStatus (..), sseStatusFromText, sseStatusToText)
import Test.Hspec

tests :: Spec
tests = do
  describe "Proto.SseStatus" do
    it "sseStatusToText" do
      sseStatusToText Success `shouldBe` "success"
      sseStatusToText Duplicated `shouldBe` "duplicated"
      sseStatusToText Failed `shouldBe` "failed"

    it "sseStatusFromText (case-insensitive + duplicate alias)" do
      sseStatusFromText "success" `shouldBe` Just Success
      sseStatusFromText "SUCCESS" `shouldBe` Just Success
      sseStatusFromText "duplicate" `shouldBe` Just Duplicated
      sseStatusFromText "duplicated" `shouldBe` Just Duplicated
      sseStatusFromText "failed" `shouldBe` Just Failed
      sseStatusFromText "unknown" `shouldBe` Nothing

    it "ToJSON/FromJSON roundtrip" do
      let values = [Success, Duplicated, Failed]
      mapM_ (\v -> A.decode (A.encode v) `shouldBe` Just v) values
