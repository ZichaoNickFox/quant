{-# LANGUAGE OverloadedStrings #-}
module Tests.Unit.Proto.SseStatusSpec (tests) where

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
      sseStatusToText (Failed "err") `shouldBe` "failed"

    it "sseStatusFromText (case-insensitive)" do
      sseStatusFromText "success" `shouldBe` Just Success
      sseStatusFromText "SUCCESS" `shouldBe` Just Success
      sseStatusFromText "duplicated" `shouldBe` Just Duplicated
      sseStatusFromText "failed" `shouldBe` Just (Failed "")
      sseStatusFromText "unknown" `shouldBe` Nothing

    it "ToJSON/FromJSON roundtrip" do
      let values = [Success, Duplicated, Failed "err"]
      mapM_ (\v -> A.decode (A.encode v) `shouldBe` Just v) values
