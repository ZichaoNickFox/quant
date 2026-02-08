{-# LANGUAGE OverloadedStrings #-}
module Tests.Integration.Web.Controller.NotifyControllerSpec (tests) where

import Config (config)
import Control.Monad (when)
import Data.Maybe (isNothing)
import IHP.Hspec (withIHPApp)
import IHP.Test.Mocking (callActionWithParams, withContext)
import Prelude
import System.Timeout (timeout)
import Test.Hspec
import Web.Controller.NotifyController ()
import Web.FrontController ()
import Web.Types (NotifyController (..), WebApplication (..))

tests :: Spec
tests = aroundAll (withIHPApp WebApplication config) do
  describe "NotifyController (integration)" do
    it "keeps the SSE stream open (callActionWithParams times out)" $ \ctx ->
      withContext
        (do
          result <- timeout 200000 $ callActionWithParams NotifyAction [("clientId", "test-client")]
          when (not (isNothing result)) $
            expectationFailure "expected timeout while streaming"
        )
        ctx
