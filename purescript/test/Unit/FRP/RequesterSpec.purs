module Test.Unit.FRP.RequesterSpec (tests) where

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import FRP.Event (Event, subscribe)
import FRP.Requester.Requester (createFRP)
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

awaitOnceWith :: forall a. Event a -> Effect Unit -> Aff a
awaitOnceWith ev fire = makeAff \k -> do
  _ <- subscribe ev \a -> k (Right a)
  fire
  pure nonCanceler

tests :: Spec Unit
tests = do
  it "createFRP emits response for request" do
    { requestPush, responseEvent } <-
      liftEffect $ createFRP "[req]" "[resp]" (\r -> pure (r <> "-ok"))
    resp <- awaitOnceWith responseEvent (requestPush "ping")
    resp `shouldEqual` "ping-ok"

  it "createFRP supports multiple requests" do
    { requestPush, responseEvent } <-
      liftEffect $ createFRP "[req]" "[resp]" (\r -> pure (r <> "-ok"))
    _ <- awaitOnceWith responseEvent (requestPush "a")
    resp <- awaitOnceWith responseEvent (requestPush "b")
    resp `shouldEqual` "b-ok"
