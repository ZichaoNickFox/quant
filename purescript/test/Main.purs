module Test.Main where

import Prelude

import FRP as FRP
import Data.Array as Array
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import FRP.Event (create, subscribe)

assertEqual :: forall a. Eq a => Show a => String -> a -> a -> Effect Unit
assertEqual label expected actual =
  if expected == actual then
    log ("[PASS] " <> label)
  else
    throw ("[FAIL] " <> label <> " expected=" <> show expected <> " actual=" <> show actual)

testBeginEvent :: Effect Unit
testBeginEvent = do
  { event: a, push: pushA } <- create
  { event: b, push: pushB } <- create
  let begin = FRP.take1 $ FRP.map (const unit) $ FRP.combineLatest2 a b
  outRef <- Ref.new ([] :: Array Unit)
  _ <- subscribe begin \u -> Ref.modify_ (\xs -> Array.snoc xs u) outRef

  pushA unit
  xs1 <- Ref.read outRef
  assertEqual "no output after only A" [] xs1

  pushB unit
  xs2 <- Ref.read outRef
  assertEqual "output after A+B" [ unit ] xs2

  pushB unit
  xs3 <- Ref.read outRef
  assertEqual "still one output after extra B" [ unit ] xs3

  pure unit

main :: Effect Unit
main = do
  testBeginEvent
  log "All tests passed."
