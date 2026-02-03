module Test.FRP.CombinatorSpec (tests) where

import Data.Array as Array
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP as FRP
import FRP.Event (create, subscribe)
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "combineLatest2 emits after both streams" do
    { event: a, push: pushA } <- liftEffect create
    { event: b, push: pushB } <- liftEffect create
    let combined = FRP.combineLatest2 a b
    outRef <- liftEffect $ Ref.new ([] :: Array (Tuple Unit Unit))
    _ <- liftEffect $ subscribe combined \u -> Ref.modify_ (\xs -> Array.snoc xs u) outRef

    liftEffect $ pushA unit
    xs1 <- liftEffect $ Ref.read outRef
    xs1 `shouldEqual` []

    liftEffect $ pushB unit
    xs2 <- liftEffect $ Ref.read outRef
    xs2 `shouldEqual` [ Tuple unit unit ]

  it "take1 only emits once" do
    { event: e, push } <- liftEffect create
    let once = FRP.take1 e
    outRef <- liftEffect $ Ref.new ([] :: Array Unit)
    _ <- liftEffect $ subscribe once \u -> Ref.modify_ (\xs -> Array.snoc xs u) outRef

    liftEffect $ push unit
    liftEffect $ push unit
    xs <- liftEffect $ Ref.read outRef
    xs `shouldEqual` [ unit ]
