module Test.Unit.Common.TreeNodeWidgetSpec (tests) where

import Common.TreeNodeWidget as W
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event as E
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Unit.FFI.FakeDom as FakeDom

tests :: Spec Unit
tests = do
  it "clicking node name emits onSelect only once" do
    liftEffect FakeDom.installFakeDom
    countRef <- liftEffect (Ref.new 0)
    widget <- liftEffect $ W.createFRP
      { node: Just
          { externalId: "n1"
          , parentExternalId: Nothing
          , nodeOrder: 1
          , payload: { ownerId: "o1", name: "Node-1" }
          }
      , depth: 1
      , index: 0
      , lastIndex: 0
      , canPromote: false
      , rootName: "策略"
      , selectedExternalId: Nothing
      }
    _ <- liftEffect $ E.subscribe widget.onSelect \_ ->
      Ref.modify_ (_ + 1) countRef

    liftEffect (FakeDom.clickFirstSpan widget.element)

    count <- liftEffect (Ref.read countRef)
    count `shouldEqual` 1
