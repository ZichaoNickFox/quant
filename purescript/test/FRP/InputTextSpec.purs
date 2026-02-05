module Test.FRP.InputTextSpec (tests) where

import Data.Array as Array
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (create, subscribe)
import FRP.InputText as IT
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "createInputText emits startedEvent and confirmedEvent as a sub network" do
    net <- liftEffect $ IT.createInputText "init"
    startedRef <- liftEffect $ Ref.new ([] :: Array String)
    confirmedRef <- liftEffect $ Ref.new ([] :: Array String)
    _ <- liftEffect $ subscribe net.startedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) startedRef
    _ <- liftEffect $ subscribe net.confirmedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) confirmedRef

    liftEffect $ net.actionPush (IT.StartEdit "abc")
    liftEffect $ net.actionPush (IT.ConfirmEdit "xyz")
    started <- liftEffect $ Ref.read startedRef
    confirmed <- liftEffect $ Ref.read confirmedRef
    started `shouldEqual` ["abc"]
    confirmed `shouldEqual` ["xyz"]

  it "start emits StartedEditing and confirm emits ConfirmedEditing" do
    { event, push } <- liftEffect create
    let out = IT.deriveInputTextOutput event "init"
    ref <- liftEffect $ Ref.new ([] :: Array IT.InputTextOutput)
    _ <- liftEffect $ subscribe out \x -> Ref.modify_ (\xs -> Array.snoc xs x) ref

    liftEffect $ push (IT.StartEdit "init")
    liftEffect $ push (IT.ConfirmEdit "new-name")
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual` [IT.StartedEditing "init", IT.ConfirmedEditing "new-name"]

  it "confirm without start still emits ConfirmedEditing" do
    { event, push } <- liftEffect create
    let out = IT.deriveInputTextOutput event "init"
    ref <- liftEffect $ Ref.new ([] :: Array IT.InputTextOutput)
    _ <- liftEffect $ subscribe out \x -> Ref.modify_ (\xs -> Array.snoc xs x) ref

    liftEffect $ push (IT.ConfirmEdit "x")
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual` [IT.ConfirmedEditing "x"]

  it "deriveInputTextOutput preserves event order across multiple starts/confirms" do
    { event, push } <- liftEffect create
    let out = IT.deriveInputTextOutput event "init"
    ref <- liftEffect $ Ref.new ([] :: Array IT.InputTextOutput)
    _ <- liftEffect $ subscribe out \x -> Ref.modify_ (\xs -> Array.snoc xs x) ref

    liftEffect $ push (IT.StartEdit "v1")
    liftEffect $ push (IT.StartEdit "v2")
    liftEffect $ push (IT.ConfirmEdit "v2")
    liftEffect $ push (IT.ConfirmEdit "v3")
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual`
      [ IT.StartedEditing "v1"
      , IT.StartedEditing "v2"
      , IT.ConfirmedEditing "v2"
      , IT.ConfirmedEditing "v3"
      ]

  it "createInputText keeps startedEvent and confirmedEvent separated" do
    net <- liftEffect $ IT.createInputText "seed"
    startedRef <- liftEffect $ Ref.new ([] :: Array String)
    confirmedRef <- liftEffect $ Ref.new ([] :: Array String)
    _ <- liftEffect $ subscribe net.startedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) startedRef
    _ <- liftEffect $ subscribe net.confirmedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) confirmedRef

    liftEffect $ net.actionPush (IT.StartEdit "a")
    liftEffect $ net.actionPush (IT.StartEdit "b")
    liftEffect $ net.actionPush (IT.ConfirmEdit "c")
    started <- liftEffect $ Ref.read startedRef
    confirmed <- liftEffect $ Ref.read confirmedRef
    started `shouldEqual` ["a", "b"]
    confirmed `shouldEqual` ["c"]
