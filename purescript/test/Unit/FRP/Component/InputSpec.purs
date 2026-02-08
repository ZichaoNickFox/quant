module Test.Unit.FRP.Component.InputSpec (tests) where

import Data.Array as Array
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Component.Input as I
import FRP.Event (create, subscribe)
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "createFRP emits startedEvent and confirmedEvent as a sub network" do
    net <- liftEffect $ I.createFRP "init"
    startedRef <- liftEffect $ Ref.new ([] :: Array String)
    confirmedRef <- liftEffect $ Ref.new ([] :: Array String)
    _ <- liftEffect $ subscribe net.startedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) startedRef
    _ <- liftEffect $ subscribe net.confirmedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) confirmedRef

    liftEffect $ net.actionPush (I.StartEdit "abc")
    liftEffect $ net.actionPush (I.ConfirmEdit "xyz")
    started <- liftEffect $ Ref.read startedRef
    confirmed <- liftEffect $ Ref.read confirmedRef
    started `shouldEqual` ["abc"]
    confirmed `shouldEqual` ["xyz"]

  it "start emits StartedEditing and confirm emits ConfirmedEditing" do
    { event, push } <- liftEffect create
    let out = I.deriveInputOutput event "init"
    ref <- liftEffect $ Ref.new ([] :: Array I.InputOutput)
    _ <- liftEffect $ subscribe out \x -> Ref.modify_ (\xs -> Array.snoc xs x) ref

    liftEffect $ push (I.StartEdit "init")
    liftEffect $ push (I.ConfirmEdit "new-name")
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual` [I.StartedEditing "init", I.ConfirmedEditing "new-name"]

  it "confirm without start still emits ConfirmedEditing" do
    { event, push } <- liftEffect create
    let out = I.deriveInputOutput event "init"
    ref <- liftEffect $ Ref.new ([] :: Array I.InputOutput)
    _ <- liftEffect $ subscribe out \x -> Ref.modify_ (\xs -> Array.snoc xs x) ref

    liftEffect $ push (I.ConfirmEdit "x")
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual` [I.ConfirmedEditing "x"]

  it "deriveInputOutput preserves event order across multiple starts/confirms" do
    { event, push } <- liftEffect create
    let out = I.deriveInputOutput event "init"
    ref <- liftEffect $ Ref.new ([] :: Array I.InputOutput)
    _ <- liftEffect $ subscribe out \x -> Ref.modify_ (\xs -> Array.snoc xs x) ref

    liftEffect $ push (I.StartEdit "v1")
    liftEffect $ push (I.StartEdit "v2")
    liftEffect $ push (I.ConfirmEdit "v2")
    liftEffect $ push (I.ConfirmEdit "v3")
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual`
      [ I.StartedEditing "v1"
      , I.StartedEditing "v2"
      , I.ConfirmedEditing "v2"
      , I.ConfirmedEditing "v3"
      ]

  it "createFRP keeps startedEvent and confirmedEvent separated" do
    net <- liftEffect $ I.createFRP "seed"
    startedRef <- liftEffect $ Ref.new ([] :: Array String)
    confirmedRef <- liftEffect $ Ref.new ([] :: Array String)
    _ <- liftEffect $ subscribe net.startedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) startedRef
    _ <- liftEffect $ subscribe net.confirmedEvent \x -> Ref.modify_ (\xs -> Array.snoc xs x) confirmedRef

    liftEffect $ net.actionPush (I.StartEdit "a")
    liftEffect $ net.actionPush (I.StartEdit "b")
    liftEffect $ net.actionPush (I.ConfirmEdit "c")
    started <- liftEffect $ Ref.read startedRef
    confirmed <- liftEffect $ Ref.read confirmedRef
    started `shouldEqual` ["a", "b"]
    confirmed `shouldEqual` ["c"]
