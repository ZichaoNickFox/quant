module Test.FRP.TreeSpec (tests) where

import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import FRP.Event (create, subscribe)
import FRP.Tree as TL
import Prelude
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual)

tests :: Spec Unit
tests = do
  it "isMoveAction distinguishes move vs non-move actions" do
    TL.isMoveAction (TL.ActionMove TL.MoveUp "n1") `shouldEqual` true
    TL.isMoveAction TL.ActionReload `shouldEqual` false
    TL.isMoveAction (TL.ActionAddChild "n1") `shouldEqual` false

  it "siblings filters by parent and sorts by node_order" do
    let xs = sampleNodes
    map _.id (TL.siblings xs (Just "p1")) `shouldEqual` ["n2", "n1", "n3"]
    map _.id (TL.siblings xs Nothing) `shouldEqual` ["p1", "root2"]

  it "nextOrder returns 1 for empty siblings and max+1 otherwise" do
    TL.nextOrder sampleNodes (Just "missing") `shouldEqual` 1
    TL.nextOrder sampleNodes (Just "p1") `shouldEqual` 4
    TL.nextOrder sampleNodes Nothing `shouldEqual` 3

  it "movePatches handles up/down swap under same parent" do
    let ss = TL.siblings sampleNodes (Just "p1")
    let n1 = findNode "n1" ss
    let up = maybe [] (TL.movePatches TL.MoveUp sampleNodes) n1
    up `shouldEqual`
      [ { treeId: "n1", parentTreeId: Just "p1", nodeOrder: 1 }
      , { treeId: "n2", parentTreeId: Just "p1", nodeOrder: 2 }
      ]
    let n2 = findNode "n2" ss
    let down = maybe [] (TL.movePatches TL.MoveDown sampleNodes) n2
    down `shouldEqual`
      [ { treeId: "n2", parentTreeId: Just "p1", nodeOrder: 2 }
      , { treeId: "n1", parentTreeId: Just "p1", nodeOrder: 1 }
      ]

  it "movePatches handles promote/demote re-parenting and boundaries" do
    let n4 = findNode "n4" sampleNodes
    let promote = maybe [] (TL.movePatches TL.Promote sampleNodes) n4
    promote `shouldEqual`
      [ { treeId: "n4", parentTreeId: Just "p1", nodeOrder: 3 } ]

    let n3 = findNode "n3" sampleNodes
    let demote = maybe [] (TL.movePatches TL.Demote sampleNodes) n3
    demote `shouldEqual`
      [ { treeId: "n3", parentTreeId: Just "n1", nodeOrder: 2 } ]

    let n2 = findNode "n2" sampleNodes
    let demoteBoundary = maybe [] (TL.movePatches TL.Demote sampleNodes) n2
    demoteBoundary `shouldEqual` []

  it "movePatches returns empty for boundaries and missing-parent cases" do
    let topNode = findNode "n2" sampleNodes
    let upBoundary = maybe [] (TL.movePatches TL.MoveUp sampleNodes) topNode
    upBoundary `shouldEqual` []

    let lastNode = findNode "n3" sampleNodes
    let downBoundary = maybe [] (TL.movePatches TL.MoveDown sampleNodes) lastNode
    downBoundary `shouldEqual` []

    let rootNode = findNode "p1" sampleNodes
    let promoteAtRoot = maybe [] (TL.movePatches TL.Promote sampleNodes) rootNode
    promoteAtRoot `shouldEqual` []

    let orphanNode = { id: "x", name: "x", nodeType: "file", parentTreeId: Just "missing", nodeOrder: 1 }
    TL.movePatches TL.Promote sampleNodes orphanNode `shouldEqual` []

  it "movePatches demote uses previous sibling as new parent and appends to its children" do
    let n3 = findNode "n3" sampleNodes
    let demote = maybe [] (TL.movePatches TL.Demote sampleNodes) n3
    demote `shouldEqual` [{ treeId: "n3", parentTreeId: Just "n1", nodeOrder: 2 }]

  it "promote places node immediately after its parent as sibling" do
    let n4 = findNode "n4" sampleNodes
    let promoted = maybe [] (TL.movePatches TL.Promote sampleNodes) n4
    promoted `shouldEqual`
      [ { treeId: "n4", parentTreeId: Just "p1", nodeOrder: 3 } ]

  it "deriveActionStreams routes direct actions and combines move with latest nodes" do
    { event: actionEvent, push: actionPush } <- liftEffect create
    { event: nodesEvent, push: nodesPush } <- liftEffect create
    let streams = TL.deriveActionStreams actionEvent nodesEvent

    directRef <- liftEffect $ Ref.new ([] :: Array TL.TreeAction)
    moveRef <- liftEffect $ Ref.new ([] :: Array (Tuple TL.TreeAction (Array TL.TreeNode)))
    _ <- liftEffect $ subscribe streams.directEvent \a -> Ref.modify_ (\xs -> Array.snoc xs a) directRef
    _ <- liftEffect $ subscribe streams.moveWithNodes \x -> Ref.modify_ (\xs -> Array.snoc xs x) moveRef

    liftEffect $ actionPush TL.ActionReload
    liftEffect $ actionPush (TL.ActionMove TL.MoveUp "n1")
    d1 <- liftEffect $ Ref.read directRef
    m1 <- liftEffect $ Ref.read moveRef
    d1 `shouldEqual` [TL.ActionReload]
    m1 `shouldEqual` []

    liftEffect $ nodesPush sampleNodes
    liftEffect $ actionPush (TL.ActionMove TL.MoveDown "n1")
    d2 <- liftEffect $ Ref.read directRef
    m2 <- liftEffect $ Ref.read moveRef
    d2 `shouldEqual` [TL.ActionReload]
    map (\(Tuple a ns) -> Tuple a (map _.id ns)) m2 `shouldEqual`
      [ Tuple (TL.ActionMove TL.MoveDown "n1") (map _.id sampleNodes) ]

  it "deriveActionStreams emits all non-move actions into directEvent" do
    { event: actionEvent, push: actionPush } <- liftEffect create
    { event: nodesEvent, push: _ } <- liftEffect create
    let streams = TL.deriveActionStreams actionEvent nodesEvent
    ref <- liftEffect $ Ref.new ([] :: Array TL.TreeAction)
    _ <- liftEffect $ subscribe streams.directEvent \a -> Ref.modify_ (\xs -> Array.snoc xs a) ref

    liftEffect $ actionPush TL.ActionReload
    liftEffect $ actionPush TL.ActionAddRoot
    liftEffect $ actionPush (TL.ActionAddChild "n1")
    liftEffect $ actionPush (TL.ActionDelete "n2")
    liftEffect $ actionPush (TL.ActionRename "n3" "new" (Just "p1") 3)
    xs <- liftEffect $ Ref.read ref
    xs `shouldEqual`
      [ TL.ActionReload
      , TL.ActionAddRoot
      , TL.ActionAddChild "n1"
      , TL.ActionDelete "n2"
      , TL.ActionRename "n3" "new" (Just "p1") 3
      ]

  it "createTreeOp connects push and subscribe for direct/move streams" do
    net <- liftEffect TL.createTreeOp
    directRef <- liftEffect $ Ref.new ([] :: Array TL.TreeAction)
    moveRef <- liftEffect $ Ref.new ([] :: Array (Tuple TL.TreeAction (Array TL.TreeNode)))
    _ <- liftEffect $ subscribe net.directEvent \a -> Ref.modify_ (\xs -> Array.snoc xs a) directRef
    _ <- liftEffect $ subscribe net.moveWithNodes \x -> Ref.modify_ (\xs -> Array.snoc xs x) moveRef

    liftEffect $ net.actionPush TL.ActionReload
    liftEffect $ net.actionPush (TL.ActionMove TL.MoveUp "n1")
    d1 <- liftEffect $ Ref.read directRef
    m1 <- liftEffect $ Ref.read moveRef
    d1 `shouldEqual` [TL.ActionReload]
    m1 `shouldEqual` []

    liftEffect $ net.nodesPush sampleNodes
    liftEffect $ net.actionPush (TL.ActionMove TL.MoveDown "n1")
    m2 <- liftEffect $ Ref.read moveRef
    map (\(Tuple a ns) -> Tuple a (map _.id ns)) m2 `shouldEqual`
      [ Tuple (TL.ActionMove TL.MoveDown "n1") (map _.id sampleNodes) ]

  it "createTreeOp uses latest nodes snapshot for move events" do
    net <- liftEffect TL.createTreeOp
    moveRef <- liftEffect $ Ref.new ([] :: Array (Tuple TL.TreeAction (Array TL.TreeNode)))
    _ <- liftEffect $ subscribe net.moveWithNodes \x -> Ref.modify_ (\xs -> Array.snoc xs x) moveRef

    liftEffect $ net.nodesPush sampleNodes
    liftEffect $ net.actionPush (TL.ActionMove TL.MoveUp "n1")

    let changedNodes = sampleNodes <> [ { id: "z", name: "z", nodeType: "file", parentTreeId: Nothing, nodeOrder: 99 } ]
    liftEffect $ net.nodesPush changedNodes
    liftEffect $ net.actionPush (TL.ActionMove TL.MoveDown "n1")

    xs <- liftEffect $ Ref.read moveRef
    map (\(Tuple a ns) -> Tuple a (map _.id ns)) xs `shouldEqual`
      [ Tuple (TL.ActionMove TL.MoveUp "n1") (map _.id sampleNodes)
      , Tuple (TL.ActionMove TL.MoveDown "n1") (map _.id changedNodes)
      ]

  it "blur confirms edit only when not already closed" do
    TL.shouldConfirmOnBlur false `shouldEqual` true
    TL.shouldConfirmOnBlur true `shouldEqual` false

  it "move button visibility hides up/right for first sibling and down for last sibling" do
    TL.moveButtonVisibility 0 2 `shouldEqual`
      { showUp: false, showDown: true, showRight: false }
    TL.moveButtonVisibility 1 2 `shouldEqual`
      { showUp: true, showDown: true, showRight: true }
    TL.moveButtonVisibility 2 2 `shouldEqual`
      { showUp: true, showDown: false, showRight: true }

sampleNodes :: Array TL.TreeNode
sampleNodes =
  [ mk "p1" "parent1" Nothing 1
  , mk "root2" "root2" Nothing 2
  , mk "n1" "node1" (Just "p1") 2
  , mk "n2" "node2" (Just "p1") 1
  , mk "n3" "node3" (Just "p1") 3
  , mk "n4" "node4" (Just "n1") 1
  ]
  where
  mk id name parentTreeId nodeOrder =
    { id, name, nodeType: "file", parentTreeId, nodeOrder }

findNode :: String -> Array TL.TreeNode -> Maybe TL.TreeNode
findNode nodeId = Array.find (\n -> n.id == nodeId)
