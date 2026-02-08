module FRP.Component.Tree
  ( Config
  , Events
  , MoveDir(..)
  , NodeConfig
  , TreeAction(..)
  , TreeNode
  , TreeUpdatePatch
  , createFRP
  , movePatches
  ) where

import Data.Array as A

import Data.Filterable (filterMap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import FRP as FRP
import FRP.Combinator as C
import Prelude
import Prim.Row as Row
import Web.DOM.Document (createElement)
import Web.DOM.Element (Element, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

type Events payload nodeEvents =
  { reloadPush :: Effect Unit
  , addChildPush :: String -> Effect Unit  -- String is external node id (externalId)
  , deletePush :: String -> Effect Unit    -- String is external node id (externalId)
  , moveUpPush :: String -> Effect Unit    -- String is external node id (externalId)
  , moveDownPush :: String -> Effect Unit  -- String is external node id (externalId)
  , promotePush :: String -> Effect Unit   -- String is external node id (externalId)
  , demotePush :: String -> Effect Unit    -- String is external node id (externalId)
  , setNodesPush :: Array (TreeNode payload) -> Effect Unit
  , selectNodePush :: TreeNode payload -> Effect Unit
  , onReloadSubs :: FRP.Event Unit
  , onAddChildSubs :: FRP.Event String    -- String is external node id (externalId)
  , onDeleteSubs :: FRP.Event String      -- String is external node id (externalId)
  , onMoveUpSubs :: FRP.Event (Tuple String (Array (TreeNode payload)))    -- String is external node id
  , onMoveDownSubs :: FRP.Event (Tuple String (Array (TreeNode payload)))  -- String is external node id
  , onPromoteSubs :: FRP.Event (Tuple String (Array (TreeNode payload)))   -- String is external node id
  , onDemoteSubs :: FRP.Event (Tuple String (Array (TreeNode payload)))    -- String is external node id
  , onNodeSelected :: FRP.Event (TreeNode payload)
  , onNodeCreatedSubs :: FRP.Event { node :: Maybe (TreeNode payload), nodeEvents :: { element :: Element | nodeEvents }, container :: Element }
  }
type TreeNode payload =
  { externalId :: String  -- External id (must be unique across all nodes)
  , nodeType :: String
  , parentExternalId :: Maybe String
  , nodeOrder :: Int
  , payload :: payload
  }

type NodeConfig payload =
  { node :: Maybe (TreeNode payload)
  , depth :: Int
  , index :: Int
  , lastIndex :: Int
  , canPromote :: Boolean
  , rootName :: String
  , selectedExternalId :: Maybe String
  }

type Config payload nodeEvents =
  { createNodeFRP :: NodeConfig payload -> Effect { element :: Element | nodeEvents }
  , rootName :: String
  }

createFRP
  :: forall payload nodeEvents
   . Element
  -> Array (TreeNode payload)
  -> Config payload nodeEvents
  -> Effect (Events payload nodeEvents)
createFRP rootEl initialNodes config = do
  win <- window
  htmlDoc <- document win
  let doc = HTMLDoc.toDocument htmlDoc
  { event: inputEvent, push: inputPush } <- FRP.create
  { event: nodesEvent, push: nodesPush } <- FRP.create
  { event: onNodeCreatedSubs, push: pushNodeCreated } <- FRP.create
  let moveEvent = C.filter (\action -> case action of
        ActionMove _ _ -> true
        _ -> false
      ) inputEvent
  let moveWithNodes = C.withLatestFrom moveEvent nodesEvent
  let directEvent = C.filter (\action -> case action of
        ActionMove _ _ -> false
        _ -> true
      ) inputEvent
  let onReloadSubs = filterMap (\action -> case action of
        ActionReload -> Just unit
        _ -> Nothing
      ) directEvent
  let onAddChildSubs = filterMap (\action -> case action of
        ActionAddChild nodeId -> Just nodeId
        _ -> Nothing
      ) directEvent
  let onDeleteSubs = filterMap (\action -> case action of
        ActionDelete nodeId -> Just nodeId
        _ -> Nothing
      ) directEvent
  let onMoveUpSubs = filterMap (\(Tuple action nodes) -> case action of
        ActionMove MoveUp nodeId -> Just (Tuple nodeId nodes)
        _ -> Nothing
      ) moveWithNodes
  let onMoveDownSubs = filterMap (\(Tuple action nodes) -> case action of
        ActionMove MoveDown nodeId -> Just (Tuple nodeId nodes)
        _ -> Nothing
      ) moveWithNodes
  let onPromoteSubs = filterMap (\(Tuple action nodes) -> case action of
        ActionMove Promote nodeId -> Just (Tuple nodeId nodes)
        _ -> Nothing
      ) moveWithNodes
  let onDemoteSubs = filterMap (\(Tuple action nodes) -> case action of
        ActionMove Demote nodeId -> Just (Tuple nodeId nodes)
        _ -> Nothing
      ) moveWithNodes
  { event: onNodeSelected, push: selectedNodePush } <- FRP.create
  selectedExternalIdRef <- Ref.new Nothing
  currentNodesRef <- Ref.new ([] :: Array (TreeNode payload))
  let renderNodes nodes selectedExternalId = do
        let renderChildren container allNodes parentId depth = do
              let children = siblings allNodes parentId
                  lastIndex = A.length children - 1
                  canPromote = case parentId of
                    Nothing -> false
                    Just _ -> true
              for_ (A.mapWithIndex Tuple children) \(Tuple index node) -> do
                nodeEvents <- config.createNodeFRP
                  { node: Just node
                  , depth
                  , index
                  , lastIndex
                  , canPromote
                  , rootName: config.rootName
                  , selectedExternalId
                  }
                _ <- appendChild (toNode nodeEvents.element) (toNode container)
                pushNodeCreated { node: Just node, nodeEvents, container }
                renderChildren container allNodes (Just node.externalId) (depth + 1)
              pure unit
            renderTree = do
              setTextContent "" (toNode rootEl)
              rootNodeEvents <- config.createNodeFRP
                { node: Nothing
                , depth: 0
                , index: 0
                , lastIndex: 0
                , canPromote: false
                , rootName: config.rootName
                , selectedExternalId
                }
              _ <- appendChild (toNode rootNodeEvents.element) (toNode rootEl)
              pushNodeCreated { node: Nothing, nodeEvents: rootNodeEvents, container: rootEl }
              listWrap <- createElement "div" doc
              renderChildren listWrap nodes Nothing 1
              _ <- appendChild (toNode listWrap) (toNode rootEl)
              pure unit
        renderTree
  let setNodes nodes = do
        Ref.write nodes currentNodesRef
        nodesPush nodes
        selectedExternalId <- Ref.read selectedExternalIdRef
        renderNodes nodes selectedExternalId
        case selectedExternalId of
          Nothing -> pure unit
          Just nodeId ->
            case A.find (\n -> n.externalId == nodeId) nodes of
              Nothing -> pure unit
              Just selectedNode -> selectedNodePush selectedNode
  _ <- FRP.subscribe onNodeSelected \node -> do
    Ref.write (Just node.externalId) selectedExternalIdRef
    nodes <- Ref.read currentNodesRef
    renderNodes nodes (Just node.externalId)
  setNodes initialNodes
  let reloadPush = inputPush ActionReload
  let addChildPush nodeId = inputPush (ActionAddChild nodeId)
  let deletePush nodeId = inputPush (ActionDelete nodeId)
  let moveUpPush nodeId = inputPush (ActionMove MoveUp nodeId)
  let moveDownPush nodeId = inputPush (ActionMove MoveDown nodeId)
  let promotePush nodeId = inputPush (ActionMove Promote nodeId)
  let demotePush nodeId = inputPush (ActionMove Demote nodeId)
  pure
    { reloadPush
    , addChildPush
    , deletePush
    , moveUpPush
    , moveDownPush
    , promotePush
    , demotePush
    , setNodesPush: setNodes
    , selectNodePush: selectedNodePush
    , onReloadSubs
    , onAddChildSubs
    , onDeleteSubs
    , onMoveUpSubs
    , onMoveDownSubs
    , onPromoteSubs
    , onDemoteSubs
    , onNodeSelected
    , onNodeCreatedSubs
    }

type TreeUpdatePatch =
  { externalId :: String  -- External id (must be unique)
  , parentExternalId :: Maybe String
  , nodeOrder :: Int
  }

data MoveDir
  = MoveUp
  | MoveDown
  | Promote
  | Demote
derive instance eqMoveDir :: Eq MoveDir
instance showMoveDir :: Show MoveDir where
  show = case _ of
    MoveUp -> "MoveUp"
    MoveDown -> "MoveDown"
    Promote -> "Promote"
    Demote -> "Demote"

data TreeAction
  = ActionReload
  | ActionAddChild String
  | ActionDelete String
  | ActionRename String String (Maybe String) Int
  | ActionMove MoveDir String
derive instance eqTreeAction :: Eq TreeAction
instance showTreeAction :: Show TreeAction where
  show = case _ of
    ActionReload -> "ActionReload"
    ActionAddChild nodeId -> "(ActionAddChild " <> show nodeId <> ")"
    ActionDelete nodeId -> "(ActionDelete " <> show nodeId <> ")"
    ActionRename nodeId name parent order ->
      "(ActionRename " <> show nodeId <> " " <> show name <> " " <> show parent <> " " <> show order <> ")"
    ActionMove dir nodeId -> "(ActionMove " <> show dir <> " " <> show nodeId <> ")"

movePatches :: forall payload. MoveDir -> Array (TreeNode payload) -> TreeNode payload -> Array TreeUpdatePatch
movePatches moveDir nodes node =
  case moveDir of
    MoveUp ->
      let ss = siblings nodes node.parentExternalId
      in case prevSibling node.externalId ss of
          Just prev ->
            [ mkPatch node.externalId node.parentExternalId prev.nodeOrder
            , mkPatch prev.externalId prev.parentExternalId node.nodeOrder
            ]
          Nothing -> []

    MoveDown ->
      let ss = siblings nodes node.parentExternalId
      in case nextSibling node.externalId ss of
          Just next ->
            [ mkPatch node.externalId node.parentExternalId next.nodeOrder
            , mkPatch next.externalId next.parentExternalId node.nodeOrder
            ]
          Nothing -> []

    Promote ->
      case node.parentExternalId of
        Nothing -> []
        Just parentId ->
          case A.find (\n -> n.externalId == parentId) nodes of
            Nothing -> []
            Just parentNode ->
              let newParent = parentNode.parentExternalId
                  newOrder = parentNode.nodeOrder + 1
              in [ mkPatch node.externalId newParent newOrder ]

    Demote ->
      let ss = siblings nodes node.parentExternalId
      in case prevSibling node.externalId ss of
          Just prev ->
            let newParent = Just prev.externalId
                newOrder = nextOrder nodes newParent
            in [ mkPatch node.externalId newParent newOrder ]
          Nothing -> []

mkPatch :: String -> Maybe String -> Int -> TreeUpdatePatch
mkPatch externalId parentExternalId nodeOrder = { externalId, parentExternalId, nodeOrder }

siblings :: forall payload. Array (TreeNode payload) -> Maybe String -> Array (TreeNode payload)
siblings nodes parentId =
  A.sortBy (comparing _.nodeOrder) $ A.filter (\n -> n.parentExternalId == parentId) nodes

nextOrder :: forall payload. Array (TreeNode payload) -> Maybe String -> Int
nextOrder nodes parentId =
  let ss = siblings nodes parentId
  in fromMaybe 1 ((\n -> n.nodeOrder + 1) <$> A.last ss)

prevSibling :: forall payload. String -> Array (TreeNode payload) -> Maybe (TreeNode payload)
prevSibling nodeId ss =
  case A.findIndex (\n -> n.externalId == nodeId) ss of
    Just i | i > 0 -> A.index ss (i - 1)
    _ -> Nothing

nextSibling :: forall payload. String -> Array (TreeNode payload) -> Maybe (TreeNode payload)
nextSibling nodeId ss =
  case A.findIndex (\n -> n.externalId == nodeId) ss of
    Just i -> A.index ss (i + 1)
    _ -> Nothing
