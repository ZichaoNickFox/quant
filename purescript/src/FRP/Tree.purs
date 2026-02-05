module FRP.Tree
  ( createTree
  , TreeNode
  , TreeUpdatePatch
  , MoveDir(..)
  , TreeAction(..)
  , createTreeOp
  , deriveActionStreams
  , isMoveAction
  , movePatches
  , siblings
  , nextOrder
  , TreeRootHandle
  , shouldConfirmOnBlur
  , moveButtonVisibility
  ) where

import Prelude

import Data.Array as A
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import FFI.Keyboard (isEnterKey)
import FRP as FRP
import FRP.Combinator as C
import FRP.InputText as InputText
import FRP.Event (Event)
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, setAttribute, toEventTarget, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.Window (document)

type MoveButtonVisibility =
  { showUp :: Boolean
  , showDown :: Boolean
  , showRight :: Boolean
  }

type TreeNode =
  { id :: String
  , name :: String
  , nodeType :: String
  , parentTreeId :: Maybe String
  , nodeOrder :: Int
  }

type TreeUpdatePatch =
  { treeId :: String
  , parentTreeId :: Maybe String
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
  | ActionAddRoot
  | ActionAddChild String
  | ActionDelete String
  | ActionRename String String (Maybe String) Int
  | ActionMove MoveDir String
derive instance eqTreeAction :: Eq TreeAction
instance showTreeAction :: Show TreeAction where
  show = case _ of
    ActionReload -> "ActionReload"
    ActionAddRoot -> "ActionAddRoot"
    ActionAddChild nodeId -> "(ActionAddChild " <> show nodeId <> ")"
    ActionDelete nodeId -> "(ActionDelete " <> show nodeId <> ")"
    ActionRename nodeId name parent order ->
      "(ActionRename " <> show nodeId <> " " <> show name <> " " <> show parent <> " " <> show order <> ")"
    ActionMove dir nodeId -> "(ActionMove " <> show dir <> " " <> show nodeId <> ")"

createTreeOp
  :: Effect
       { actionPush :: TreeAction -> Effect Unit
       , nodesPush :: Array TreeNode -> Effect Unit
       , directEvent :: Event TreeAction
       , moveWithNodes :: Event (Tuple TreeAction (Array TreeNode))
       }
createTreeOp = do
  { event: actionEvent, push: actionPush } <- FRP.create
  { event: nodesEvent, push: nodesPush } <- FRP.create
  let { directEvent, moveWithNodes } = deriveActionStreams actionEvent nodesEvent
  pure { actionPush, nodesPush, directEvent, moveWithNodes }

deriveActionStreams
  :: Event TreeAction
  -> Event (Array TreeNode)
  -> { directEvent :: Event TreeAction
     , moveWithNodes :: Event (Tuple TreeAction (Array TreeNode))
     }
deriveActionStreams actionEvent nodesEvent =
  let moveEvent = C.filter isMoveAction actionEvent
      moveWithNodes = C.withLatestFrom moveEvent nodesEvent
      directEvent = C.filter (not <<< isMoveAction) actionEvent
  in { directEvent, moveWithNodes }

isMoveAction :: TreeAction -> Boolean
isMoveAction action = case action of
  ActionMove _ _ -> true
  _ -> false

movePatches :: MoveDir -> Array TreeNode -> TreeNode -> Array TreeUpdatePatch
movePatches moveDir nodes node =
  case moveDir of
    MoveUp ->
      let ss = siblings nodes node.parentTreeId
      in case prevSibling node.id ss of
          Just prev ->
            [ mkPatch node.id node.parentTreeId prev.nodeOrder
            , mkPatch prev.id prev.parentTreeId node.nodeOrder
            ]
          Nothing -> []

    MoveDown ->
      let ss = siblings nodes node.parentTreeId
      in case nextSibling node.id ss of
          Just next ->
            [ mkPatch node.id node.parentTreeId next.nodeOrder
            , mkPatch next.id next.parentTreeId node.nodeOrder
            ]
          Nothing -> []

    Promote ->
      case node.parentTreeId of
        Nothing -> []
        Just parentId ->
          case A.find (\n -> n.id == parentId) nodes of
            Nothing -> []
            Just parentNode ->
              let newParent = parentNode.parentTreeId
                  newOrder = parentNode.nodeOrder + 1
              in [ mkPatch node.id newParent newOrder ]

    Demote ->
      let ss = siblings nodes node.parentTreeId
      in case prevSibling node.id ss of
          Just prev ->
            let newParent = Just prev.id
                newOrder = nextOrder nodes newParent
            in [ mkPatch node.id newParent newOrder ]
          Nothing -> []

mkPatch :: String -> Maybe String -> Int -> TreeUpdatePatch
mkPatch treeId parentTreeId nodeOrder = { treeId, parentTreeId, nodeOrder }

siblings :: Array TreeNode -> Maybe String -> Array TreeNode
siblings nodes parentId =
  A.sortBy (comparing _.nodeOrder) $ A.filter (\n -> n.parentTreeId == parentId) nodes

nextOrder :: Array TreeNode -> Maybe String -> Int
nextOrder nodes parentId =
  let ss = siblings nodes parentId
  in fromMaybe 1 ((\n -> n.nodeOrder + 1) <$> A.last ss)

prevSibling :: String -> Array TreeNode -> Maybe TreeNode
prevSibling nodeId ss =
  case A.findIndex (\n -> n.id == nodeId) ss of
    Just i | i > 0 -> A.index ss (i - 1)
    _ -> Nothing

nextSibling :: String -> Array TreeNode -> Maybe TreeNode
nextSibling nodeId ss =
  case A.findIndex (\n -> n.id == nodeId) ss of
    Just i -> A.index ss (i + 1)
    _ -> Nothing

type TreeRootHandle =
  { directActionEvent :: FRP.Event TreeAction
  , moveActionEvent :: FRP.Event (Tuple TreeAction (Array TreeNode))
  , selectedNodeEvent :: FRP.Event TreeNode
  , setNodes :: Array TreeNode -> Effect Unit
  , actionPush :: TreeAction -> Effect Unit
  }

createTree :: Element -> Array TreeNode -> Effect TreeRootHandle
createTree rootEl initialNodes = do
  win <- window
  htmlDoc <- document win
  let doc = HTMLDoc.toDocument htmlDoc
  { actionPush, nodesPush, directEvent, moveWithNodes } <- createTreeOp
  { event: selectedNodeEvent, push: selectedNodePush } <- FRP.create
  selectedNodeIdRef <- Ref.new Nothing
  currentNodesRef <- Ref.new ([] :: Array TreeNode)
  _ <- FRP.subscribe selectedNodeEvent \node -> do
    Ref.write (Just node.id) selectedNodeIdRef
  let setNodes nodes = do
        Ref.write nodes currentNodesRef
        nodesPush nodes
        selectedNodeId <- Ref.read selectedNodeIdRef
        renderTree doc rootEl nodes actionPush selectedNodePush selectedNodeId
        case selectedNodeId of
          Nothing -> pure unit
          Just nodeId ->
            case A.find (\n -> n.id == nodeId) nodes of
              Nothing -> pure unit
              Just selectedNode -> selectedNodePush selectedNode
  setNodes initialNodes
  pure
    { directActionEvent: directEvent
    , moveActionEvent: moveWithNodes
    , selectedNodeEvent
    , setNodes
    , actionPush
    }

renderTree
  :: Document
  -> Element
  -> Array TreeNode
  -> (TreeAction -> Effect Unit)
  -> (TreeNode -> Effect Unit)
  -> Maybe String
  -> Effect Unit
renderTree doc rootEl nodes actionPush selectedNodePush selectedNodeId = do
  setTextContent "" (toNode rootEl)

  rootRow <- createElement "div" doc
  setAttribute "style" "display:flex; align-items:center; gap:4px; margin:4px 0;" rootRow
  rootName <- createElement "span" doc
  setTextContent "Root" (toNode rootName)
  setAttribute "style" "min-width:120px; font-weight:600;" rootName
  rootAddBtn <- makeButton doc "+" (actionPush ActionAddRoot)
  _ <- appendChild (toNode rootName) (toNode rootRow)
  _ <- appendChild (toNode rootAddBtn) (toNode rootRow)
  _ <- appendChild (toNode rootRow) (toNode rootEl)

  listWrap <- createElement "div" doc
  renderChildren doc listWrap nodes Nothing 1 actionPush selectedNodePush selectedNodeId
  _ <- appendChild (toNode listWrap) (toNode rootEl)
  pure unit

renderChildren
  :: Document
  -> Element
  -> Array TreeNode
  -> Maybe String
  -> Int
  -> (TreeAction -> Effect Unit)
  -> (TreeNode -> Effect Unit)
  -> Maybe String
  -> Effect Unit
renderChildren doc container allNodes parentId depth actionPush selectedNodePush selectedNodeId = do
  let children = siblings allNodes parentId
      lastIndex = A.length children - 1
      canPromote = case parentId of
        Nothing -> false
        Just _ -> true
  for_ (A.mapWithIndex Tuple children) \(Tuple index node) -> do
    let visibility = moveButtonVisibility index lastIndex
    row <- createElement "div" doc
    setAttribute "style" ("display:flex; align-items:center; gap:4px; margin:4px 0; margin-left:" <> show (depth * 14) <> "px;") row

    nameWrap <- createElement "div" doc
    setAttribute "style" "min-width:120px;" nameWrap
    renderNodeName doc nameWrap node actionPush selectedNodePush selectedNodeId

    addChildBtn <- makeButton doc "+" (actionPush (ActionAddChild node.id))

    delBtn <- makeButton doc "-" (actionPush (ActionDelete node.id))

    upBtn <- if visibility.showUp
      then Just <$> makeButton doc "↑" (actionPush (ActionMove MoveUp node.id))
      else pure Nothing

    downBtn <- if visibility.showDown
      then Just <$> makeButton doc "↓" (actionPush (ActionMove MoveDown node.id))
      else pure Nothing

    promoteBtn <- if canPromote
      then Just <$> makeButton doc "←" (actionPush (ActionMove Promote node.id))
      else pure Nothing

    rightBtn <- if visibility.showRight
      then Just <$> makeButton doc "→" (actionPush (ActionMove Demote node.id))
      else pure Nothing

    _ <- appendChild (toNode nameWrap) (toNode row)
    _ <- appendChild (toNode addChildBtn) (toNode row)
    _ <- appendChild (toNode delBtn) (toNode row)
    for_ upBtn (\btn -> appendChild (toNode btn) (toNode row))
    for_ downBtn (\btn -> appendChild (toNode btn) (toNode row))
    for_ promoteBtn (\btn -> appendChild (toNode btn) (toNode row))
    for_ rightBtn (\btn -> appendChild (toNode btn) (toNode row))
    _ <- appendChild (toNode row) (toNode container)
    renderChildren doc container allNodes (Just node.id) (depth + 1) actionPush selectedNodePush selectedNodeId

renderNodeName
  :: Document
  -> Element
  -> TreeNode
  -> (TreeAction -> Effect Unit)
  -> (TreeNode -> Effect Unit)
  -> Maybe String
  -> Effect Unit
renderNodeName doc container node actionPush selectedNodePush selectedNodeId = do
  setTextContent "" (toNode container)
  nameSpan <- createElement "span" doc
  setTextContent node.name (toNode nameSpan)
  let selectedStyle = case selectedNodeId of
        Just nodeId | nodeId == node.id -> "font-weight:700; text-decoration:underline;"
        _ -> ""
  setAttribute "style" ("cursor:pointer;" <> selectedStyle) nameSpan
  onClick <- eventListener \_ -> selectedNodePush node
  onDoubleClick <- eventListener \_ -> renderNodeEditor doc container node actionPush selectedNodePush selectedNodeId
  addEventListener (EventType "click") onClick false (toEventTarget nameSpan)
  addEventListener (EventType "dblclick") onDoubleClick false (toEventTarget nameSpan)
  _ <- appendChild (toNode nameSpan) (toNode container)
  pure unit

renderNodeEditor
  :: Document
  -> Element
  -> TreeNode
  -> (TreeAction -> Effect Unit)
  -> (TreeNode -> Effect Unit)
  -> Maybe String
  -> Effect Unit
renderNodeEditor doc container node actionPush selectedNodePush selectedNodeId = do
  setTextContent "" (toNode container)
  inputEl <- createElement "input" doc
  setAttribute "type" "text" inputEl
  setAttribute "class" "form-control form-control-sm" inputEl
  setAttribute "style" "min-width:120px; max-width:220px;" inputEl
  _ <- appendChild (toNode inputEl) (toNode container)

  case HTMLInput.fromElement inputEl of
    Nothing -> renderNodeName doc container node actionPush selectedNodePush selectedNodeId
    Just input -> do
      { actionPush: inputActionPush, startedEvent, confirmedEvent } <- InputText.createInputText node.name
      closedRef <- Ref.new false
      _ <- FRP.subscribe startedEvent \initialName -> do
        HTMLInput.setValue initialName input
        HTMLElement.focus (HTMLInput.toHTMLElement input)
      _ <- FRP.subscribe confirmedEvent \newName -> do
        closed <- Ref.read closedRef
        when (not closed) do
          Ref.write true closedRef
          if newName == node.name || newName == ""
            then renderNodeName doc container node actionPush selectedNodePush selectedNodeId
            else do
              -- Optimistic DOM update so users see the renamed text immediately.
              renderNodeName doc container (node { name = newName }) actionPush selectedNodePush selectedNodeId
              actionPush (ActionRename node.id newName node.parentTreeId node.nodeOrder)
      inputActionPush (InputText.StartEdit node.name)

      onBlur <- eventListener \_ ->
        do
          closed <- Ref.read closedRef
          when (shouldConfirmOnBlur closed) do
            current <- HTMLInput.value input
            inputActionPush (InputText.ConfirmEdit current)

      onKeyDown <- eventListener \ev -> when (isEnterKey ev) do
        current <- HTMLInput.value input
        inputActionPush (InputText.ConfirmEdit current)

      addEventListener (EventType "blur") onBlur false (HTMLInput.toEventTarget input)
      addEventListener (EventType "keydown") onKeyDown false (HTMLInput.toEventTarget input)

shouldConfirmOnBlur :: Boolean -> Boolean
shouldConfirmOnBlur closed = not closed

moveButtonVisibility :: Int -> Int -> MoveButtonVisibility
moveButtonVisibility index lastIndex =
  { showUp: index > 0
  , showDown: index < lastIndex
  , showRight: index > 0
  }

makeButton :: Document -> String -> Effect Unit -> Effect Element
makeButton doc label onClick = do
  btn <- createElement "button" doc
  setAttribute "type" "button" btn
  setAttribute "class" "btn btn-sm btn-outline-secondary" btn
  setTextContent label (toNode btn)
  listener <- eventListener \_ -> onClick
  addEventListener (EventType "click") listener false (toEventTarget btn)
  pure btn
