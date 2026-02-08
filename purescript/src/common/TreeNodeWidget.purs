module Common.TreeNodeWidget
  ( createFRP
  , makeButton
  , renderNodeEditor
  , renderNodeName
  , TreeNodePayload
  ) where

import Data.Foldable (for_)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as Ref
import FFI.Keyboard (isEnterKey)
import FRP as FRP
import FRP.Component.Input as Input
import Prelude
import Web.DOM.Document (Document, createElement)
import Web.DOM.Element (Element, setAttribute, toEventTarget, toNode)
import Web.DOM.Node (appendChild, setTextContent)
import Web.Event.Event (EventType(..), stopPropagation)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInput
import Web.HTML.Window (document)

type TreeNodePayload =
  { ownerId :: String
  , name :: String
  }

type MoveButtonVisibility =
  { showUp :: Boolean
  , showDown :: Boolean
  , showRight :: Boolean
  }

moveButtonVisibility :: Int -> Int -> MoveButtonVisibility
moveButtonVisibility index lastIndex =
  { showUp: index > 0
  , showDown: index < lastIndex
  , showRight: index > 0
  }

renderNodeName
  :: forall node
   . Document
  -> Element
  -> Int
  -> Int
  -> { externalId :: String
     , parentExternalId :: Maybe String
     , nodeOrder :: Int
     , payload :: TreeNodePayload
     | node
     }
  -> { onRename :: String -> String -> Maybe String -> Int -> Effect Unit
     , onSelect :: { externalId :: String
                   , parentExternalId :: Maybe String
                   , nodeOrder :: Int
                   , payload :: TreeNodePayload
                   | node
                   } -> Effect Unit
     }
  -> Maybe String
  -> Effect Unit
renderNodeName doc container depth index node handlers selectedNodeId = do
  setTextContent "" (toNode container)
  nameSpan <- createElement "span" doc
  let displayName = if node.payload.name == ""
        then "NewNode"
        else node.payload.name
  setTextContent displayName (toNode nameSpan)
  let selectedStyle = case selectedNodeId of
        Just nodeId | nodeId == node.externalId -> "font-weight:700;"
        _ -> ""
  setAttribute "style" ("cursor:pointer;" <> selectedStyle) nameSpan
  onClick <- eventListener \ev -> do
    stopPropagation ev
    handlers.onSelect node
  onDoubleClick <- eventListener \ev -> do
    stopPropagation ev
    renderNodeEditor doc container depth index node handlers selectedNodeId
  addEventListener (EventType "click") onClick false (toEventTarget nameSpan)
  addEventListener (EventType "dblclick") onDoubleClick false (toEventTarget nameSpan)
  _ <- appendChild (toNode nameSpan) (toNode container)
  pure unit

renderNodeEditor
  :: forall node
   . Document
  -> Element
  -> Int
  -> Int
  -> { externalId :: String, parentExternalId :: Maybe String, nodeOrder :: Int, payload :: TreeNodePayload | node }
  -> { onRename :: String -> String -> Maybe String -> Int -> Effect Unit
     , onSelect :: { externalId :: String, parentExternalId :: Maybe String, nodeOrder :: Int, payload :: TreeNodePayload | node } -> Effect Unit
     }
  -> Maybe String
  -> Effect Unit
renderNodeEditor doc container depth index node handlers selectedNodeId = do
  setTextContent "" (toNode container)
  inputEl <- createElement "input" doc
  setAttribute "type" "text" inputEl
  setAttribute "class" "form-control form-control-sm" inputEl
  setAttribute "style" "min-width:120px; max-width:220px;" inputEl
  _ <- appendChild (toNode inputEl) (toNode container)

  case HTMLInput.fromElement inputEl of
    Nothing -> renderNodeName doc container depth index node handlers selectedNodeId
    Just input -> do
      { actionPush: inputActionPush, startedEvent, confirmedEvent } <- Input.createFRP node.payload.name
      closedRef <- Ref.new false
      _ <- FRP.subscribe startedEvent \initialName -> do
        HTMLInput.setValue initialName input
        HTMLElement.focus (HTMLInput.toHTMLElement input)
      _ <- FRP.subscribe confirmedEvent \newName -> do
        closed <- Ref.read closedRef
        when (not closed) do
          Ref.write true closedRef
          if newName == node.payload.name || newName == ""
            then renderNodeName doc container depth index node handlers selectedNodeId
            else do
              renderNodeName doc container depth index (node { payload = node.payload { name = newName } }) handlers selectedNodeId
              handlers.onRename node.externalId newName node.parentExternalId node.nodeOrder
      inputActionPush (Input.StartEdit node.payload.name)

      onBlur <- eventListener \_ ->
        do
          closed <- Ref.read closedRef
          when (not closed) do
            current <- HTMLInput.value input
            inputActionPush (Input.ConfirmEdit current)

      onKeyDown <- eventListener \ev -> when (isEnterKey ev) do
        current <- HTMLInput.value input
        inputActionPush (Input.ConfirmEdit current)

      addEventListener (EventType "blur") onBlur false (HTMLInput.toEventTarget input)
      addEventListener (EventType "keydown") onKeyDown false (HTMLInput.toEventTarget input)

createFRP
  :: forall node
   . { node :: Maybe { externalId :: String, parentExternalId :: Maybe String, nodeOrder :: Int, payload :: TreeNodePayload | node }
     , depth :: Int
     , index :: Int
     , lastIndex :: Int
     , canPromote :: Boolean
     , rootName :: String
     , selectedExternalId :: Maybe String
     }
  -> Effect { element :: Element
            , onAddChild :: FRP.Event String
            , onDelete :: FRP.Event String
            , onRename :: FRP.Event { nodeId :: String, name :: String, parentExternalId :: Maybe String, nodeOrder :: Int }
            , onMoveUp :: FRP.Event String
            , onMoveDown :: FRP.Event String
            , onPromote :: FRP.Event String
            , onDemote :: FRP.Event String
            , onSelect :: FRP.Event { externalId :: String, parentExternalId :: Maybe String, nodeOrder :: Int, payload :: TreeNodePayload | node }
            }
createFRP ctx = do
  win <- window
  htmlDoc <- document win
  let doc = HTMLDoc.toDocument htmlDoc
  { event: onAddChild, push: pushAddChild } <- FRP.create
  { event: onDelete, push: pushDelete } <- FRP.create
  { event: onRename, push: pushRename } <- FRP.create
  { event: onMoveUp, push: pushMoveUp } <- FRP.create
  { event: onMoveDown, push: pushMoveDown } <- FRP.create
  { event: onPromote, push: pushPromote } <- FRP.create
  { event: onDemote, push: pushDemote } <- FRP.create
  { event: onSelect, push: pushSelect } <- FRP.create
  row <- case ctx.node of
    Nothing | ctx.depth == 0 -> do
      rootRow <- createElement "div" doc
      setAttribute "style" "display:flex; align-items:center; gap:4px; margin:4px 0;" rootRow
      rootName <- createElement "span" doc
      setTextContent ctx.rootName (toNode rootName)
      setAttribute "style" "min-width:120px; font-weight:600;" rootName
      addRootBtn <- makeButton doc "+" (pushAddChild "")
      _ <- appendChild (toNode rootName) (toNode rootRow)
      _ <- appendChild (toNode addRootBtn) (toNode rootRow)
      pure rootRow
    Nothing -> do
      fallbackRow <- createElement "div" doc
      setAttribute "style" "display:flex; align-items:center; gap:4px; margin:4px 0;" fallbackRow
      addBtn <- makeButton doc "+" (pushAddChild "")
      _ <- appendChild (toNode addBtn) (toNode fallbackRow)
      pure fallbackRow
    Just node -> do
      let visibility = moveButtonVisibility ctx.index ctx.lastIndex
      let isSelected = case ctx.selectedExternalId of
            Just selectedId -> selectedId == node.externalId
            Nothing -> false
      let rowHighlightStyle = if isSelected
            then "background:#e7f1ff; border:1px solid #90c2ff; border-radius:6px; padding:2px 4px;"
            else ""
      nodeRow <- createElement "div" doc
      setAttribute "style" ("display:flex; align-items:center; gap:4px; margin:4px 0; margin-left:" <> show (ctx.depth * 14) <> "px;" <> rowHighlightStyle) nodeRow
      rowClick <- eventListener \_ -> pushSelect node
      addEventListener (EventType "click") rowClick false (toEventTarget nodeRow)

      nameWrap <- createElement "div" doc
      setAttribute "style" "min-width:120px;" nameWrap
      renderNodeName doc nameWrap ctx.depth ctx.index node
        { onRename: \nodeId name parentExternalId nodeOrder ->
            pushRename { nodeId, name, parentExternalId, nodeOrder }
        , onSelect: pushSelect
        }
        ctx.selectedExternalId

      addChildBtn <- makeButton doc "+" (pushAddChild node.externalId)
      delBtn <- makeButton doc "-" (pushDelete node.externalId)

      upBtn <- if visibility.showUp
        then Just <$> makeButton doc "↑" (pushMoveUp node.externalId)
        else pure Nothing

      downBtn <- if visibility.showDown
        then Just <$> makeButton doc "↓" (pushMoveDown node.externalId)
        else pure Nothing

      promoteBtn <- if ctx.canPromote
        then Just <$> makeButton doc "←" (pushPromote node.externalId)
        else pure Nothing

      rightBtn <- if visibility.showRight
        then Just <$> makeButton doc "→" (pushDemote node.externalId)
        else pure Nothing

      _ <- appendChild (toNode nameWrap) (toNode nodeRow)
      _ <- appendChild (toNode addChildBtn) (toNode nodeRow)
      _ <- appendChild (toNode delBtn) (toNode nodeRow)
      for_ upBtn (\btn -> appendChild (toNode btn) (toNode nodeRow))
      for_ downBtn (\btn -> appendChild (toNode btn) (toNode nodeRow))
      for_ promoteBtn (\btn -> appendChild (toNode btn) (toNode nodeRow))
      for_ rightBtn (\btn -> appendChild (toNode btn) (toNode nodeRow))
      pure nodeRow
  pure
    { element: row
    , onAddChild
    , onDelete
    , onRename
    , onMoveUp
    , onMoveDown
    , onPromote
    , onDemote
    , onSelect
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
