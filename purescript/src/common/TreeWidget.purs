module Common.TreeWidget
  ( buildCreateUrl
  , buildUpdateUrl
  , createFRP
  , Events
  ) where

import Affjax.ResponseFormat as RF
import Affjax.Web as AX
import Common.TreeNodeWidget as TreeNodeWidget
import Data.Argonaut.Core as J
import Data.Argonaut.Decode as D
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import Foreign.Object as FO
import FRP as FRP
import FRP.Component.Tree (MoveDir(..), TreeUpdatePatch, movePatches)
import FRP.Component.Tree as Tree
import Prelude
import Web.DOM.Element (Element, getAttribute, setAttribute, toNode)
import Web.DOM.Node (setTextContent)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

type Events =
  { refreshPush :: Effect Unit
  , onNodeSelected :: FRP.Event (Tree.TreeNode TreeNodeWidget.TreeNodePayload)
  }

createFRP
  :: String
  -> String
  -> Effect Events
createFRP ownerType selector = do
  win <- window
  htmlDoc <- document win
  mRoot <- querySelector (QuerySelector selector) (HTMLDoc.toParentNode htmlDoc)
  case mRoot of
    Nothing -> throw ("Missing tree root element for selector: " <> selector)
    Just rootEl -> setupTreeRoot ownerType rootEl

buildCreateUrl :: String -> String -> String -> String
buildCreateUrl ownerType ownerId nodeId =
  let base = "/StrategyTreeCreate?strategyId=" <> ownerId
  in if nodeId == ""
      then base
      else base <> "&parentTreeId=" <> nodeId

buildUpdateUrl :: TreeUpdatePatch -> String
buildUpdateUrl patch =
  let base = "/StrategyTreeUpdate?treeId=" <> patch.externalId <> "&nodeOrder=" <> show patch.nodeOrder
  in case patch.parentExternalId of
      Just pid -> base <> "&parentTreeId=" <> pid
      Nothing -> base

callVoidAff :: String -> Aff Unit
callVoidAff url = do
  _ <- AX.get RF.ignore url
  pure unit

requestCreatedStrategyId :: Aff (Maybe { id :: String, name :: String })
requestCreatedStrategyId = do
  response <- AX.get RF.json "/StrategyCreate"
  pure case response of
    Left _ -> Nothing
    Right ok ->
      case D.decodeJson ok.body :: Either D.JsonDecodeError { id :: String, name :: String } of
        Right strategy -> Just strategy
        Left _ -> Nothing

decodeNode :: J.Json -> Maybe (Tree.TreeNode TreeNodeWidget.TreeNodePayload)
decodeNode json = do
  obj <- J.toObject json
  nodeId <- readField ["id"] obj
  ownerId <- readField ["owner_id", "ownerId"] obj
  nodeType <- readField ["node_type", "nodeType"] obj
  nodeOrder <- readField ["node_order", "nodeOrder"] obj
  let parentExternalId = readOptionalField ["parent_tree_id", "parentExternalId"] obj
  pure
    { externalId: nodeId
    , nodeType
    , parentExternalId
    , nodeOrder
    , payload: { ownerId, name: "" }
    }

firstPresent :: Array String -> FO.Object J.Json -> Maybe J.Json
firstPresent keys obj =
  case A.uncons keys of
    Nothing -> Nothing
    Just { head, tail } ->
      case FO.lookup head obj of
        Just v -> Just v
        Nothing -> firstPresent tail obj

performMove
  :: MoveDir
  -> String
  -> Array (Tree.TreeNode TreeNodeWidget.TreeNodePayload)
  -> Aff Boolean
performMove dir nodeId nodes =
  case A.find (\n -> n.externalId == nodeId) nodes of
    Nothing -> pure false
    Just node -> do
      let urls = map buildUpdateUrl (movePatches dir nodes node)
      traverse_ callVoidAff urls
      pure (not (A.null urls))

readField :: forall a. D.DecodeJson a => Array String -> FO.Object J.Json -> Maybe a
readField keys obj = do
  value <- firstPresent keys obj
  case D.decodeJson value of
    Left _ -> Nothing
    Right v -> Just v

readOptionalField :: forall a. D.DecodeJson a => Array String -> FO.Object J.Json -> Maybe a
readOptionalField keys obj =
  case firstPresent keys obj of
    Nothing -> Nothing
    Just value ->
      case D.decodeJson value of
        Left _ -> Nothing
        Right v -> v

requestTreeNodes :: String -> String -> Int -> Aff (Array (Tree.TreeNode TreeNodeWidget.TreeNodePayload))
requestTreeNodes ownerType ownerId refreshSeq =
  let url = "/StrategyTreeRead?strategyId=" <> ownerId <> "&r=" <> show refreshSeq
  in do
  response <- AX.get RF.json url
  pure $ case response of
    Left _ -> []
    Right ok ->
      case D.decodeJson ok.body :: Either D.JsonDecodeError (Array J.Json) of
        Left _ -> []
        Right arr -> A.mapMaybe decodeNode arr

setupTreeRoot
  :: String
  -> Element
  -> Effect Events
setupTreeRoot ownerType rootEl = do
  mOwnerId <- getAttribute "data-owner-id" rootEl
  ownerIdRef <- Ref.new (fromMaybe "" mOwnerId)
  let rootName = if ownerType == "strategy" then "策略" else ownerType
  handle <- Tree.createFRP rootEl []
    { createNodeFRP: TreeNodeWidget.createFRP
    , rootName
    }

  -- Connect node events to tree events
  _ <- FRP.subscribe handle.onNodeCreatedSubs \{ node, nodeEvents, container } -> do
    _ <- FRP.subscribe nodeEvents.onAddChild handle.addChildPush
    _ <- FRP.subscribe nodeEvents.onDelete handle.deletePush
    _ <- FRP.subscribe nodeEvents.onRename \{ nodeId, name, parentExternalId, nodeOrder } ->
      -- TODO: implement rename handler
      pure unit
    _ <- FRP.subscribe nodeEvents.onMoveUp handle.moveUpPush
    _ <- FRP.subscribe nodeEvents.onMoveDown handle.moveDownPush
    _ <- FRP.subscribe nodeEvents.onPromote handle.promotePush
    _ <- FRP.subscribe nodeEvents.onDemote handle.demotePush
    _ <- FRP.subscribe nodeEvents.onSelect handle.selectNodePush
    pure unit

  { event: refreshEvent, push: refreshPush } <- FRP.create
  let refresh = refreshPush unit
  refreshSeqRef <- Ref.new 0

  requestRequester <- FRP.createRequester
    ("[Tree " <> ownerType <> "] request req")
    ("[Tree " <> ownerType <> "] request resp")
    \_ -> do
      seq <- liftEffect do
        n <- Ref.read refreshSeqRef
        let n' = n + 1
        Ref.write n' refreshSeqRef
        pure n'
      ownerId <- liftEffect (Ref.read ownerIdRef)
      requestTreeNodes ownerType ownerId seq

  _ <- FRP.subscribe refreshEvent \_ -> requestRequester.requestPush unit
  _ <- FRP.subscribe requestRequester.responseEvent handle.setNodesPush
  _ <- FRP.subscribe handle.onReloadSubs \_ ->
    refresh
  _ <- FRP.subscribe handle.onAddChildSubs \nodeId ->
    launchAff_ do
      ownerId <- liftEffect (Ref.read ownerIdRef)
      if ownerType == "strategy" && ownerId == "" && nodeId == "" then do
        created <- requestCreatedStrategyId
        case created of
          Nothing -> pure unit
          Just strategy ->
            liftEffect do
              let newOwnerId = strategy.id
              Ref.write newOwnerId ownerIdRef
              setAttribute "data-owner-id" newOwnerId rootEl
              win <- window
              doc <- document win
              mCellOwnerId <- querySelector (QuerySelector "[data-cell-create-owner-id='1']") (HTMLDoc.toParentNode doc)
              traverse_ (setAttribute "value" newOwnerId) mCellOwnerId
              refresh
      else do
        callVoidAff (buildCreateUrl ownerType ownerId nodeId)
        liftEffect refresh
  _ <- FRP.subscribe handle.onDeleteSubs \nodeId ->
    launchAff_ do
      callVoidAff ("/StrategyTreeDelete?treeId=" <> nodeId)
      liftEffect refresh
  _ <- FRP.subscribe handle.onMoveUpSubs \(Tuple nodeId nodes) ->
    launchAff_ do
      shouldRefresh <- performMove MoveUp nodeId nodes
      when shouldRefresh (liftEffect refresh)
  _ <- FRP.subscribe handle.onMoveDownSubs \(Tuple nodeId nodes) ->
    launchAff_ do
      shouldRefresh <- performMove MoveDown nodeId nodes
      when shouldRefresh (liftEffect refresh)
  _ <- FRP.subscribe handle.onPromoteSubs \(Tuple nodeId nodes) ->
    launchAff_ do
      shouldRefresh <- performMove Promote nodeId nodes
      when shouldRefresh (liftEffect refresh)
  _ <- FRP.subscribe handle.onDemoteSubs \(Tuple nodeId nodes) ->
    launchAff_ do
      shouldRefresh <- performMove Demote nodeId nodes
      when shouldRefresh (liftEffect refresh)
  refresh
  pure
    { refreshPush: refresh
    , onNodeSelected: handle.onNodeSelected
    }
