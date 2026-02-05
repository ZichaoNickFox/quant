module Common.TreeWidget
  ( createTreeWidgetByOwnerType
  , TreeWidgetHandle
  , TreeWidgetAction(..)
  , buildUpdateUrl
  , buildRenameUrl
  ) where

import Prelude

import Affjax.ResponseFormat as RF
import Affjax.Web as AX
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Foldable (traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Ref as Ref
import FFI.URI (encodeURIComponent)
import FRP as FRP
import FRP.Tree as Tree
import FRP.Tree (TreeAction(..), TreeNode, TreeUpdatePatch, movePatches)
import Web.DOM.Element (Element, getAttribute)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDoc
import Web.HTML.Window (document)

import Data.Argonaut.Core as J
import Data.Argonaut.Decode as D
import Foreign.Object as FO

type TreeWidgetHandle =
  { onTreeUpdate :: FRP.Event Tree.TreeNode
  , actionPush :: TreeWidgetAction -> Effect Unit
  }

data TreeWidgetAction
  = RefreshTree

createTreeWidgetByOwnerType
  :: String
  -> String
  -> Effect TreeWidgetHandle
createTreeWidgetByOwnerType ownerType selector = do
  win <- window
  htmlDoc <- document win
  mRoot <- querySelector (QuerySelector selector) (HTMLDoc.toParentNode htmlDoc)
  case mRoot of
    Nothing -> throw ("Missing tree root element for selector: " <> selector)
    Just rootEl -> setupTreeRoot ownerType rootEl

setupTreeRoot
  :: String
  -> Element
  -> Effect TreeWidgetHandle
setupTreeRoot ownerType rootEl = do
  mOwnerId <- getAttribute "data-owner-id" rootEl
  let ownerId = fromMaybe "" mOwnerId
  handle <- Tree.createTree rootEl []
  { event: actionEvent, push: actionPush } <- FRP.create
  { event: refreshEvent, push: refreshPush } <- FRP.create
  refreshSeqRef <- Ref.new 0

  fetchRequester <- FRP.createRequester
    ("[Tree " <> ownerType <> "] fetch req")
    ("[Tree " <> ownerType <> "] fetch resp")
    \_ -> do
      seq <- liftEffect do
        n <- Ref.read refreshSeqRef
        let n' = n + 1
        Ref.write n' refreshSeqRef
        pure n'
      fetchTreeNodes ownerType ownerId seq

  directRequester <- FRP.createRequester
    ("[Tree " <> ownerType <> "] direct req")
    ("[Tree " <> ownerType <> "] direct resp")
    (runNonMoveAction ownerType ownerId)

  moveRequester <- FRP.createRequester
    ("[Tree " <> ownerType <> "] move req")
    ("[Tree " <> ownerType <> "] move resp")
    \(Tuple action nodes) -> runMoveAction action nodes

  _ <- FRP.subscribe refreshEvent \_ -> fetchRequester.requestPush unit
  _ <- FRP.subscribe fetchRequester.responseEvent handle.setNodes
  _ <- FRP.subscribe handle.directActionEvent directRequester.requestPush
  _ <- FRP.subscribe directRequester.responseEvent \shouldRefresh ->
    when shouldRefresh (refreshPush unit)
  _ <- FRP.subscribe handle.moveActionEvent moveRequester.requestPush
  _ <- FRP.subscribe moveRequester.responseEvent \shouldRefresh ->
    when shouldRefresh (refreshPush unit)
  _ <- FRP.subscribe actionEvent \action ->
    case action of
      RefreshTree -> refreshPush unit

  refreshPush unit
  pure
    { onTreeUpdate: handle.selectedNodeEvent
    , actionPush
    }

fetchTreeNodes :: String -> String -> Int -> Aff (Array TreeNode)
fetchTreeNodes ownerType ownerId refreshSeq =
  let url = "/TreeRead?ownerType=" <> ownerType <> "&ownerId=" <> ownerId <> "&r=" <> show refreshSeq
  in do
  response <- AX.get RF.json url
  pure $ case response of
    Left _ -> []
    Right ok ->
      case D.decodeJson ok.body :: Either D.JsonDecodeError (Array J.Json) of
        Left _ -> []
        Right arr -> A.mapMaybe decodeNode arr

runNonMoveAction :: String -> String -> TreeAction -> Aff Boolean
runNonMoveAction ownerType ownerId action =
  case action of
    ActionReload ->
      pure true
    ActionAddRoot -> do
      callVoidAff $ "/TreeCreate?ownerType=" <> ownerType <> "&ownerId=" <> ownerId <> "&nodeType=file&name=NewNode"
      pure true
    ActionAddChild nodeId -> do
      callVoidAff $ "/TreeCreate?ownerType=" <> ownerType <> "&ownerId=" <> ownerId <> "&nodeType=file&name=NewNode&parentTreeId=" <> nodeId
      pure true
    ActionDelete nodeId -> do
      callVoidAff $ "/TreeDelete?treeId=" <> nodeId
      pure true
    ActionRename nodeId newName parentTreeId nodeOrder -> do
      callVoidAff $ buildRenameUrl nodeId newName parentTreeId nodeOrder
      pure true
    ActionMove _ _ ->
      pure false

runMoveAction :: TreeAction -> Array TreeNode -> Aff Boolean
runMoveAction action nodes =
  case action of
    ActionMove dir nodeId ->
      case A.find (\n -> n.id == nodeId) nodes of
        Nothing -> pure false
        Just node -> do
          let urls = map buildUpdateUrl (movePatches dir nodes node)
          traverse_ callVoidAff urls
          pure (not (A.null urls))
    _ -> pure false

decodeNode :: J.Json -> Maybe TreeNode
decodeNode json = do
  obj <- J.toObject json
  nodeId <- readField ["id"] obj
  name <- readField ["name"] obj
  nodeType <- readField ["node_type", "nodeType"] obj
  nodeOrder <- readField ["node_order", "nodeOrder"] obj
  let parentTreeId = readOptionalField ["parent_tree_id", "parentTreeId"] obj
  pure { id: nodeId, name, nodeType, parentTreeId, nodeOrder }

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

firstPresent :: Array String -> FO.Object J.Json -> Maybe J.Json
firstPresent keys obj =
  case A.uncons keys of
    Nothing -> Nothing
    Just { head, tail } ->
      case FO.lookup head obj of
        Just v -> Just v
        Nothing -> firstPresent tail obj

callVoidAff :: String -> Aff Unit
callVoidAff url = do
  _ <- AX.get RF.ignore url
  pure unit

buildUpdateUrl :: TreeUpdatePatch -> String
buildUpdateUrl patch =
  let base = "/TreeUpdate?treeId=" <> patch.treeId <> "&nodeOrder=" <> show patch.nodeOrder
  in case patch.parentTreeId of
      Just pid -> base <> "&parentTreeId=" <> pid
      Nothing -> base

buildRenameUrl :: String -> String -> Maybe String -> Int -> String
buildRenameUrl treeId name parentTreeId nodeOrder =
  buildUpdateUrl { treeId, parentTreeId, nodeOrder } <> "&name=" <> encodeURIComponent name
