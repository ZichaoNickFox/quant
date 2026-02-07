module Web.Controller.TreeController where

import qualified Data.Aeson as A
import Web.Prelude
import Web.Repo.TreeRepo as TreeRepo
import Web.Types

instance Controller TreeController where
  action TreeCreateAction = do
    let ownerType = param @TreeOwnerType "ownerType"
        ownerId = param @UUID "ownerId"
        nodeType = param @NodeType "nodeType"
        parentId = paramOrNothing @UUID "parentTreeId"
    _ <- TreeRepo.createTreeNode ownerType ownerId nodeType parentId
    redirectBack

  action TreeUpdateAction = do
    let treeId = Id (param @UUID "treeId") :: Id Tree
        parentId = paramOrNothing @UUID "parentTreeId"
        nodeOrder = param @Int "nodeOrder"
    TreeRepo.updateTreeNode treeId parentId nodeOrder
    redirectBack

  action TreeDeleteAction = do
    let treeId = Id (param @UUID "treeId") :: Id Tree
    TreeRepo.deleteTreeNode treeId
    redirectBack

  action TreeReadAction = do
    let ownerType = param @TreeOwnerType "ownerType"
        ownerId = param @UUID "ownerId"
    nodes <- TreeRepo.loadTree ownerType ownerId
    renderJson (map treeToJson nodes)

treeToJson :: Tree -> A.Value
treeToJson node =
  A.object
    [ "id" A..= get #id node
    , "owner_type" A..= inputValue (get #ownerType node)
    , "owner_id" A..= get #ownerId node
    , "node_type" A..= inputValue (get #nodeType node)
    , "parent_tree_id" A..= get #parentTreeId node
    , "node_order" A..= get #nodeOrder node
    ]
