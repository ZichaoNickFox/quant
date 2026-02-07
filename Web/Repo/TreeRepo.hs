module Web.Repo.TreeRepo
  ( createTreeNode
  , updateTreeNode
  , deleteTreeNode
  , loadTree
  ) where

import qualified Data.ByteString.Char8 as BS8
import Database.PostgreSQL.Simple (Only (..), execute)
import Database.PostgreSQL.Simple.Types (Query (..))
import Web.Prelude
import Web.Types

createTreeNode
  :: (?modelContext :: ModelContext)
  => TreeOwnerType
  -> UUID
  -> NodeType
  -> Maybe UUID
  -> IO Tree
createTreeNode ownerType ownerId nodeType parentId = do
  safeParentId <- normalizeParentForCreate ownerType ownerId parentId
  order <- nextTreeOrder ownerType ownerId safeParentId
  newRecord @Tree
    |> set #ownerType ownerType
    |> set #ownerId ownerId
    |> set #nodeType nodeType
    |> set #parentTreeId safeParentId
    |> set #nodeOrder order
    |> createRecord

updateTreeNode
  :: (?modelContext :: ModelContext)
  => Id Tree
  -> Maybe UUID
  -> Int
  -> IO ()
updateTreeNode treeId parentId nodeOrder = do
  tree <- fetchOne treeId
  safeParentId <- normalizeParentForUpdate tree parentId
  tree
    |> set #parentTreeId safeParentId
    |> set #nodeOrder nodeOrder
    |> updateRecord
  pure ()

deleteTreeNode :: (?modelContext :: ModelContext) => Id Tree -> IO ()
deleteTreeNode treeId = do
  tree <- fetchOne treeId
  deleteCellsInTree (get #ownerType tree) treeId
  deleteTreeSubtree treeId

loadTree :: (?modelContext :: ModelContext) => TreeOwnerType -> UUID -> IO [Tree]
loadTree ownerType ownerId =
  query @Tree
    |> filterWhere (#ownerType, ownerType)
    |> filterWhere (#ownerId, ownerId)
    |> orderByAsc #nodeOrder
    |> fetch

nextTreeOrder :: (?modelContext :: ModelContext) => TreeOwnerType -> UUID -> Maybe UUID -> IO Int
nextTreeOrder ownerType ownerId parentId = do
  mb <- query @Tree
    |> filterWhere (#ownerType, ownerType)
    |> filterWhere (#ownerId, ownerId)
    |> filterWhere (#parentTreeId, parentId)
    |> orderByDesc #nodeOrder
    |> fetchOneOrNothing
  pure $ maybe 1 ((+ 1) . get #nodeOrder) mb

deleteCellsInTree :: (?modelContext :: ModelContext) => TreeOwnerType -> Id Tree -> IO ()
deleteCellsInTree ownerType treeId = do
  let cellOwnerType = treeOwnerToCellOwner ownerType
  let sql = Query (BS8.pack
        "WITH RECURSIVE subtree AS ( \
        \  SELECT id FROM tree WHERE id = ? \
        \  UNION ALL \
        \  SELECT t.id FROM tree t JOIN subtree s ON t.parent_tree_id = s.id \
        \) \
        \DELETE FROM cell \
        \WHERE owner_type = ? AND owner_id IN (SELECT id FROM subtree)")
  withDatabaseConnection $ \conn -> do
    _ <- execute conn sql (treeId, cellOwnerType)
    pure ()

deleteTreeSubtree :: (?modelContext :: ModelContext) => Id Tree -> IO ()
deleteTreeSubtree treeId = do
  let sql = Query (BS8.pack
        "WITH RECURSIVE subtree AS ( \
        \  SELECT id FROM tree WHERE id = ? \
        \  UNION ALL \
        \  SELECT t.id FROM tree t JOIN subtree s ON t.parent_tree_id = s.id \
        \) \
        \DELETE FROM tree \
        \WHERE id IN (SELECT id FROM subtree)")
  withDatabaseConnection $ \conn -> do
    _ <- execute conn sql (Only treeId)
    pure ()

normalizeParentForCreate
  :: (?modelContext :: ModelContext)
  => TreeOwnerType
  -> UUID
  -> Maybe UUID
  -> IO (Maybe UUID)
normalizeParentForCreate ownerType ownerId parentId =
  case parentId of
    Nothing -> pure Nothing
    Just pid -> do
      mbParent <- fetchOneOrNothing (Id pid :: Id Tree)
      pure $ case mbParent of
        Just parent
          | get #ownerType parent == ownerType
          , get #ownerId parent == ownerId ->
              Just pid
        _ -> Nothing

normalizeParentForUpdate
  :: (?modelContext :: ModelContext)
  => Tree
  -> Maybe UUID
  -> IO (Maybe UUID)
normalizeParentForUpdate tree parentId = do
  let (Id treeId) = get #id tree
      ownerType = get #ownerType tree
      ownerId = get #ownerId tree
  nodes <- loadTree ownerType ownerId
  pure $ case parentId of
    Nothing -> Nothing
    Just pid
      | pid == treeId -> get #parentTreeId tree
      | not (nodeExists pid nodes) -> get #parentTreeId tree
      | pid `elem` subtreeIds treeId nodes -> get #parentTreeId tree
      | otherwise -> Just pid

nodeExists :: UUID -> [Tree] -> Bool
nodeExists treeId = any (\n -> treeUuid n == treeId)

subtreeIds :: UUID -> [Tree] -> [UUID]
subtreeIds rootId nodes = go [rootId] []
  where
    go [] acc = acc
    go (x:xs) acc
      | x `elem` acc = go xs acc
      | otherwise =
          let children = map treeUuid (filter (\n -> get #parentTreeId n == Just x) nodes)
          in go (xs <> children) (x : acc)

treeUuid :: Tree -> UUID
treeUuid tree = let (Id uuid) = get #id tree in uuid

treeOwnerToCellOwner :: TreeOwnerType -> CellOwnerType
treeOwnerToCellOwner TreeOwnerTypeNote = CellOwnerTypeNote
treeOwnerToCellOwner TreeOwnerTypeStrategy = CellOwnerTypeStrategy
