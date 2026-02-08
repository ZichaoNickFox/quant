module Web.Service.Strategy.StrategyService
  ( createStrategyWithRootTree
  , createStrategyTreeNode
  , deleteStrategyAndTree
  , deleteStrategyTreeNode
  , loadStrategy
  , loadStrategyTree
  , updateStrategyName
  , updateStrategyTreeNode
  ) where

import Data.Foldable (for_)
import Data.List (sortBy)
import Web.Prelude
import qualified Web.Repo.TreeRepo as TreeRepo
import Web.Types
import qualified Web.Types as Types

createStrategyWithRootTree :: (?modelContext :: ModelContext) => Maybe Text -> IO Strategy
createStrategyWithRootTree mName = do
  created <- newRecord @Strategy
    |> set #name (fromMaybe "策略" mName)
    |> createRecord
  let strategyName =
        case mName of
          Just name -> name
          Nothing -> "策略-" <> tshow (get #id created)
  _ <- created
    |> set #name strategyName
    |> updateRecord
  let (Id strategyId) = get #id created
  _ <- TreeRepo.createTreeNode TreeOwnerTypeStrategy strategyId Types.File Nothing
  pure created

loadStrategy :: (?modelContext :: ModelContext) => Maybe (Id Strategy) -> IO (Maybe Strategy)
loadStrategy mStrategyId =
  case mStrategyId of
    Just strategyId -> query @Strategy |> filterWhere (#id, strategyId) |> fetchOneOrNothing
    Nothing -> query @Strategy |> fetchOneOrNothing

updateStrategyName :: (?modelContext :: ModelContext) => Id Strategy -> Text -> IO Bool
updateStrategyName strategyId strategyName = do
  mbStrategy <- query @Strategy |> filterWhere (#id, strategyId) |> fetchOneOrNothing
  case mbStrategy of
    Nothing -> pure False
    Just strategy -> do
      _ <- strategy
        |> set #name strategyName
        |> updateRecord
      pure True

deleteStrategyAndTree :: (?modelContext :: ModelContext) => Id Strategy -> IO Bool
deleteStrategyAndTree strategyId = do
  mbStrategy <- query @Strategy |> filterWhere (#id, strategyId) |> fetchOneOrNothing
  case mbStrategy of
    Nothing -> pure False
    Just strategy -> do
      let (Id strategyUuid) = strategyId
      roots <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
        |> filterWhere (#ownerId, strategyUuid)
        |> filterWhere (#parentTreeId, Nothing)
        |> fetch
      for_ roots \root ->
        TreeRepo.deleteTreeNode (get #id root)
      deleteRecord strategy
      pure True

createStrategyTreeNode
  :: (?modelContext :: ModelContext)
  => UUID
  -> Maybe UUID
  -> IO Tree
createStrategyTreeNode strategyId parentTreeId =
  case parentTreeId of
    Nothing ->
      TreeRepo.createTreeNode TreeOwnerTypeStrategy strategyId Types.File Nothing
    Just parentId -> do
      created <- newRecord @Strategy
        |> set #name ("策略" :: Text)
        |> createRecord
      let strategyName = "策略-" <> tshow (get #id created)
      _ <- created |> set #name strategyName |> updateRecord
      let (Id childStrategyId) = get #id created
      childOrder <- nextChildOrder parentId
      newRecord @Tree
        |> set #ownerType TreeOwnerTypeStrategy
        |> set #ownerId childStrategyId
        |> set #nodeType Types.File
        |> set #parentTreeId (Just parentId)
        |> set #nodeOrder childOrder
        |> createRecord

loadStrategyTree :: (?modelContext :: ModelContext) => UUID -> IO [Tree]
loadStrategyTree strategyId = do
  roots <- query @Tree
    |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
    |> filterWhere (#ownerId, strategyId)
    |> filterWhere (#parentTreeId, Nothing)
    |> orderByAsc #nodeOrder
    |> fetch
  if null roots
    then pure []
    else do
      allNodes <- query @Tree
        |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
        |> orderByAsc #nodeOrder
        |> fetch
      pure (concatMap (\root -> collectSubtree allNodes (get #id root)) roots)

updateStrategyTreeNode
  :: (?modelContext :: ModelContext)
  => Id Tree
  -> Maybe UUID
  -> Int
  -> IO ()
updateStrategyTreeNode treeId parentTreeId nodeOrder =
  TreeRepo.updateTreeNode treeId parentTreeId nodeOrder

deleteStrategyTreeNode :: (?modelContext :: ModelContext) => Id Tree -> IO ()
deleteStrategyTreeNode treeId =
  TreeRepo.deleteTreeNode treeId

nextChildOrder :: (?modelContext :: ModelContext) => UUID -> IO Int
nextChildOrder parentId = do
  children <- query @Tree
    |> filterWhere (#ownerType, TreeOwnerTypeStrategy)
    |> filterWhere (#parentTreeId, Just parentId)
    |> orderByDesc #nodeOrder
    |> fetch
  pure $ case children of
    [] -> 1
    x : _ -> get #nodeOrder x + 1

collectSubtree :: [Tree] -> Id Tree -> [Tree]
collectSubtree allNodes rootId =
  case find (\n -> get #id n == rootId) allNodes of
    Nothing -> []
    Just root ->
      let (Id rootUuid) = rootId
          children =
            allNodes
              |> filter (\n -> get #parentTreeId n == Just rootUuid)
              |> sortBy (comparing (get #nodeOrder))
      in root : concatMap (\child -> collectSubtree allNodes (get #id child)) children
