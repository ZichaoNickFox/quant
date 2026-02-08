module Web.Controller.StrategyController where

import qualified Data.Aeson as A
import Proto.Strategy (StrategyInfo(..))
import Web.Prelude
import qualified Web.Service.Strategy.StrategyService as StrategyService
import Web.Types

instance Controller StrategyController where
  action StrategyCreateAction = do
    let mName = paramOrNothing @Text "name"
    strategy <- StrategyService.createStrategyWithRootTree mName
    renderJson (toStrategyInfo strategy)

  action StrategyReadAction = do
    let mStrategyId = Id <$> (paramOrNothing @UUID "strategyId")
    mbStrategy <- StrategyService.loadStrategy mStrategyId
    renderJson (fmap toStrategyInfo mbStrategy)

  action StrategyUpdateAction = do
    let strategyId = Id (param @UUID "strategyId") :: Id Strategy
        strategyName = param @Text "name"
    ok <- StrategyService.updateStrategyName strategyId strategyName
    renderJson (A.object [ "ok" A..= ok ])

  action StrategyDeleteAction = do
    let strategyId = Id (param @UUID "strategyId") :: Id Strategy
    ok <- StrategyService.deleteStrategyAndTree strategyId
    renderJson (A.object [ "ok" A..= ok ])

  action StrategyTreeCreateAction = do
    let strategyId = param @UUID "strategyId"
        parentTreeId = paramOrNothing @UUID "parentTreeId"
    node <- StrategyService.createStrategyTreeNode strategyId parentTreeId
    renderJson (treeToJson node)

  action StrategyTreeReadAction = do
    let strategyId = param @UUID "strategyId"
    nodes <- StrategyService.loadStrategyTree strategyId
    renderJson (map treeToJson nodes)

  action StrategyTreeUpdateAction = do
    let treeId = Id (param @UUID "treeId") :: Id Tree
        parentTreeId = paramOrNothing @UUID "parentTreeId"
        nodeOrder = param @Int "nodeOrder"
    StrategyService.updateStrategyTreeNode treeId parentTreeId nodeOrder
    renderJson (A.object [ "ok" A..= True ])

  action StrategyTreeDeleteAction = do
    let treeId = Id (param @UUID "treeId") :: Id Tree
    StrategyService.deleteStrategyTreeNode treeId
    renderJson (A.object [ "ok" A..= True ])

toStrategyInfo :: Strategy -> StrategyInfo
toStrategyInfo strategy =
  StrategyInfo
    { id = tshow (get #id strategy)
    , name = get #name strategy
    }

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
