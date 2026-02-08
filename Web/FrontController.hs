module Web.FrontController where

import Web.Controller.ColdWarmController
import Web.Controller.BacktestController
import Web.Controller.CellController
import Web.Controller.NotifyController
import Web.Controller.PageController
import Web.Controller.StrategyController
import Web.Prelude
import Web.Types
import Web.View.Layout

instance FrontController WebApplication where
  controllers =
    [ startPage PageStrategyAction
    , parseRoute @ColdWarmController
    , parseRoute @BacktestController
    , parseRoute @PageController
    , parseRoute @CellController
    , parseRoute @StrategyController
    , parseRoute @NotifyController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh
