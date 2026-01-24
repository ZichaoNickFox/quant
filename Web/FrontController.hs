module Web.FrontController where

import Web.Prelude
import Web.Types
import Web.View.Layout

instance FrontController WebApplication where
  controllers =
    [ startPage StrategyAction
    , parseRoute @DataController
    , parseRoute @NoteController
    , parseRoute @StrategyController
    , parseRoute @BacktestController
    , parseRoute @(JobsDashboardController NoAuth '[])
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh