module Web.FrontController where

import Web.Controller.APIController
import Web.Controller.BacktestController
import Web.Controller.DataController
import Web.Controller.NotifyController
import Web.Controller.NoteController
import Web.Controller.StrategyController
import Web.Prelude
import Web.Types
import Web.View.Layout

instance FrontController WebApplication where
  controllers =
    [ startPage StrategyAction
    , parseRoute @APIController
    , parseRoute @BacktestController
    , parseRoute @DataController
    , parseRoute @NoteController
    , parseRoute @StrategyController
    , parseRoute @NotifyController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh
