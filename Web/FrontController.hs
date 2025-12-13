module Web.FrontController where

import IHP.RouterPrelude
import IHP.ControllerPrelude
import IHP.LoginSupport.Middleware

-- Controllers
import Web.Controller.StaticController
import Web.Controller.StrategyController
import Web.Controller.NoteController
import Web.Controller.DataController
import Web.Controller.BacktestController
import Web.Types
import Web.View.Layout (defaultLayout)

instance FrontController QuantApplication where
  controllers =
    [ startPage StrategyAction
    , parseRoute @StaticController
    , parseRoute @DataController
    , parseRoute @NoteController
    , parseRoute @StrategyController
    , parseRoute @BacktestController
    ]

instance InitControllerContext QuantApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh