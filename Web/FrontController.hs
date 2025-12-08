module Web.FrontController where

import IHP.RouterPrelude
import IHP.ControllerPrelude
import IHP.LoginSupport.Middleware

import Web.View.Layout (defaultLayout)

-- Controllers
import Web.Controller.Strategy
import Web.Controller.Data
import Web.Controller.Backtest
import Web.Controller.Static
import Web.Types

instance FrontController QuantApplication where
  controllers =
    [ startPage StrategyAction
    , parseRoute @StrategyController
    , parseRoute @DataController
    , parseRoute @BacktestController
    , parseRoute @StaticController
    ]

instance InitControllerContext QuantApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh