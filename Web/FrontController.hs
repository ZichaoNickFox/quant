module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controllers
import Web.Controller.Strategies
import Web.Controller.Data
import Web.Controller.Backtest
import Web.Controller.Static
import IHP.LoginSupport.Middleware

-- 如果你有用户系统可以保留，不需要可以删掉
import Web.Controller.Sessions
import Web.Controller.Users

instance FrontController WebApplication where
  controllers =
    [ startPage StrategyAction
    , parseRoute @StrategyController
    , parseRoute @DataController
    , parseRoute @BacktestController

    -- 登录 / 用户（可选）
    , parseRoute @SessionsController 
    , parseRoute @UsersController

    , parseRoute @StaticController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh

    -- 可选（如果你启用了 login）
    initAuthentication @User