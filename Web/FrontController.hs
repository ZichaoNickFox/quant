module Web.FrontController where

import Web.Controller.APIController
import Web.Controller.PageController
import Web.Controller.CellController
import Web.Controller.TreeController
import Web.Controller.NotifyController
import Web.Prelude
import Web.Types
import Web.View.Layout

instance FrontController WebApplication where
  controllers =
    [ startPage PageStrategyAction
    , parseRoute @APIController
    , parseRoute @PageController
    , parseRoute @CellController
    , parseRoute @TreeController
    , parseRoute @NotifyController
    ]

instance InitControllerContext WebApplication where
  initContext = do
    setLayout defaultLayout
    initAutoRefresh
