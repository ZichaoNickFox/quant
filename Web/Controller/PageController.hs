module Web.Controller.PageController where

import           Web.Prelude
import           Web.Types

data AppShellView = AppShellView
  { page :: Text
  }

instance View AppShellView where
  html AppShellView { page } = [hsx|
    <div id="app-root" data-app-page={page}></div>
  |]

instance Controller PageController where
  action PageDataAction = do
    render AppShellView { page = "data" }

  action PageStrategyAction = do
    render AppShellView { page = "strategy" }

  action PageStrategyCreateAction = do
    render AppShellView { page = "strategy" }

  action PageBacktestAction = do
    render AppShellView { page = "backtest" }

  action PageRuntimeAction = do
    render AppShellView { page = "runtime" }
