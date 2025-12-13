module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import IHP.RouterSupport
import Web.Controller.BacktestController
import Web.Routes
import Web.Types
import Application.Helper.View

-- 主 layout
defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $
  [hsx|
    <head>
      {metaTags}

      {stylesheets}
      {scripts}

      <title>{pageTitleOrDefault "Quant Platform"}</title>
    </head>

    <body>
      {renderNavbar}
      <div class="container-fluid mt-3">
        {renderFlashMessages}
        {inner}
      </div>
      {modal}
    </body>
  |]

-- 顶部导航栏
renderNavbar :: Html
renderNavbar = [hsx|
  <nav class="navbar navbar-expand-lg navbar-dark bg-dark px-3">
    <a class="navbar-brand" href="/">Quant</a>
    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navmenu">
      <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse" id="navmenu">
      <ul class="navbar-nav me-auto">
        <li class="nav-item">
            <a class="nav data" href={DataAction}>数据</a>
        </li>
        <li class="nav-item">
            <a class="nav note" href={NoteAction}>笔记</a>
        </li>
        <li class="nav-item">
            <a class="nav strategy" href={StrategyAction}>策略</a>
        </li>
        <li class="nav-item">
            <a class="nav backtest" href={BacktestAction}>回测</a>
        </li>
      </ul>
    </div>
  </nav>
  |]

stylesheets :: Html
stylesheets = [hsx|
  <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
  <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
  <link rel="stylesheet" href={assetPath "/app.css"}/>
|]

scripts :: Html
scripts = [hsx|
  {when isDevelopment devScripts}
  <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
  <script src={assetPath "/vendor/timeago.js"}></script>
  <script src={assetPath "/vendor/popper.min.js"}></script>
  <script src={assetPath "/vendor/bootstrap.min.js"}></script>
  <script src={assetPath "/vendor/flatpickr.js"}></script>
  <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
  <script src={assetPath "/vendor/turbolinks.js"}></script>
  <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
  <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
  <script src={assetPath "/helpers.js"}></script>
  <script src={assetPath "/ihp-auto-refresh.js"}></script>
|]

devScripts :: Html
devScripts = [hsx|
  <script id="livereload-script" src={assetPath "/livereload.js"}></script>
|]

metaTags :: Html
metaTags = [hsx|
  <meta charset="utf-8"/>
  <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
  <meta property="og:title" content="Quant Platform"/>
  <meta property="og:type" content="website"/>
  <meta property="og:url" content="TODO"/>
  <meta property="og:description" content="Quant Platform"/>
  {autoRefreshMeta}
|]