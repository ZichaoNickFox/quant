module Web.View.Layout (defaultLayout, Html) where

import           Application.Helper.View
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import           Web.Prelude
import           Web.Types

-- 主 layout
defaultLayout :: Html -> Html
defaultLayout innerHtml = H.docTypeHtml H.! A.lang "en" $
  [hsx|
    <head>
      {metaTags}
      {stylesheets}
      {scripts}
      <title>{pageTitleOrDefault "Quant Platform"}</title>
    </head>

    <body>
      {navHtml}
      <div class="container-fluid mt-3">
        {renderFlashMessages}
        {innerHtml}
      </div>
      {modal}
    </body>
  |]

-- 顶部导航栏
navHtml :: Html
navHtml = [hsx|
  <nav class="navbar navbar-expand-sm navbar-dark bg-dark px-3">
    <a class="navbar-brand" href="/">Quant</a>
    <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navmenu">
      <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse" id="navmenu">
      <ul class="navbar-nav d-flex flex-row align-items-center">
        <li class="nav-item mr-3">
          <a class="nav-link px-0 py-0" href={PageDataAction}>数据</a>
        </li>
        <li class="nav-item mr-3">
          <a class="nav-link px-0 py-0" href={PageNoteAction}>笔记</a>
        </li>
        <li class="nav-item mr-3">
          <a class="nav-link px-0 py-0" href={PageStrategyAction}>策略</a>
        </li>
        <li class="nav-item mr-3">
          <a class="nav-link px-0 py-0" href={PageBacktestAction}>回测</a>
        </li>
        <li class="nav-item mr-3">
          <a class="nav-link px-0 py-0" href={PageRuntimeAction}>盯盘</a>
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
  <!-- <script src={assetPath "/vendor/turbolinks.js"}></script> -->
  <!-- <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script> -->
  <!-- <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script> -->
  <script src={assetPath "/vendor/lightweight-charts.standalone.development.js"}></script>
  <script src={assetPath "/ps-bundle.js"}></script>

  <script src={assetPath "/helpers.js"}></script>
  <!-- <script src={assetPath "/ihp-auto-refresh.js"}></script> -->
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
