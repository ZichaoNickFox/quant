module Web.View.RuntimeView where

import Web.Prelude

data RuntimeView = RuntimeView

instance View RuntimeView where
  html RuntimeView = [hsx|
    <div class="container mt-3">
      <h2>Runtime</h2>
      <p>Runtime page.</p>
    </div>
  |]
