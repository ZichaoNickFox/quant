module Web.View.DebugHtml where

import Data.Maybe
import Data.Text hiding (length)
import Generated.Types
import IHP.ViewPrelude hiding (Symbol)
import IHP.ViewSupport

debugHtml :: Maybe Text -> Html
debugHtml mbMessage =
  [hsx|
    {maybe mempty alert mbMessage }
  |]

alert :: Text -> Html
alert message =
  [hsx|
    <div class="alert alert-warning">{message}</div>
  |]