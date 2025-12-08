module Web.View.Static.Index where
import IHP.ViewPrelude

data IndexView = IndexView

instance View IndexView where
  html IndexView = [hsx|
  |]