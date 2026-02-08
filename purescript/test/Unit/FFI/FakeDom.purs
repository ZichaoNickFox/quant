module Test.Unit.FFI.FakeDom
  ( clickFirstSpan
  , installFakeDom
  ) where

import Effect (Effect)
import Prelude (Unit)
import Web.DOM.Element (Element)

foreign import installFakeDom :: Effect Unit
foreign import clickFirstSpan :: Element -> Effect Unit
