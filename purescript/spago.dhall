{- This is a basic Spago configuration. You will need the PureScript toolchain
   (purs + spago) available to build. Run:
   spago bundle-app --main Main --to static/ps-bundle.js
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "quant-ps"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "argonaut"
  , "argonaut-aeson-generic"
  , "argonaut-codecs"
  , "behaviors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "event"
  , "exceptions"
  , "filterable"
  , "foldable-traversable"
  , "foreign-object"
  , "maybe"
  , "newtype"
  , "prelude"
  , "profunctor-lenses"
  , "refs"
  , "spec"
  , "spec-node"
  , "tuples"
  , "web-socket"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
}
