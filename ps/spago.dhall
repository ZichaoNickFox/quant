{- This is a basic Spago configuration. You will need the PureScript toolchain
   (purs + spago) available to build. Run:
   spago bundle-app --main Main --to static/ps-bundle.js
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "quant-ps"
, dependencies =
  [ "aff"
  , "affjax"
  , "argonaut"
  , "argonaut-codecs"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "maybe"
  , "prelude"
  , "refs"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
-- add packages missing from the upstream set.
, additions =
  { websocket-simple =
      { dependencies =
          [ "aff", "console", "either", "effect", "foldable-traversable"
          , "foreign", "maybe", "refs", "web-dom", "web-events", "web-html"
          ]
      , repo = "https://github.com/purescript-contrib/purescript-websocket-simple.git"
      , version = "v9.0.0"
      }
  , events =
      { dependencies =
          [ "aff", "console", "effect", "either", "foldable-traversable"
          , "maybe", "prelude"
          ]
      , repo = "https://github.com/purescript-contrib/purescript-events.git"
      , version = "v6.0.0"
      }
  , behaviors =
      { dependencies =
          [ "aff", "console", "effect", "either", "events", "foldable-traversable"
          , "maybe", "prelude", "refs", "tuples"
          ]
      , repo = "https://github.com/paf31/purescript-behaviors.git"
      , version = "v9.0.0"
      }
  }
}
