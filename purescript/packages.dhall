{-
  This is a minimal packages.dhall; you may update to a newer package set if needed.
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260129/packages.dhall
        sha256:778b107772398c27032dcf28e171a2f3c04f5f6d36736d27a5059d15724db1ad

let additions =
      { event =
        { dependencies =
          [ "console"
          , "effect"
          , "filterable"
          , "record"
          , "monoid-extras"
          , "nullable"
          , "unsafe-reference"
          , "js-timers"
          , "now"
          ]
        , repo = "https://github.com/mikesol/purescript-event.git"
        , version = "v1.6.6"
        }
      , behaviors =
          { dependencies =
              [ "aff", "console", "effect", "either", "event", "filterable"
              , "foldable-traversable", "maybe", "prelude", "refs", "tuples"
              , "web-uievents", "web-html", "web-events"
              ]
          , repo = "https://github.com/mikesol/purescript-behaviors"
          , version = "v8.2.2"
          }
      , argonaut-aeson-generic =
          { dependencies =
              [ "argonaut", "argonaut-codecs", "argonaut-generic", "console"
              , "foreign-object", "prelude", "typelevel-prelude"
              ]
          , repo = "https://github.com/coot/purescript-argonaut-aeson-generic.git"
          , version = "v0.4.1"
          }
      }

in  upstream // additions
