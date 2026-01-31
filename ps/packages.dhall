{-
  This is a minimal packages.dhall; you may update to a newer package set if needed.
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.15-20260129/packages.dhall
        sha256:778b107772398c27032dcf28e171a2f3c04f5f6d36736d27a5059d15724db1ad

in  upstream
