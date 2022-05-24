{ name = "linalg"
, license = "MIT"
, repository = "https://github.com/gbagan/purescript-linalg"
, dependencies =
  [ "arrays"
  , "debug"
  , "foldable-traversable"
  , "linalg"
  , "maybe"
  , "partial"
  , "prelude"
  , "rationals"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}