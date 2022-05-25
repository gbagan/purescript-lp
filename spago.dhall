{ name = "linalg"
, license = "MIT"
, repository = "https://github.com/gbagan/purescript-linalg"
, dependencies =
  [ "arrays"
  , "either"
  , "foldable-traversable"
  , "linalg"
  , "maybe"
  , "prelude"
  , "rationals"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
