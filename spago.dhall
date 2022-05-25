{ name = "lp"
, license = "MIT"
, repository = "https://github.com/gbagan/purescript-lp"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "either"
  , "foldable-traversable"
  , "linalg"
  , "maybe"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "rationals"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
