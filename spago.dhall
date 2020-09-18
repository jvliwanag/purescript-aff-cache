{ name = "aff-cache"
, license = "MIT"
, repository = "https://github.com/jvliwanag/purescript-aff-cache.git"
, dependencies =
  [ "aff"
  , "avar"
  , "console"
  , "effect"
  , "psci-support"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
