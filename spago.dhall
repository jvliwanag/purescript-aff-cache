{ name = "aff-cache"
, license = "MIT"
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
