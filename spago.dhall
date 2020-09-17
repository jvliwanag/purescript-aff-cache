{ name = "aff-cache"
, dependencies =
  [ "aff"
  , "avar"
  , "console"
  , "effect"
  , "psci-support"
  , "spec"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
