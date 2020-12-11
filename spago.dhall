{ name = "my-project"
, dependencies =
  [ "ansi"
  , "console"
  , "debug"
  , "debugged"
  , "effect"
  , "lists"
  , "naturals"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "run"
  , "strings"
  , "unordered-collections"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
