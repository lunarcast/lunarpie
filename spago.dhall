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
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "run"
  , "strings"
  , "unordered-collections"
  , "zipperarray"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
