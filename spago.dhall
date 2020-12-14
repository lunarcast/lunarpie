{ name = "my-project"
, dependencies =
  [ "ansi"
  , "console"
  , "debug"
  , "debugged"
  , "effect"
  , "lists"
  , "naturals"
  , "node-fs"
  , "ordered-collections"
  , "parsing"
  , "profunctor-lenses"
  , "psci-support"
  , "run"
  , "strings"
  , "these"
  , "unordered-collections"
  , "zipperarray"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
