let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201206/packages.dhall sha256:c9ffd7577fb8ee2197309591d7ccc0f506ee37b9078866f0ef159f5abbb1b32b

let overrides = {=}

let additions =
      { debugged =
        { dependencies =
          [ "prelude"
          , "console"
          , "ordered-collections"
          , "either"
          , "tuples"
          , "lists"
          , "strings"
          , "arrays"
          , "bifunctors"
          , "record"
          , "effect"
          , "generics-rep"
          , "datetime"
          , "enums"
          ]
        , repo = "https://github.com/hdgarrood/purescript-debugged"
        , version = "744498226da3c9b5b37c69771cc0378a65cc8189"
        }
      }

in  upstream ⫽ overrides ⫽ additions
