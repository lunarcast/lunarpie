let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.5-20220110/packages.dhall
        sha256:8dbf71bfc6c7a11043619eebe90ff85f7d884541048aa8cc48eef1ee781cbc0e

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
          , "datetime"
          , "enums"
          , "matryoshka"
          ]
        , repo = "https://github.com/Mateiadrielrafael/purescript-debugged"
        , version = "633220f91f87c9acbc4eebbf87628e6cdc658b7b"
        }
      , zipperarray =
        { dependencies =
          [ "arrays", "maybe", "prelude", "naturals", "strictlypositiveint" ]
        , repo = "https://github.com/jamieyung/purescript-zipperarray/"
        , version = "master"
        }
      , strictlypositiveint =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/jamieyung/purescript-strictlypositiveint"
        , version = "master"
        }
      }

in  upstream // overrides // additions
