module String where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, withGraphics)
import Data.String (Pattern(..), joinWith, split)

parenthesis :: String -> String
parenthesis s = "(" <> s <> ")"

unlines :: Array String -> String
unlines = joinWith "\n"

lines :: String -> Array String
lines = split (Pattern "\n")

indent :: Int -> String -> String
indent n = lines >>> map (spaces <> _) >>> unlines
  where
  spaces = repeat n " "

repeat :: Int -> String -> String
repeat n s | n <= 0 = s
           | otherwise = s <> repeat (n - 1) s

errorText :: String -> String
errorText = withGraphics (foreground BrightRed <> bold)
