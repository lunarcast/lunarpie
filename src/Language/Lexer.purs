module Lunarpie.Language.Lexer
  ( Token
  , LexingError
  , lex
  , lexerStage
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Variant (SProxy(..), Variant, inj)
import Lunarpie.Compiler.Stage (Stage)
import Lunarpie.Data.Foreign (EitherConfig, eitherConfig)
import Run.Except (throw)
import Text.Parsing.Parser.Pos (Position)

type Token = 
  { type :: String
  , value :: String
  , indentation :: Int
  , start :: Position
  , end :: Position
  }

lex :: String -> Either String (Array Token)
lex = lexImpl eitherConfig

foreign import lexImpl :: EitherConfig -> String -> Either String (Array Token)

--------- Compiler stage
type LexingError e = 
  ( tokenizationError :: String -- TODO: better errors
  | e )

lexerStage :: forall e r. Stage (LexingError e) r String (Array Token)
lexerStage text = case lex text of
  Left err -> throw $ tokenizationError err
  Right result -> pure result

--------- Codegen stuff
tokenizationError :: forall r a. a -> Variant ( tokenizationError :: a | r )
tokenizationError = inj _tokenizationError

_tokenizationError :: SProxy "tokenizationError"
_tokenizationError = SProxy
