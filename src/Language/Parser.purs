module Lunarpie.Language.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.State (gets, modify_)
import Data.Array (many, some)
import Data.Maybe (Maybe(..), optional)
import Data.Tuple (Tuple(..))
import Data.ZipperArray (ZipperArray, current, goNext)
import Lunarpie.Data.Ast (Ast(..), Declaration(..), curriedLambda)
import Text.Parsing.Indent (IndentParser)
import Text.Parsing.Parser (ParseState(..), fail)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Pos (Position)

type Token = 
  { type :: String
  , value :: String
  , start :: Position
  , end :: Position
  }

type TokenStream = ZipperArray Token
type Parser a = IndentParser TokenStream a

foreign import data Lexer :: Type -> Type

-------- Helpers
-- | Get the first token in the stream
token :: Parser Token
token = do
  input <- gets \(ParseState input _ _) -> input
  let head = current input
  case goNext input of
    Nothing -> pure unit 
    Just input' -> 
      modify_ \(ParseState _ position _) ->
        ParseState input' head.start true
  pure head

peek :: Parser Token
peek = gets \(ParseState input _ _) -> current input

match :: String -> Parser Token
match expected = do
  head <- token
  unless (head.type == expected) 
    $ fail 
    $ "Unexpected token " 
      <> show head.value 
      <> ". Expected a token of type " 
      <> show expected 
      <> "."
  pure head

identifier :: Parser String
identifier = match "identifier" <#> _.value
  
punctuation :: String -> Parser Unit
punctuation expected = match "punctuation" >>= \token' -> do
  unless (token'.value == expected) 
    $ fail
    $ "Unexpected punctuation \"" 
      <> token'.value 
      <> "\". Expected " 
      <> show expected 
      <> "."

parenthesis :: forall a. Parser a -> Parser a
parenthesis p = punctuation "(" *> p <* punctuation ")"

---------- Actual parsing
var :: Parser Ast
var = identifier <#> Var

star :: Parser Ast
star = punctuation "*" $> Star

lambda :: Parser Ast
lambda = do
  try $ punctuation "\\"
  arguments <- some (try identifier)
  punctuation "=>"
  body <- ast
  pure $ curriedLambda arguments body

piBinder :: Parser { name :: Maybe String, type :: Ast }
piBinder = fix \_ -> try bound <|> notBound 
  where
  bound = parenthesis ado
    name <- identifier
    punctuation ":"
    type' <- atom
    in { name: Just name, type: type' }

  notBound = do 
    atom <#> { name: Nothing, type: _ }

  atom = try var <|> try star <|> fix \_ -> parenthesis ast

pi :: Parser Ast
pi = fix \_ -> ado
  binder <- piBinder
  punctuation "->"
  return <- ast
  in Pi binder.name binder.type return

ast :: Parser Ast
ast = fix \_ -> lambda <|> try pi <|> try var <|> try star <|> parenthesis ast

declaration :: Parser Declaration
declaration = do
  maybeAnnotation <- optional annotation
  name <- case maybeAnnotation of
    Nothing -> identifier
    Just (Tuple name _) -> do
      name' <- identifier
      unless (name == name') 
        $ fail
        $ "Type definition for "
          <> show name
          <> "must be followed by it's implementation."
      pure name
  punctuation "="
  implementation <- ast
  let value = case maybeAnnotation of
        Nothing -> implementation
        Just (Tuple _ type') -> Annotation implementation type'
  pure $ Declaration { name, value }
  where
  annotation = ado
    name <- identifier 
    punctuation "::"
    type' <- ast
    in Tuple name type'

file :: Parser (Array Declaration)
file = many declaration <* match "eof"
