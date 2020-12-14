module Lunarpie.Language.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Control.Monad.Reader (lift)
import Control.Monad.State (State, evalState, get, gets, modify_, put)
import Data.Array (many, some)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant, inj)
import Data.ZipperArray (ZipperArray, current, goNext)
import Data.ZipperArray as ZipperArray
import Lunarpie.Compiler.Stage (Stage)
import Lunarpie.Data.Ast (Ast(..), TopLevelEntry(..), curriedLambda, manyCalls)
import Lunarpie.Language.Lexer (Token)
import Run.Except (throw)
import Text.Parsing.Parser (ParseError, ParseState(..), ParserT, fail, runParserT)
import Text.Parsing.Parser.Combinators (try)

type TokenStream = ZipperArray Token

type Parser a = ParserT TokenStream (State Int) a

runIndent :: forall a. State Int a -> a
runIndent = flip evalState 0

-------- Helpers
withIndentation :: forall a. Int -> Parser a -> Parser a
withIndentation amount parser = do
  old <- lift get
  lift $ put amount
  result <- parser
  lift $ put old
  pure result

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
  indentation <- lift get
  unless (head.indentation >= indentation) $ fail $ "Token \"" <> head.value <> "\" is not indented enough (minimum " <> show indentation <> " spaces required)"
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

keyword :: String -> Parser Unit
keyword expected = token >>= \token' -> do
  unless (token'.value == expected && token'.type == "keyword")
    $ fail
    $ "Unexpected token \""
    <> token'.value
    <> "\". Expected keyword "
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

piBinder :: Parser Ast -> Parser { name :: Maybe String, type :: Ast }
piBinder ast' = try bound <|> notBound
  where
  bound = parenthesis ado
    name <- identifier
    punctuation ":"
    type' <- ast'
    in { name: Just name, type: type' }

  notBound = (try calls <|> atom ast') <#> { name: Nothing, type: _ }

pi :: Parser Ast
pi = fix \_ -> ado
  binder <- piBinder ast
  punctuation "->"
  return <- ast
  in Pi binder.name binder.type return

calls :: Parser Ast
calls = fix \_ -> ado
  function <- atom ast
  arguments <- some (try $ atom ast) -- TODO: support f \_ => ... syntax
  in manyCalls function arguments

ast :: Parser Ast
ast = fix \_ -> try calls <|> lambda <|> try pi <|> try var <|> try star <|> parenthesis ast

atom :: Parser Ast -> Parser Ast
atom ast' = try var <|> try star <|> fix \_ -> parenthesis ast'

assumption :: Parser TopLevelEntry
assumption = do
  try $ keyword "assume"
  name <- identifier
  punctuation "::"
  type' <- withIndentation 2 ast
  pure $ Axiom name type'

declaration :: Parser TopLevelEntry
declaration = do
  Tuple name maybeAnnotation <- annotation
  case maybeAnnotation of
    Nothing -> pure unit
    Just _ -> do
      name' <- identifier
      unless (name == name')
        $ fail
        $ "Type definition for "
        <> show name
        <> "must be followed by it's implementation."
  punctuation "="
  implementation <- withIndentation 2 ast
  let value = case maybeAnnotation of
        Nothing -> implementation
        Just type' -> Annotation implementation type'
  pure $ Declaration name value
  where
  annotation = do
    name <- identifier
    shouldContinue <- (try (punctuation "::") <#> Just) <|> pure Nothing
    type' <- traverse (const $ withIndentation 2 ast) shouldContinue
    pure $ Tuple name type'

file :: Parser (Array TopLevelEntry)
file = many (assumption <|> declaration) <* match "eof"

---------- Compiler stage
type ParsingStageError e = 
  ( syntaxError :: ParseError -- TODO: better errors
  , emptyFile :: Unit
  | e )

parsingStage :: forall r e. Stage (ParsingStageError e) r (Array Token) (Array TopLevelEntry)
parsingStage tokens = case ZipperArray.fromArray tokens of
  Nothing -> throw emptyFile
  Just zipper -> case runIndent $ runParserT zipper file of
    Right result -> pure result
    Left error -> throw $ syntaxError error

---------- Stuff generated using snippets 
syntaxError :: forall r a. a -> Variant ( syntaxError :: a | r )
syntaxError = inj _syntaxError

_syntaxError :: SProxy "syntaxError"
_syntaxError = SProxy

emptyFile :: forall r. Variant ( emptyFile :: Unit | r )
emptyFile = inj _emptyFile unit

_emptyFile :: SProxy "emptyFile"
_emptyFile = SProxy
