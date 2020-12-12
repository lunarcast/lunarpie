module Main where

import Prelude

import Check (CheckM, infer, runCheckM)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List ((:), List(..))
import Data.List as List
import Data.Map as Map
import Data.Tuple (Tuple(..))
import Data.ZipperArray as ZipperArray
import Effect (Effect)
import Effect.Console (error, logShow)
import Eval (eval)
import Lunarpie.Data.Ast (Ast, Declaration(..), runAstM, toTerm)
import Lunarpie.Language.Parser (Token, file)
import Term (Environment, Name(..), Term(..), Value, valueToTerm)
import Text.Parsing.Indent (runIndent)
import Text.Parsing.Parser (ParseError(..), runParserT)
import Text.Parsing.Parser.Pos (Position(..))

types :: Environment
types = mempty

values :: Environment
values = mempty

evalAndInfer :: forall r. Term -> CheckM r Term
evalAndInfer x = do
  ty <- infer x
  value <- eval x
  pure $ Annotation (valueToTerm value) (valueToTerm ty)

-- TODO: getting the tokens here is kinda hacky, fix pls
main :: Array Token -> Effect Unit
main tokens = do
  for_ (ZipperArray.fromArray tokens) \tokens' -> do
    -- logShow plus'
    -- case runCheckM { types, values } m of
    --   Right v -> logShow v
    --   Left e -> error $ show e
    case runIndent (runParserT tokens' file) of
      Left (ParseError message (Position { line, column })) -> error $ "Parsing error (" <> show line <> ":" <> show column <> "): " <> message
      Right ast -> go $ List.fromFoldable ast
  where -- This is some very messy code I use for debugging
  go :: List Declaration -> Effect Unit
  go = go' Nil

  go' :: List (Tuple String Value) -> List Declaration -> Effect Unit
  go' _ Nil = pure unit
  go' past ((Declaration { value: last }):Nil) = do
    handleCheck true (pure unit) $ runCheckM (mkEnv past) (evalAndInfer $ toTerm' last) 
  go' past (Declaration { name, value }:tail) = do
    let 
      term = toTerm' value
      past' = case runCheckM (mkEnv past) $ eval term of
        Left err -> past 
        Right new -> (Tuple name new):past
    handleCheck false (go' past' tail) $ runCheckM (mkEnv past) (infer term)

  handleCheck :: forall a. Show a => Boolean -> Effect Unit -> Either _ a -> Effect Unit
  handleCheck shouldLog next = case _ of
    Left err -> logShow err
    Right result -> do
      when shouldLog $ logShow result
      next

  toTerm' :: Ast -> Term
  toTerm' value = runAstM mempty $ toTerm value

  mkEnv :: List (Tuple String Value) -> _
  mkEnv past = { types, values: { global: Map.fromFoldable $ lmap Global <$> past, local: mempty } }


