module Main where

import Prelude

import Ansi.Output (italic, withGraphics)
import Check (CheckM, infer, runCheckM)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List ((:), List(..))
import Data.List as List
import Data.Map as Map
import Data.ZipperArray as ZipperArray
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Console (error, logShow)
import Eval (eval, quote', runEvalM)
import Lunarpie.Data.Ast (Ast, TopLevelEntry(..), runAstM, toTerm)
import Lunarpie.Language.Parser (Token, file, runIndent)
import String (errorText)
import Term (Environment, Name(..), Term(..), Value, valueToTerm)
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
  go :: List TopLevelEntry -> Effect Unit
  go = go' mempty

  go' :: { types :: Map.Map Name Value, values :: Map.Map Name Value } -> List TopLevelEntry -> Effect Unit
  go' _ Nil = pure unit
  go' past ((Declaration name ast):tail) = do
    let 
      term = toTerm' ast
      m = do
        type' <- infer term
        value <- eval term
        quoted <- quote' value
        pure { value, type', quoted }
    case runCheckM (mkEnv past) m of
      Left err -> do
        log $ errorText $ "Type error (" <> withGraphics italic name <> "):"  
        logShow err
      Right { type', value, quoted } -> do
        when (List.null tail) $ logShow $ Annotation quoted (valueToTerm type')
        let past' 
              = past 
                { values = Map.insert (Global name) value past.values
                , types = Map.insert (Global name) type' past.types 
                }
        go' past' tail
  go' past ((Axiom name ast):tail) = do
    let type' = runEvalM { global: past.values, local: mempty } $ eval $ toTerm' ast
    go' (past { types = Map.insert (Global name) type' past.types }) tail

  toTerm' :: Ast -> Term
  toTerm' value = runAstM mempty $ toTerm value

  mkEnv past = { types: mkCtx past.types, values: mkCtx past.values }
    where
    mkCtx global = { global, local: mempty }

