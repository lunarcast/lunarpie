module Main where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (errorShow)
import Effect.Console (logShow)
import Lunarpie.Compiler.Check (TypeCheckingErrors, typeCheckingStage)
import Lunarpie.Compiler.Stage (Stage)
import Lunarpie.Language.Lexer (LexingError, lexerStage)
import Lunarpie.Language.Parser (ParsingStageError, parsingStage)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Run (extract)
import Run.Except (runExcept)
import Term (Term)
import Type.Row (type (+))

main :: Effect Unit
main = do
  text <- readTextFile UTF8 "examples/test.lpi"
  case extract $ runExcept $ stage text of
    Left err -> errorShow err
    Right Nothing -> pure unit
    Right (Just term) -> logShow term
  pure unit

stage :: forall r. Stage (LexingError + ParsingStageError + TypeCheckingErrors + ()) r String (Maybe Term)
stage = lexerStage >=> parsingStage >=> typeCheckingStage


