module Lunarpie.Compiler.Check where

import Prelude

import Check (Action, TypeError, infer)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Natural (Natural)
import Data.Traversable (sequence)
import Data.ZipperArray as ZipperArray
import ErrorStack (ErrorStack)
import Eval (_global, _values, eval, localValues, quote')
import Lunarpie.Compiler.Stage (Stage, liftError)
import Lunarpie.Data.Ast (Module, TopLevelEntry(..), runAstM, toTerm)
import Run (Run, SProxy(..))
import Run.Reader (READER, local, runReader, runReaderAt)
import Run.State (STATE, evalState)
import Term (Context, Environment, Name(..), Term(..))

type RTypeCheck r 
  = ( reader :: READER Context
    , values :: READER Environment
    , state :: STATE Natural
    | r )

type TypeCheckingErrors e =
  ( typeError :: ErrorStack Action TypeError
  | e )

runTypeCheck :: forall r. Run (RTypeCheck r) ~> Run r
runTypeCheck = runReaderAt _values mempty >>> runReader mempty >>> evalState zero

typeCheckingStage :: forall r e. Stage (TypeCheckingErrors e) r Module (Maybe Term)
typeCheckingStage = typecheckModule >>> runTypeCheck

--------- Actual module typechecking logic
typecheckModule :: forall r e. Stage (TypeCheckingErrors e) (RTypeCheck r) Module (Maybe Term)
typecheckModule declarations = case ZipperArray.fromArray declarations of
  Nothing -> pure Nothing
  Just zipper -> typecheckModule' zipper

typecheckModule' :: forall r e. Stage (TypeCheckingErrors e) (RTypeCheck r) (ZipperArray.ZipperArray TopLevelEntry) (Maybe Term)
typecheckModule' declarations = case ZipperArray.current declarations of
  Declaration name ast -> do
    type' <- liftError _typeError $ infer term
    value <- eval term
    with name type' (Just value) $ continue $ Just ado
      quotedValue <- quote' value
      quotedType <- quote' type'
      in Annotation quotedValue quotedType
    where
    term = astToTerm ast
  Axiom name ast -> do 
    type' <- eval $ astToTerm ast
    with name type' Nothing $ continue Nothing
  where
  astToTerm = toTerm >>> runAstM mempty 

  continue last = case ZipperArray.goNext declarations of
    Nothing -> sequence last
    Just next -> typecheckModule' next

  with name type' maybeValue = local (Map.insert name' type') >>> localValues updateValues
    where
    updateValues = case maybeValue of
      Nothing -> identity
      Just value -> over (prop _global) $ Map.insert name' value 
    name' = Global name

---------- Codegen stuff
_typeError :: SProxy "typeError"
_typeError = SProxy
