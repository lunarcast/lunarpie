module Lunarpie.Compiler.Check where

import Prelude

import Check (Action, TypeError, infer)
import Control.Plus (empty)
import Data.HashMap as HashMap
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
import Run (Run)
import Run.Reader (READER, Reader(..), local, runReader, runReaderAt)
import Run.State (STATE, evalState)
import Term (Context, Environment, Name(..), Term(..))
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type Values r = ( values :: Reader Environment | r )

type RTypeCheck r 
  = ( READER Context
    + Values 
    + STATE Natural
    + r )

type TypeCheckingErrors e =
  ( typeError :: ErrorStack Action TypeError
  | e )

runTypeCheck :: forall r. Run (RTypeCheck r) ~> Run r
runTypeCheck = runReaderAt _values emptyValues >>> runReader emptyContext >>> evalState zero
  where
  emptyValues = { local: mempty, global: Map.empty }
  emptyContext = Map.empty

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
  astToTerm = toTerm >>> runAstM emptyEnv
    where
    emptyEnv = HashMap.empty

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
_typeError :: Proxy "typeError"
_typeError = Proxy
