module Eval where

import Prelude

import Data.Lens (over, set)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Natural (natToInt)
import Run (Run, SProxy(..), extract)
import Run.Reader (READER, askAt, localAt, runReaderAt)
import Subsitution (substitute)
import Term (Name(..), Term(..), VFunction, Value(..), Environment)

type EvalM r = Run 
  ( values :: READER Environment
  | r )

---------- Helpers
runEvalM :: forall a. Environment -> EvalM () a -> a
runEvalM e = runReaderAt _values e >>> extract

getValues :: forall r. EvalM r Environment
getValues = askAt _values

localValues :: forall r a. (Environment -> Environment) -> EvalM r a -> EvalM r a
localValues = localAt _values

createFunction :: forall r. Term -> EvalM r VFunction
createFunction term = getValues <#> { closure: _, term }

---------- Evaluation
eval :: forall r. Term -> EvalM r Value
eval Star = pure VStar
eval (Abstraction body) = VLambda <$> createFunction body
eval (Annotation term _) = eval term
eval (Pi from to) = VPi <$> eval from <*> createFunction to
eval (Free name) = getValues <#> _.global <#> \ctx -> fromMaybe (VFree name) $ Map.lookup name ctx
eval (Bound i) = getValues <#> _.local <#> \env -> fromMaybe (VFree $ Local i) $ List.index env (natToInt i)
eval (Application f argument) = do
  argument' <- eval argument
  eval f >>= case _ of
    VLambda { closure, term } -> localValues ctx $ eval term
      where
      ctx = set (prop _local) closure.local >>> over (prop _local) (Cons argument')
    f' -> pure $ VApp f' argument'

call :: forall r. VFunction -> Term -> EvalM r Value
call { closure, term } argument = localValues (const closure) $ eval $ substitute zero argument term 

-- SProxies
_local :: SProxy "local"
_local = SProxy

_global :: SProxy "global"
_global = SProxy

_values :: SProxy "values"
_values = SProxy
