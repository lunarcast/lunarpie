module Eval where

import Prelude

import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.List (List(..))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe')
import Data.Natural (Natural, natToInt, (+-))
import Effect.Class.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Run (Run, SProxy(..), extract)
import Run.Reader (READER, askAt, localAt, runReaderAt)
import Subsitution (shift, substitute, succ)
import Term (Environment, Name(..), Neutral(..), Term(..), VFunction, Value(..))

type EvalM r = Run 
  ( values :: READER Environment
  | r )

type QuoteM r = EvalM
  ( depth :: READER Natural
  | r )

---------- Helpers
runEvalM :: forall a. Environment -> EvalM () a -> a
runEvalM e = runReaderAt _values e >>> extract

runQuoteM :: forall a r. QuoteM r a -> EvalM r a
runQuoteM = runReaderAt _depth zero

getValues :: forall r. EvalM r Environment
getValues = askAt _values

localValues :: forall r a. (Environment -> Environment) -> EvalM r a -> EvalM r a
localValues = localAt _values

createFunction :: forall r. Term -> EvalM r VFunction
createFunction term = getValues <#> { closure: _, term }

increaseDepth :: forall r a. QuoteM r a -> QuoteM r a 
increaseDepth = localAt _depth succ

createQuote :: forall r. QuoteM r Term
createQuote = askAt _depth <#> Quote <#> Free 

---------- Evaluation
eval :: forall r. Term -> EvalM r Value
eval Star = pure VStar
eval (Abstraction body) = VLambda <$> createFunction body
eval (Annotation term _) = eval term
eval (Pi from to) = VPi <$> eval from <*> createFunction to
eval (Free name) = getValues <#> _.global <#> \ctx -> fromMaybe (Neutral $ NFree name) $ Map.lookup name ctx
eval (Bound i) = getValues <#> _.local <#> \env -> fromMaybe' (notInScope env) $ List.index env (natToInt i)
  where
  notInScope scope _ = let 
      a = unsafePerformEffect $ logShow scope 
    in unsafeCrashWith $ "Variable " <> show i <> " is not in scope " 
eval (Application f argument) = do
  eval f >>= case _ of
    VLambda function -> call function argument
    (Neutral neutral) -> do
      argument' <- eval argument
      pure $ Neutral $ NApp neutral argument'
    _ -> unsafeCrashWith "TODO: handle case"

call :: forall r. VFunction -> Term -> EvalM r Value
call { closure, term } argument = do
  argument' <- eval argument 
  let ctx = set (prop _local) $ Cons argument' closure.local 
  localValues ctx $ eval term

quote' :: forall r. Value -> EvalM r Term
quote' = quote >>> runQuoteM

-- | Generate a term from a value with a few guarantees:
-- | - Everything is eta-reduced
-- | - There are no unapplied lambdas
quote :: forall r. Value -> QuoteM r Term
quote (VLambda function) = do
  argument <- createQuote
  increaseDepth do
    result <- call function argument
    quoted <- quote result
    pure case etaReduce quoted of
      Just t -> t
      Nothing -> Abstraction quoted
quote (VPi from to) = do
  from' <- quote from
  argument <- createQuote
  increaseDepth do
    result <- call to argument
    Pi from' <$> quote result
quote (Neutral neutral) = quoteNeutral neutral
quote VStar = pure Star

quoteNeutral :: forall r. Neutral -> QuoteM r Term
quoteNeutral (NFree (Quote depth)) = do
  currentDepth <- askAt _depth
  pure $ Bound $ currentDepth +- depth +- one
quoteNeutral (NApp f a) = do 
  Application <$> quoteNeutral f <*> quote a
quoteNeutral (NFree name) = pure $ Free name

areEqual :: forall r. Value -> Value -> EvalM r Boolean
areEqual a b = runQuoteM $ (==) <$> quote a <*> quote b

references :: Natural -> Term -> Boolean
references index term = substitute index (Free $ Quote zero) term /= term  

etaReduce :: Term -> Maybe Term
etaReduce (Application body (Bound i)) | i == zero && not (references zero body) = Just $ shift (-1) zero body
etaReduce other =  Nothing


-- SProxies
_local :: SProxy "local"
_local = SProxy

_global :: SProxy "global"
_global = SProxy

_values :: SProxy "values"
_values = SProxy

_depth :: SProxy "depth"
_depth = SProxy
