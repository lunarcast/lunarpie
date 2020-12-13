module Check where

import Prelude

import Data.Either (Either)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Natural (Natural)
import ErrorStack (EXCEPT_STACKED, ErrorStack, throw, while)
import Eval (EvalM, _global, _values, areEqual, call, eval, getValues, localValues, quote')
import Run (extract)
import Run.Except (runExcept)
import Run.Reader (READER, ask, local, runReader, runReaderAt)
import Run.State (STATE, evalState, get, modify)
import String (errorText, indent, unlines)
import Subsitution (substitute)
import Term (Environment, Name(..), Term(..), Value(..))

type CheckM r = EvalM 
  ( reader :: READER Environment
  -- TODO: better error managment
  , except :: EXCEPT_STACKED Action TypeError
  , state :: STATE Natural 
  | r )

data TypeError
  = TypeMissmatch { inferred :: Value, expected :: Value, term :: Term }
  | IllegalApplication { function :: Term, argument :: Term, functionType :: Value }
  | NameNotInScope Name
  | CannotInfer Term
  | NotAPi Term Value

data Action
  = Inferring Term
  | Checking Term Value

---------- Helpers
runCheckM :: forall a. { types :: Environment, values :: Environment } -> CheckM () a -> Either (ErrorStack Action TypeError) a
runCheckM { types, values } = evalState zero >>> runReaderAt _values values >>> runReader types >>> runExcept >>> extract

getId :: forall r. CheckM r Natural
getId = get <* modify ((+) one)

withValue :: forall a r. Name -> Value -> CheckM r a -> CheckM r a
withValue name value = localValues (over (prop _global) $ Map.insert name value)

withType :: forall a r. Name -> Value -> CheckM r a -> CheckM r a
withType name value = local (over (prop _global) $ Map.insert name value)

---------- Checking and inference
check :: forall r. Term -> Value -> CheckM r Unit
check term value = while (Checking term value) $ check' term value

check' :: forall r. Term -> Value -> CheckM r Unit
check' Star VStar = pure unit
check' (Abstraction body) (VPi from to) = do
  unknown <- getId <#> Local
  withType unknown from do 
    to' <- call to $ Free unknown 
    let return = substitute zero (Free unknown) body 
    check return to'
check' expr@(Abstraction _) other = throw $ NotAPi expr other
check' term expected = do
  inferred <- infer term
  equivalent <- areEqual inferred expected
  unless equivalent do
    a <- quote' inferred
    b <- quote' expected
    throw $ TypeMissmatch { inferred, expected, term } 

infer :: forall r. Term -> CheckM r Value
infer term = while (Inferring term) $ infer' term

infer' :: forall r. Term -> CheckM r Value
infer' Star = pure VStar
infer' (Annotation term annotation) = do
  check annotation VStar
  expected <- eval annotation
  check term expected
  pure expected
infer' (Free name) = ask <#> _.global >>= \ctx -> case Map.lookup name ctx of
  Just ty -> pure ty
  Nothing -> getValues <#> _.global >>= \ctx' -> case Map.lookup name ctx' of
    Just value -> quote' value >>= infer
    Nothing -> throw $ NameNotInScope name
infer' (Pi from to) = do
  check from VStar
  from' <- eval from
  unknown <- getId <#> Local
  withType unknown from' do
    let to' = substitute zero (Free unknown) to
    check to' VStar
  pure VStar
infer' (Application function argument) = do
  functionType <- infer function
  case functionType of
    VPi from to -> do
      check argument from
      call to argument
    _ -> throw $ IllegalApplication { function, argument, functionType }
infer' term = throw $ CannotInfer term

---------- Typeclass instances
instance showTypeError :: Show TypeError where
  show (CannotInfer term) = unlines 
    [ errorText "Cannot infer the type of"
    , indent 2 $ show term
    ]
  show (TypeMissmatch { inferred, expected, term }) = unlines
    [ errorText "Cannot match expected type"
    , indent 2 $ show expected
    , errorText "with type"
    , indent 2 $ show inferred
    , errorText "for term"
    , indent 2 $ show term ]
  show (NameNotInScope name) = errorText $ "Name " <> show name <> " not in scope."
  show (IllegalApplication { function, argument, functionType }) = unlines 
    [ errorText "Cannot apply function" 
    , indent 2 $ show function
    , errorText "to argument"
    , indent 2 $ show argument
    , errorText "because type"
    , indent 2 $ show functionType
    , errorText "is not a pi type" ]
  show (NotAPi _ type') = unlines
    [ errorText "Type"
    , indent 2 $ show type'
    , errorText "is not a pi type" ]

instance showAction :: Show Action where
  show (Checking term ty) = unlines
    [ errorText "ensuring term"
    , indent 2 $ show term
    , errorText "has type"
    , indent 2 $ show ty]
  show (Inferring term) = unlines
    [ errorText "inferring the type of term"
    , indent 2 $ show term]
