module Lunarpie.Data.Ast where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Foldable (foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Natural (Natural)
import Data.Show.Generic (genericShow)
import Run (Run, extract)
import Run.Reader (READER, ask, local, runReader)
import Subsitution (succ)
import Term as Term

data Ast
  = Var String
  | Lambda String Ast
  | Annotation Ast Ast
  | Application Ast Ast
  | Pi (Maybe String) Ast Ast
  | Star

data TopLevelEntry 
  = Declaration String Ast
  | Axiom String Ast

type Module = Array TopLevelEntry

---------- Generating terms
type AstEnv = HashMap.HashMap String Natural

type AstM = Run 
  ( READER AstEnv ())

-- | Introduce a new variable in scope
withValue :: forall a. String -> AstM a -> AstM a
withValue name = local (map succ >>> HashMap.insert name zero)

-- | Run the ast monad
runAstM :: forall a. AstEnv -> AstM a -> a
runAstM env = runReader env >>> extract

-- | Generate the coresponding term for a given ast
toTerm :: Ast -> AstM Term.Term
toTerm Star = pure Term.Star
toTerm (Var name) = ask <#> \ctx -> case HashMap.lookup name ctx of
  Just index -> Term.Bound index
  Nothing -> Term.Free $ Term.Global name
toTerm (Application f a) = Term.Application <$> toTerm f <*> toTerm a
toTerm (Annotation t a) = Term.Annotation <$> toTerm t <*> toTerm a
toTerm (Lambda arg body) = withValue arg $ Term.Abstraction arg <$> toTerm body
toTerm (Pi binder from to) = Term.Pi (fromMaybe "" binder) <$> toTerm from <*> case binder of
  Nothing -> local (map succ) $ toTerm to
  Just name -> withValue name $ toTerm to

---------- Helpers
-- | Construct a curried lambdia
curriedLambda :: Array String -> Ast -> Ast
curriedLambda arguments body = foldr Lambda body arguments

-- | Desugar a call with multiple arguments
manyCalls :: Ast -> Array Ast -> Ast
manyCalls = foldl Application

---------- Typeclass instances
derive instance Generic Ast _

instance Show Ast where
  show a = genericShow a

derive instance Generic TopLevelEntry _

instance Debug Ast where
  debug a = genericDebug a

instance Debug TopLevelEntry where
  debug = genericDebug

