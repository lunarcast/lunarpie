module Lunarpie.Data.Ast where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Foldable (foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..))
import Data.Natural (Natural)
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

newtype Declaration = Declaration { name :: String, value :: Ast }

---------- Generating terms
type AstEnv = HashMap.HashMap String Natural

type AstM = Run 
  ( reader :: READER AstEnv )

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
toTerm (Lambda arg body) = withValue arg $ Term.Abstraction <$> toTerm body
toTerm (Pi binder from to) = Term.Pi <$> toTerm from <*> case binder of
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
derive instance genericAst :: Generic Ast _

instance showAst :: Show Ast where
  show a = genericShow a

derive instance genericDeclaration :: Generic Declaration _

instance showDeclaration :: Show Declaration where
  show (Declaration { name, value }) = name <> ": " <> show value

instance debugAst :: Debug Ast where
  debug a = genericDebug a

instance debugDeclaration :: Debug Declaration where
  debug = genericDebug
