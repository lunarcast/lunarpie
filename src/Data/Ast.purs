module Lunarpie.Data.Ast where

import Prelude

import Data.Debug (class Debug, genericDebug)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)

data Ast
  = Var String
  | Lambda String Ast
  | Annotation Ast Ast
  | Application Ast Ast
  | Pi (Maybe String) Ast Ast
  | Star

newtype Declaration = Declaration { name :: String, value :: Ast }

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
