module Lunarpie.Compiler.Stage where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
import Prim.Row (class Cons)
import Run (Run)
import Run.Except (EXCEPT, Except, runExcept, throw)
import Type.Proxy (Proxy)
import Type.Row (type (+))


type RStage :: forall k. Row Type -> Row (k -> Type) -> Row (k -> Type)
type RStage e r = ( except :: Except (Variant e) | r )
type Stage e r i o = i -> Run (RStage e r) o

liftError :: 
  forall r error extraErrors errorRow name a. 
  IsSymbol name =>
  Cons name error extraErrors errorRow =>
  Proxy name -> 
  Run ( EXCEPT error + EXCEPT (Variant errorRow) + r ) a -> 
  Run ( EXCEPT (Variant errorRow) r ) a
liftError name m = do
  either <- runExcept m
  case either of
    Right a -> pure a
    Left err -> throw $ inj name err
