module Lunarpie.Compiler.Stage where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy, Variant, inj)
import Prim.Row (class Cons)
import Run (Run)
import Run.Except (EXCEPT, runExcept, throw)

type RStage e r = ( except :: EXCEPT (Variant e) | r )
type Stage e r i o = i -> Run (RStage e r) o

liftError :: 
  forall r error extraErrors errorRow name a. 
  IsSymbol name =>
  Cons name error extraErrors errorRow =>
  SProxy name -> 
  Run ( except :: EXCEPT error, except :: EXCEPT (Variant errorRow) | r ) a -> 
  Run ( except :: EXCEPT (Variant errorRow) | r ) a
liftError name m = do
  either <- runExcept m
  case either of
    Right a -> pure a
    Left err -> throw $ inj name err
