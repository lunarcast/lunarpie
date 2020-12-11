module Subsitution where

import Prelude

import Data.Natural (Natural)
import Term (Term(..))

-- TODO: perhaps update this to use recursion schemes?
substitute :: Natural -> Term -> Term -> Term
substitute index with term = case term of
  Annotation e t -> Annotation (self e) (self t)
  Pi from to -> Pi (self from) (substitute (succ index) with to)
  Bound at | at == index -> with
  Application f a -> Application (self f) (self a)
  Abstraction body -> Abstraction (substitute (succ index) with body)
  v -> v
  where 
  self = substitute index with

---------- Helpers
succ :: forall a. Semiring a => a -> a
succ = (+) one
