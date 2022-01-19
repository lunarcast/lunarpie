module Subsitution where

import Prelude

import Data.Natural (Natural, intToNat, natToInt)
import Term (Term(..))

-- TODO: perhaps update this to use recursion schemes?
substitute :: Natural -> Term -> Term -> Term
substitute index with term = case term of
  Annotation e t -> Annotation (self e) (self t)
  Pi argName from to -> Pi argName (self from) (substitute (succ index) with to)
  Bound at | at == index -> with
  Application f a -> Application (self f) (self a)
  Abstraction argName body -> Abstraction argName (substitute (succ index) with body)
  v -> v
  where 
  self = substitute index with

shift :: Int -> Natural -> Term -> Term
shift index past term = case term of
  Annotation e t -> Annotation (self e) (self t)
  Pi argName from to -> Pi argName (self from) (shift index (succ past) to)
  Bound at | at >= past -> Bound (intToNat $ index + natToInt at)
  Application f a -> Application (self f) (self a)
  Abstraction argName body -> Abstraction argName (shift index (succ past) body)
  v -> v
  where 
  self = shift index past

---------- Helpers
succ :: forall a. Semiring a => a -> a
succ = (+) one
