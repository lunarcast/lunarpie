module Main where

import Prelude

import Check (CheckM, infer, runCheckM)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Natural (intToNat)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (error)
import Eval (eval)
import Term (Environment, Name(..), Term(..), Value(..), application, boundInt, vArrow, valueToTerm)

id :: Term
id = Abstraction (Bound zero)

natural_ :: Name
natural_ = Global "Natural"

succ_ :: Name
succ_ = Global "S"

zero_ :: Name
zero_ = Global "Z"

-- (n : Type) -> (n -> n) -> n -> n
nat :: Term
nat = Pi Star $ Pi (Pi (Bound zero) (Bound one)) $ Pi (Bound one) (Bound $ intToNat 2)

-- _ -> _ -> identity
zero' :: Term
zero' = Annotation term nat
  where
  term = Abstraction $ Abstraction id 

one' :: Term
one' = Annotation term nat
  where
  term = Abstraction $ Abstraction $ Abstraction $ Application (Bound one) (Bound zero)

plus' :: Term
plus' = Annotation term ty
  where
  ty = Pi nat (Pi nat nat)

  term 
    = Abstraction -- n
    $ Abstraction -- m
    $ Abstraction -- type
    $ Abstraction -- succ 
    $ Abstraction -- zero
    $ application (boundInt 4) 
      [ boundInt 2
      , boundInt 1  
      , application (boundInt 3) [ boundInt 2, boundInt 1, boundInt 0 ]
      ] 

types :: Environment
types = { local: mempty, global } 
  where
  global = Map.fromFoldable
    [ Tuple natural_ VStar
    , Tuple zero_ (VFree natural_)
    , Tuple succ_ (vArrow (VFree natural_) (Free natural_))]

values :: Environment
values = { local: mempty, global: mempty } 

evalAndInfer :: forall r. Term -> CheckM r Term
evalAndInfer x = do
  ty <- infer x
  value <- eval x
  pure $ Annotation (valueToTerm value) (valueToTerm ty)

m :: CheckM () Term
m = evalAndInfer 
  -- $ application one' [Free natural_]
  $ application plus' [one', one', Free natural_, Free succ_, Free zero_] 
  -- $ application plus' [ one', one', Free natural_ ]

main :: Effect Unit
main = do
  -- logShow plus'
  case runCheckM { types, values } m of
    Right v -> logShow v
    Left e -> error $ show e
