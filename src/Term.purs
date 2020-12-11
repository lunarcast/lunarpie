module Term where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, italic, withGraphics)
import Data.Foldable (class Foldable, foldl)
import Data.List (List)
import Data.Map (Map)
import Data.Natural (Natural, intToNat)
import String (parenthesis)

data Term
  -- "Value" level stuff
  = Abstraction Term
  | Annotation Term Term
  | Application Term Term
  | Bound Natural
  | Free Name
  -- "Type" level stuff
  | Pi Term Term
  | Star

data Name
  = Local Natural
  | Global String

type VFunction = { closure :: Environment, term :: Term }

data Value 
  = VStar
  | VLambda VFunction
  | VPi Value VFunction
  | VFree Name
  | VApp Value Value

type LocalEnv = List Value
type Context = Map Name Value
type Environment = { local :: LocalEnv, global :: Context }

---------- Helpers
application :: forall f. Foldable f => Term -> f Term -> Term
application = foldl Application  

boundInt :: Int -> Term
boundInt = intToNat >>> Bound

-- Non dependent function type
vArrow :: Value -> Term -> Value
vArrow a b = VPi a { closure: mempty, term: b }

valueToTerm :: Value -> Term
valueToTerm VStar = Star
valueToTerm (VLambda { term }) = Abstraction term
valueToTerm (VPi from { term }) = Pi (valueToTerm from) term
valueToTerm (VFree name) = Free name
valueToTerm (VApp f a) = Application (valueToTerm f) (valueToTerm a)

---------- Typeclass instances
derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

derive instance eqTerm :: Eq Term
derive instance eqValue :: Eq Value

arrow :: String
arrow = withGraphics (foreground Yellow) " -> "

star :: String
star = withGraphics (foreground BrightGreen) "*"

instance showName :: Show Name where
  show = go 
    where 
    go (Local id) = withGraphics (foreground White) "?" <> style (show id)
    go (Global name) = style name

    style = withGraphics (foreground White <> italic)

instance showTerm :: Show Term where
  show Star = star
  show (Free name) = show name
  show (Bound id) = withGraphics (foreground White) $ show id
  show (Annotation term type') = parenthesisWhen lhs (show term) <> withGraphics (foreground BrightGreen) " :: " <> show type'
    where
    lhs = case term of
      Abstraction _ -> true
      Annotation _ _ -> true
      Pi _ _ -> true
      _ -> false
  show (Abstraction v) = withGraphics (foreground BrightYellow) "λ" <> show v
  show (Pi from to) = parenthesisWhen lhs (show from) <> arrow <> show to
    where
    lhs = case from of
      Pi _ _-> true
      Abstraction _ -> true
      _ -> false
  show (Application f a) = parenthesisWhen lhs (show f) <> " " <> parenthesisWhen rhs (show a)
    where
    rhs = case a of
      Application _ _ -> true
      Annotation _ _ -> true
      Pi _ _ -> true
      _ -> false

    lhs = case f of
      Abstraction _ -> true
      Annotation _ _ -> true
      Pi _ _ -> true
      _ -> false

parenthesisWhen :: Boolean -> String -> String
parenthesisWhen true s = parenthesis s
parenthesisWhen false s = s 

instance showValue :: Show Value where
  show = valueToTerm >>> show
