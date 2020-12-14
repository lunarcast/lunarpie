module Lunarpie.Data.Foreign where

import Data.Either (Either(..))

-- | Config usable from the js side to work with either values
type EitherConfig
  = { left :: forall e a. e -> Either e a
    , right :: forall e a. a -> Either e a
    }

eitherConfig :: EitherConfig
eitherConfig = { left: Left, right: Right }
