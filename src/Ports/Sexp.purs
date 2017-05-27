module Ports.Sexp where

import Prelude
import Data.Function.Uncurried (Fn5, runFn5)
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))

data Sexp
  = Atom String
  | List (Array Sexp)

derive instance genericSexp :: Generic Sexp

instance showSexp :: Show Sexp where
  show = gShow

foreign import _sexp :: Fn5 (Maybe Sexp) (Sexp -> Maybe Sexp) (String -> Sexp) (Array Sexp -> Sexp) String (Maybe Sexp)
sexp :: String -> Maybe Sexp
sexp = runFn5 _sexp Nothing Just Atom List
