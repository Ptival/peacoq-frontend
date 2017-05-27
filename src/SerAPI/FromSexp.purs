module SerAPI.FromSexp (class FromSexp, fromSexp) where

import Prelude
import Data.Int (fromString)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Ports.Sexp (Sexp(..))

class FromSexp t where
  fromSexp :: Sexp -> Maybe t

fromAtom :: ∀ t. (String -> Maybe t) -> Sexp -> Maybe t
fromAtom f = case _ of
  Atom s -> f s
  _      -> Nothing

instance fromSexpBoolean :: FromSexp Boolean where
  fromSexp = fromAtom $ case _ of
    "True"  -> Just true
    "False" -> Just false
    _       -> Nothing

instance fromSexpInt :: FromSexp Int where
  fromSexp = fromAtom fromString

instance fromSexpList :: FromSexp t => FromSexp (List t) where
  fromSexp = case _ of
    List l -> fromFoldable <$> traverse fromSexp l
    _      -> Nothing
    

instance fromSexpString :: FromSexp String where
  fromSexp = fromAtom Just
