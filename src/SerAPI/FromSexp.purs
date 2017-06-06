module SerAPI.FromSexp (class FromSexp, fromSexp) where

import Prelude
import Data.Array as Array
import Data.List as List
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(Pattern), stripPrefix, stripSuffix)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Ports.Sexp (Sexp(..))
import Unsafe.Coerce (unsafeCoerce)

class FromSexp t where
  fromSexp :: Sexp -> Maybe t

fromAtom :: ∀ t. (String -> Maybe t) -> Sexp -> Maybe t
fromAtom f = case _ of
  Atom s -> f s
  _      -> Nothing

quoted :: ∀ t. (String -> Maybe t) -> (String -> Maybe t)
quoted p = stripPrefix (Pattern "\"") >=> stripSuffix (Pattern "\"") >=> p

instance fromSexpBoolean :: FromSexp Boolean where
  fromSexp = fromAtom $ case _ of
    "True"  -> Just true
    "False" -> Just false
    _       -> Nothing

instance fromSexpInt :: FromSexp Int where
  fromSexp = fromAtom fromString

instance fromSexpMaybe :: FromSexp t => FromSexp (Maybe t) where
  fromSexp = case _ of
    List [e] -> Just <$> fromSexp e
    List []  -> Just Nothing
    _        -> Nothing

todo :: ∀ a. a
todo = unsafeCoerce unit

instance fromSexpTuple :: (FromSexp a, FromSexp b) => FromSexp (Tuple a b) where
  fromSexp = case _ of
    List [ sa, sb ] -> do
      a <- fromSexp sa
      b <- fromSexp sb
      pure $ Tuple a b
    _ -> Nothing

instance fromSexpArray :: FromSexp t => FromSexp (Array t) where
  fromSexp = case _ of
    List l -> Array.fromFoldable <$> traverse fromSexp l
    _      -> Nothing

instance fromSexpList :: FromSexp t => FromSexp (List.List t) where
  fromSexp = case _ of
    List l -> List.fromFoldable <$> traverse fromSexp l
    _      -> Nothing

instance fromSexpString :: FromSexp String where
  fromSexp = fromAtom Just
