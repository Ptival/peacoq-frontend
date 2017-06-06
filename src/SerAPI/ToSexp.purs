module SerAPI.Command.ToSexp (class ToSexp, toSexp) where

import Prelude
import Data.Foldable (fold, intercalate)
import Data.String (Pattern(..), Replacement(..), replaceAll)

class ToSexp t where
  toSexp :: t -> String

instance toSexpBoolean :: ToSexp Boolean where
  toSexp true  = "True"
  toSexp false = "False"

instance toSexpInt :: ToSexp Int where
  toSexp = show

instance toSexpString :: ToSexp String where
  toSexp s = fold ["\"", replaceAll (Pattern "\"") (Replacement "\\\"") s, "\""]

instance toSexpList :: ToSexp t => ToSexp (Array t) where
  toSexp l = fold ["(", intercalate " " (toSexp <$> l), ")"]
