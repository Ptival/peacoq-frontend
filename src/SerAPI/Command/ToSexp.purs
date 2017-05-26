module SerAPI.Command.ToSexp (class ToSexp, toSexp) where

import Data.String (Pattern(..), Replacement(..), replaceAll)
import Prelude (show)

class ToSexp t where
  toSexp :: t -> String

instance toSexpBoolean :: ToSexp Boolean where
  toSexp true  = "True"
  toSexp false = "False"

instance toSexpInt :: ToSexp Int where
  toSexp = show

instance toSexpString :: ToSexp String where
  toSexp = replaceAll (Pattern "\"") (Replacement "\\\"")
