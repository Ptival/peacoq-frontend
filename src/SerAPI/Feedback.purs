module SerAPI.Feedback where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Ports.Sexp (Sexp(..))
import SerAPI.FromSexp (class FromSexp, fromSexp)
import SerAPI.Types (StateId)

data Contents
  = FileDependency (List String) String
  | FileLoaded String String
  | Processed
  | ProcessingIn String

derive instance genericContents :: Generic Contents

instance showContents :: Show Contents where
  show = gShow

instance fromSexpContents :: FromSexp Contents where
  fromSexp = case _ of
    List [ Atom "FileDependency"
         , l
         , Atom f
         ] -> do
      ps <- fromSexp l
      pure $ FileDependency ps f
    List [ Atom "FileLoaded", Atom s1, Atom s2 ] -> Just $ FileLoaded s1 s2
    Atom "Processed" -> Just Processed
    List [ Atom "ProcessingIn", Atom s ] -> Just $ ProcessingIn s
    _ -> Nothing

data Feedback = Feedback
  { id       :: StateId
  , contents :: Contents
  , route    :: Int
  }

derive instance genericFeedback :: Generic Feedback

instance showFeedback :: Show Feedback where
  show = gShow

instance fromSexpFeedback :: FromSexp Feedback where
  fromSexp = case _ of
    List [ Atom "Feedback"
         , List [ List [ Atom "id",
                         List [ Atom "State"
                              , sid
                              ]
                       ]
                , List [ Atom "contents"
                       , scontents 
                       ]
                , List [ Atom "route"
                       , sroute 
                       ]
                ]
         ]
      -> do
      id       <- fromSexp sid
      contents <- fromSexp scontents
      route    <- fromSexp sroute
      pure $ Feedback { id, contents, route }
    _ -> Nothing
