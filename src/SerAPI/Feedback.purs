module SerAPI.Feedback where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Ports.Sexp (Sexp(..))
import SerAPI.FromSexp (class FromSexp, fromSexp)
import SerAPI.Types (EditId, Loc, StateId)

data Level
  = Debug
  | Info
  | Notice
  | Warning
  | Error

derive instance genericLevel :: Generic Level

instance showLevel :: Show Level where
  show = gShow

instance fromSexpLevel :: FromSexp Level where
  fromSexp = case _ of
    Atom "Debug"   -> Just Debug
    Atom "Info"    -> Just Info
    Atom "Notice"  -> Just Notice
    Atom "Warning" -> Just Warning
    Atom "Error"   -> Just Error
    _         -> Nothing

data FeedbackContent
  = Processed
  | Incomplete
  | Complete
  | ProcessingIn String
  | InProgress Int
  | WorkerStatus String String
  | AddedAxiom
  | GlobRef Loc String String String String
  | GlobDef Loc String String String
  | FileDependency (Maybe String) String
  | FileLoaded String String
  | Message Level Loc Sexp --TODO: StdPPCmds

derive instance genericFeedbackContent :: Generic FeedbackContent

instance showFeedbackContent :: Show FeedbackContent where
  show = gShow

instance fromSexpFeedbackContent :: FromSexp FeedbackContent where
  fromSexp = case _ of
    Atom "Processed" -> Just Processed
    Atom "Incomplete" -> Just Incomplete
    Atom "Complete" -> Just Complete
    List [ Atom "ProcessingIn", ss ] -> do
      s <- fromSexp ss
      pure $ ProcessingIn s
    List [ Atom "InProgress", si ] -> do
      i <- fromSexp si
      pure $ InProgress i
    List [ Atom "WorkerStatus", ss1, ss2 ] -> do
      s1 <- fromSexp ss1
      s2 <- fromSexp ss2
      pure $ WorkerStatus s1 s2
    Atom "AddedAxiom" -> Just AddedAxiom
    List [ Atom "GlobRef", sl, ss1, ss2, ss3, ss4 ] -> do
      l  <- fromSexp sl
      s1 <- fromSexp ss1
      s2 <- fromSexp ss2
      s3 <- fromSexp ss3
      s4 <- fromSexp ss4
      pure $ GlobRef l s1 s2 s3 s4
    List [ Atom "GlobDef", sl, ss1, ss2, ss3 ] -> do
      l  <- fromSexp sl
      s1 <- fromSexp ss1
      s2 <- fromSexp ss2
      s3 <- fromSexp ss3
      pure $ GlobDef l s1 s2 s3
    List [ Atom "FileDependency", sm, ss ] -> do
      m <- fromSexp sm
      s <- fromSexp ss
      pure $ FileDependency m s
    List [ Atom "FileLoaded", ss1, ss2 ] -> do
      s1 <- fromSexp ss1
      s2 <- fromSexp ss2
      pure $ FileLoaded s1 s2
    List [ Atom "Message", sl1, sl2, ss ] -> do
      l1 <- fromSexp sl1
      l2 <- fromSexp sl2
      pure $ Message l1 l2 ss
    _ -> Nothing

data EditOrStateId
  = EditId EditId
  | StateId StateId

derive instance genericEditOrStateId :: Generic EditOrStateId

instance showEditOrStateId :: Show EditOrStateId where
  show = gShow

instance fromSexpEditOrStateId :: FromSexp EditOrStateId where
  fromSexp = case _ of
    List [ Atom "Edit", sid ] -> do
      id <- fromSexp sid
      pure $ EditId id
    List [ Atom "State", sid ] -> do
      id <- fromSexp sid
      pure $ StateId id
    _ -> Nothing

data Feedback = Feedback
  { id       :: EditOrStateId
  , contents :: FeedbackContent
  , route    :: Int
  }

derive instance genericFeedback :: Generic Feedback

instance showFeedback :: Show Feedback where
  show = gShow

instance fromSexpFeedback :: FromSexp Feedback where
  fromSexp = case _ of
    List [ Atom "Feedback"
         , List [ List [ Atom "id",
                         sid
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
