module SerAPI.Answer where

import Prelude
import Data.Generic (class Generic, gShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Ports.Sexp (Sexp(..))
import SerAPI.Command (CommandTag)
import SerAPI.FromSexp (class FromSexp, fromSexp)
import SerAPI.Types (CoqObject, Exn, Loc, StateId)

data Added
  = NewTip
  | Unfocus StateId

derive instance genericAdded :: Generic Added

instance showAdded :: Show Added where
  show = gShow

instance fromSexpAdded :: FromSexp Added where
  fromSexp = case _ of
    Atom "NewTip" -> Just NewTip
    List [ Atom "Unfocus", sid ] -> do
      id <- fromSexp sid
      pure $ Unfocus id
    _ -> Nothing

data AnswerKind
  = Ack
  | Completed
  | Added StateId Loc Added
  | Canceled (Array StateId)
  | ObjList (Array CoqObject)
  | CoqExn (Maybe Loc) (Maybe (Tuple StateId StateId)) Exn

derive instance genericAnswerKind :: Generic AnswerKind

instance showAnswer :: Show AnswerKind where
  show = gShow

instance fromSexpAnswerKind :: FromSexp AnswerKind where
  fromSexp = case _ of
    Atom "Ack" -> Just Ack
    Atom "Completed" -> Just Completed
    List [ Atom "StmAdded", sid, sloc, sadded ] -> do
      id    <- fromSexp sid
      loc   <- fromSexp sloc
      added <- fromSexp sadded
      pure $ Added id loc added
    List [ Atom "StmCanceled", sids ] -> do
      ids <- fromSexp sids
      pure $ Canceled ids
    List [ Atom "CoqExn", sm1, sm2, se ] -> do
      m1 <- fromSexp sm1
      m2 <- fromSexp sm2
      e  <- fromSexp se
      pure $ CoqExn m1 m2 e
    _ -> Nothing

data Answer = Answer CommandTag AnswerKind

derive instance genericAnswer :: Generic Answer

instance fromSexpAnswer :: FromSexp Answer where
  fromSexp = case _ of
    List [ Atom "Answer", sid, skind ] -> do
      id   <- fromSexp sid
      kind <- fromSexp skind
      pure $ Answer id kind
    _ -> Nothing

tagOf :: Answer -> CommandTag
tagOf (Answer tag _) = tag
