module PeaCoq.Component where

import Prelude
import CodeMirror.Component as CM
import Halogen as H
import Halogen.HTML as HH
import SerAPI.Component as SAPI
import CodeMirror.TextMarker (TextMarkerId)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens (lens, over)
import Data.Lens.Types (Lens')
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..))
import Halogen.Component.ChildPath (ChildPath, cp1, cp2)
import Network.HTTP.Affjax (AJAX)
import SerAPI.Answer (Answer)
import SerAPI.Command (Command(..), CommandTag)
import SerAPI.Command.Control (Control(..), defaultAddOptions)
import SerAPI.Feedback (Feedback)

-- this seems a little dumb, but looks like we have to...
data Query a
  = CodeMirrorSentence TextMarkerId String a
  | SAPIAnswer         Answer              a
  | SAPIFeedback       Feedback            a

type ChildQuery = Coproduct2 CM.Query SAPI.Query

type State =
  { tagToMarker :: Map CommandTag TextMarkerId
  }

_tagToMarker :: Lens' State (Map CommandTag TextMarkerId)
_tagToMarker = lens _.tagToMarker (_ { tagToMarker = _ })

type Slot = Either2 Unit Unit

cmSlot :: ChildPath CM.Query ChildQuery Unit Slot
cmSlot = cp1

sapiSlot :: ChildPath SAPI.Query ChildQuery Unit Slot
sapiSlot = cp2

type Input = Unit

type Message = Void

handleCodeMirror :: CM.Message -> Maybe (Query Unit)
handleCodeMirror = case _ of
  CM.Sentence markerId sentence -> Just $ H.action $ CodeMirrorSentence markerId sentence

handleSerAPI :: SAPI.Message -> Maybe (Query Unit)
handleSerAPI = case _ of
  SAPI.Answer   a -> Just $ H.action $ SAPIAnswer a
  SAPI.Feedback f -> Just $ H.action $ SAPIFeedback f

type PeaCoqEffects e =
  ( ajax    :: AJAX
  , avar    :: AVAR
  , console :: CONSOLE
  | e)

render :: ∀ m e. MonadAff (PeaCoqEffects e) m => State -> H.ParentHTML Query ChildQuery Slot m
render state =
  HH.div_
    [ HH.slot' sapiSlot unit SAPI.serAPIComponent   unit                   handleSerAPI
    , HH.slot' cmSlot   unit CM.codeMirrorComponent { code : initialCode } handleCodeMirror
    ]

stmAdd :: String -> SAPI.Query CommandTag
stmAdd s = H.request $ SAPI.Send $ Control $ StmAdd { addOptions : defaultAddOptions
                                                    , sentence   : s
                                                    }

stmQuit :: SAPI.Query CommandTag
stmQuit = H.request $ SAPI.Send $ Control $ Quit

ping :: SAPI.Query Unit
ping = H.action SAPI.Ping

eval :: ∀ m. Query ~> H.ParentDSL State Query ChildQuery Slot Message m
eval = case _ of
  CodeMirrorSentence markerId sentence next -> do
    mAddTag <- H.query' sapiSlot unit $ stmAdd sentence
    _ <- case mAddTag of
      Nothing -> pure unit
      Just addTag -> do
        H.modify (over _tagToMarker $ insert addTag markerId)
    pure next
  SAPIAnswer a next -> do
    -- TODO
    pure next
  SAPIFeedback f next -> do
    -- TODO
    pure next

peaCoqComponent :: ∀ e m. MonadAff (PeaCoqEffects e) m => H.Component HH.HTML Query Input Message m
peaCoqComponent =
  H.parentComponent
    { initialState : const { tagToMarker : empty }
    , render
    , eval
    , receiver     : const Nothing
    }

initialCode :: String
initialCode = """
Print Set.
Print Prop.
From Coq Require Import Bool.Bool.
From Coq Require Import ZArith.
(*
. .. .a a. .( (. ). .) {. .{ .} }.
(* . .. .a a. .( (. ). .) {. .{ .} }. *)
" *) "
. .. .a a. .( (. ). .) {. .{ .} }.
*)
Set Implicit Arguments.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).
Notation "{ x ; .. ; y }" := (cons x .. (cons y nil) ..).
CoInductive stream A := { hd :: A; tl : stream A }.
Definition expand {A} (s :: stream A) := (Build_stream s.(hd) s.(tl)).
Theorem test :: [1; 2] = {1; 2}.
Proof using.
  {-+ pose proof ( 1 + ( - 2 ) )%Z.
      simpl in * .
      simpl in *...
               * reflexivity...
  }
Qed.
"""
