module PeaCoq.Component where

import Prelude
import CSS as CSS
import CSS.Display as CSSD
import CodeMirror.Component as CM
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import SerAPI.Component as SAPI
import CodeMirror.TextMarker (TextMarkerId)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Class (class MonadAff)
import Control.Monad.Aff.Console (CONSOLE)
import Data.Either.Nested (Either2)
import Data.Foldable (fold)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Lens (lens, over, view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Halogen.Component.ChildPath (ChildPath, cp1, cp2)
import Halogen.HTML.CSS (style)
import Network.HTTP.Affjax (AJAX)
import SerAPI.Answer (Answer(..), AnswerKind(..))
import SerAPI.Command (Command(Control), CommandTag, TaggedCommand, tagOf)
import SerAPI.Command.Control (Control(..), defaultAddOptions)
import SerAPI.Feedback (Feedback)
import SerAPI.Types (StateId)

-- this seems a little dumb, but looks like we have to...
data Query a
  = CodeMirrorSentence TextMarkerId String a
  | SAPIPing                               a
  | SAPIAnswer         Answer              a
  | SAPIFeedback       Feedback            a

type ChildQuery = Coproduct2 CM.Query SAPI.Query

type State =
  { tagToMarker :: Map.Map CommandTag TextMarkerId
  }

_tagToMarker :: Lens' State (Map.Map CommandTag TextMarkerId)
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

type Render m = H.ParentHTML Query ChildQuery Slot m

renderTagToMarker :: ∀ m. Tuple CommandTag TextMarkerId -> Render m
renderTagToMarker (Tuple commandTag markerId) =
  HH.div
  [ style do
       CSSD.float CSSD.floatLeft
  ]
  [ HH.text $ fold [ "(tag", show commandTag, " : marker", show markerId, ")" ]
  ]

render :: ∀ m e. MonadAff (PeaCoqEffects e) m => State -> Render m
render state =
  HH.div [ style $ do
              CSS.height    $ CSS.fromString "100%"
              CSS.minHeight $ CSS.fromString "100%"
         ]
  $
  -- [ HH.div_ $ renderTagToMarker <$> Map.toUnfoldable state.tagToMarker ]
  -- <>
  [ HH.slot' sapiSlot unit SAPI.serAPIComponent   unit                   handleSerAPI ]
  <>
  [ HH.slot' cmSlot   unit CM.codeMirrorComponent { code : initialCode } handleCodeMirror ]

stmAdd :: String -> SAPI.Query TaggedCommand
stmAdd s = H.request $ SAPI.TagCommand $ Control $ StmAdd { addOptions : defaultAddOptions
                                                          , sentence   : s
                                                          }

stmObserve :: StateId -> SAPI.Query TaggedCommand
stmObserve sid = H.request $ SAPI.TagCommand $ Control $ StmObserve { stateId : sid }

stmQuit :: SAPI.Query TaggedCommand
stmQuit = H.request $ SAPI.TagCommand $ Control $ Quit

ping :: SAPI.Query Unit
ping = H.action SAPI.Ping

eval :: ∀ e m. MonadAff (PeaCoqEffects e) m => Query ~> H.ParentDSL State Query ChildQuery Slot Message m
eval = case _ of

  CodeMirrorSentence markerId sentence next -> do
    -- IMPORTANT: make sure the state is modified to account for the addTag before calling stmAdd
    H.query' sapiSlot unit (stmAdd sentence) >>= traverse_ \ taggedCommand -> do
      -- first, save the mapping, so that when receiving the reply we know who it is
      H.modify $ over _tagToMarker $ Map.insert (tagOf taggedCommand) markerId
      H.query' sapiSlot unit (H.action $ SAPI.Send taggedCommand)
    pure next

  SAPIAnswer (Answer tag answer) next -> do
    -- H.liftEff $ log $ fold ["Received answer: ", show tag, ": ", show answer]
    markers <- H.gets (_.tagToMarker >>> Map.keys)
    -- H.liftEff $ log $ fold ["Current known markers: ", show markers]
    H.gets (view _tagToMarker >>> Map.lookup tag) >>= traverse_ \ markerId -> do
      _ <- H.query' cmSlot unit $ H.action $ CM.ProcessAnswer markerId answer
      -- TODO
      pure unit
    case answer of
      Added sid loc added -> do
        H.query' sapiSlot unit (stmObserve sid) >>= traverse_ \ taggedCommand -> do
          H.query' sapiSlot unit $ H.action $ SAPI.Send taggedCommand
      -- When an answer has completed, we can remove its tag from tagToMarker
      Completed -> do
        -- H.liftEff $ log $ "Answer " <> show tag <> " has completed"
        H.modify (over _tagToMarker $ Map.delete tag)
      _ -> pure unit
    pure next

  SAPIFeedback f next -> do
    _ <- H.query' cmSlot unit $ H.action $ CM.ProcessFeedback f
    pure next

  SAPIPing next -> do
    _ <- H.query' sapiSlot unit ping
    pure next

peaCoqComponent :: ∀ e m. MonadAff (PeaCoqEffects e) m => H.Component HH.HTML Query Input Message m
peaCoqComponent =
  H.parentComponent
    { initialState : const { tagToMarker : Map.empty }
    , render
    , eval
    , receiver     : const Nothing
    }

initialCode :: String
initialCode = """
Theorem test : forall a b, a + b = b + a.
Proof.
  intros.
  induction a.
  { idtac. }
Abort.
From Coq Require Import Bool.Bool.
From Coq Require Import ZArith.

(* The stuff below is for testing parsing *)

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
