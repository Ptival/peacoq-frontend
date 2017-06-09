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
import Control.Monad.Eff.Console (log)
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
import SerAPI.Command (Command(..), CommandTag, TaggedCommand, tagOf)
import SerAPI.Command.Control (Control(..), defaultAddOptions)
import SerAPI.Feedback (Feedback(..), FeedbackContent(..))
import SerAPI.Types (StateId, RouteId)

peaCoqGetContextRouteId :: RouteId
peaCoqGetContextRouteId = 2

-- this seems a little dumb, but looks like we have to...
data Query a
  = CodeMirrorCancel   StateId             a
  | CodeMirrorObserve  StateId             a
  | CodeMirrorSentence TextMarkerId String a
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
  CM.Cancel   stateId           -> Just $ H.action $ CodeMirrorCancel   stateId
  CM.Observe  stateId           -> Just $ H.action $ CodeMirrorObserve  stateId
  CM.Sentence markerId sentence -> Just $ H.action $ CodeMirrorSentence markerId sentence

handleSerAPI :: SAPI.Message -> Maybe (Query Unit)
handleSerAPI = case _ of
  SAPI.MessageAnswer   a -> Just $ H.action $ SAPIAnswer a
  SAPI.MessageFeedback f -> Just $ H.action $ SAPIFeedback f

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
stmAdd sentence =
  H.request $ SAPI.TagCommand $ Control
  $ StmAdd { addOptions : defaultAddOptions
           , sentence
           }

stmCancel :: Array StateId -> SAPI.Query TaggedCommand
stmCancel stateIds =
  H.request $ SAPI.TagCommand $ Control
  $ StmCancel { stateIds }

stmObserve :: StateId -> SAPI.Query TaggedCommand
stmObserve stateId =
  H.request $ SAPI.TagCommand $ Control
  $ StmObserve { stateId }

stmQuery :: String -> RouteId -> SAPI.Query TaggedCommand
stmQuery query route =
  H.request $ SAPI.TagCommand $ Control
  $ StmQuery { queryOptions : { route : Just route }
             , query
             }

stmQuit :: SAPI.Query TaggedCommand
stmQuit = H.request $ SAPI.TagCommand $ Control $ Quit

ping :: SAPI.Query Unit
ping = H.action SAPI.Ping

tagAndSend ::
  ∀ e m. MonadAff (PeaCoqEffects e) m => SAPI.Query TaggedCommand -> H.ParentDSL State Query ChildQuery Slot Message m Unit
tagAndSend cmd =
  H.query' sapiSlot unit cmd >>= traverse_ \ taggedCommand -> do
    H.query' sapiSlot unit (H.action $ SAPI.Send taggedCommand)

eval :: ∀ e m. MonadAff (PeaCoqEffects e) m => Query ~> H.ParentDSL State Query ChildQuery Slot Message m
eval = case _ of

  CodeMirrorCancel stateId next -> do
    tagAndSend $ stmCancel [stateId]
    pure next

  CodeMirrorObserve stateId next -> do
    tagAndSend $ stmObserve stateId
    -- tagAndSend $ stmQuery "PeaCoqGetContext." peaCoqGetContextRouteId
    pure next

  CodeMirrorSentence markerId sentence next -> do
    -- IMPORTANT: make sure the state is modified to account for the addTag before calling stmAdd
    -- DO NOT `tagAndSend`
    H.query' sapiSlot unit (stmAdd sentence) >>= traverse_ \ taggedCommand -> do
      -- first, save the mapping, so that when receiving the reply we know who it is
      H.modify $ over _tagToMarker $ Map.insert (tagOf taggedCommand) markerId
      H.query' sapiSlot unit (H.action $ SAPI.Send taggedCommand)
    pure next

  SAPIAnswer (Answer tag answer) next -> do
    -- H.liftEff $ log $ fold ["Received answer: ", show tag, ": ", show answer]
    -- markers <- H.gets (_.tagToMarker >>> Map.keys)
    -- H.liftEff $ log $ fold ["Current known markers: ", show markers]
    maybeMarkerId <- H.gets (view _tagToMarker >>> Map.lookup tag)
    _ <- H.query' cmSlot unit $ H.action $ CM.ProcessAnswer answer maybeMarkerId
    case answer of
      -- When an answer has completed, we can remove its tag from tagToMarker
      Completed -> do
        -- H.liftEff $ log $ "Answer " <> show tag <> " has completed"
        H.modify (over _tagToMarker $ Map.delete tag)
      _ -> pure unit
    pure next

  SAPIFeedback feedback@(Feedback f) next -> do
    _ <- H.query' cmSlot unit $ H.action $ CM.ProcessFeedback feedback
    case f.contents of
      Processed -> do
        H.liftEff $ log $ "Processed: " <> show f.id
        pure unit
      _ -> pure unit
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
Theorem t : forall n, n = 0 \/ n <> 0.
Proof. intros. destruct n. left. reflexivity. right. discriminate. Qed.

Require Import Coq.Lists.List.
Import ListNotations.
(* Heterogeneous lists *)
(***********************)
Section hlist.
(* Heterogeneous lists are lists where the elements of the
 * list can have different types. In Coq, we can define
 * heterogeneous lists by indexing an inductive type by
 * a list of types, one for each element. For convenience
 * in the future however, we will index our type by an
 * a type [iT] and a function (F) that computes a type from
 * [iT]. This will make our definition a little bit easier
 * to use in the next exercise.
 *)
Variable iT : Type.
Variable F : iT -> Type.

Inductive hlist : list iT -> Type :=
| Hnil : hlist nil
| Hcons : forall {T Ts}, F T -> hlist Ts -> hlist (T :: Ts).

(* Exercise: Implement the head and tail functions for hlists.
 *)
Definition hlist_hd {T Ts} (h : hlist (T :: Ts)) : F T :=
  match h with
  | Hcons x _ => x
  | Hnil => tt
  end.

Definition hlist_tl {T Ts} (h : hlist (T :: Ts)) : hlist Ts :=
  match h with
  | Hcons _ xs => xs
  | Hnil => tt
  end.


(* An alternative solution is to build an inductive
 * type that carries the information about the value that
 * we find at a particular location in the list.
 *)
Inductive mem {T : Type} (t : T) : list T -> Type :=
| MO : forall {ts}, mem t (t :: ts)
| MS : forall {t' ts}, mem t ts -> mem t (t' :: ts).

Arguments MO {_ _ _}.
Arguments MS {_ _ _ _} _.

(* Exercise: Implement hlist_nth with the following type.
 *)
Fixpoint hlist_nth {T Ts} (h : hlist Ts) (n : mem T Ts) : F T :=
  match n in mem _ Ts return hlist Ts -> F T with
  | MO => hlist_hd
  | MS m => fun h => hlist_nth (hlist_tl h) m
  end h.

End hlist.

Arguments hlist {_} _ _.
Arguments hlist_hd {_ _ _ _} _.
Arguments hlist_tl {_ _ _ _} _.
Arguments hlist_nth {_ _ _ _} _ _.
Arguments Hnil {_ _}.
Arguments Hcons {_ _ _ _} _ _.

Arguments MO {_ _ _}.
Arguments MS {_ _ _ _} _.

Section mem_dec.
  Variable T : Type.
  (* In order to implement decidable equality for [mem], we
   * need decidable equality for the type T. The reason for
   * this is a little bit unintuitive but it turns out to be
   * true.
   *)
  Variable T_dec : forall a b : T, a = b \/ a <> b.

  Lemma MS_inj : forall t t' ts (m m' : mem t ts),
      @MS T t t' ts m = MS m' ->
      m = m'.
  Proof.
    intros.
    refine
      match H in _ = M
            return match M in mem _ ts return mem _ (tl ts) -> Prop with
                   | MO => fun _ => True
                   | MS m'' => fun m => m = m''
                   end m
      with
      | eq_refl => eq_refl
      end.
  Defined.

  (* A theorem in Coq tells us that decidable equality on
   * a type [T] implies proof irrelevence for proofs of equality
   * of values of type [T].
   *)
  Require Import Coq.Logic.Eqdep_dec.

  Fixpoint mem_dec {ts : list T} {t : T} (a : mem t ts)
  : forall b : mem t ts, {a = b} + {a <> b}.
  refine
    match a as a in mem _ ts
          return forall b : mem t ts, {a = b} + {a <> b}
    with
    | MO => fun b =>
      match b as b in mem _ ts
            return match ts as ts return mem t ts -> Type with
                   | nil => fun _ => unit
                   | t' :: ts => fun b =>
                                   forall pf : t' = t,
                                     {MO = match pf with
                                           | eq_refl => b
                                           end} +
                                     {MO <> match pf with
                                            | eq_refl => b
                                            end}
                   end b
      with
      | MO => fun _ => left _
      | MS m'' => fun _ => right _
      end eq_refl
    | @MS _ _ t' ts' m' => fun (b : mem t (t' :: ts')) =>
      match b as b in mem _ ts
            return forall m' : mem t (tl ts),
          (forall b, {m' = b} + {m' <> b}) ->
          match ts as ts return mem t ts -> mem t (tl ts) -> Type with
          | nil => fun _ _ => unit
          | t'' :: ts'' => fun (b : mem t (t'' :: ts'')) m' =>
                             {MS m' = b} +
                             {MS m' <> b}
          end b m'
      with
      | MO => fun _ _ => right _
      | MS m'' => fun _ rec =>
                        match rec m'' with
                        | left _ => left _
                        | right _ => right _
                        end
      end m' (fun b => mem_dec _ _ m' b)
    end.
  Proof.
  { eapply K_dec with (p := e); auto. }
  { subst. intro. inversion H. }
  { clear. red; intro. inversion H. }
  { f_equal. assumption. }
  { clear - n. intro. apply n.
    eapply MS_inj in H. assumption. }
  Defined.
End mem_dec.

Arguments mem_dec {_} _ {_ _} _ _.

(* A simple Lambda-calculus *)
(****************************)

(* We can use dependent types to describe "well-typed" terms.
 * We start with a type of types.
 *)
Inductive type : Type :=
| Nat
| Bool
| Arr (d c : type)
.

(* I've started a simple definition with just constants, addition
 * and variables.
 *)
Inductive expr (ts : list type) : type -> Type :=
| ConstNat  : nat -> expr ts Nat
| Plus      : expr ts Nat -> expr ts Nat -> expr ts Nat
| ConstBool : bool -> expr ts Bool
| If        : forall t, expr ts Bool -> expr ts t -> expr ts t -> expr ts t
| App       : forall d c, expr ts (Arr d c) -> expr ts d -> expr ts c
| Abs       : forall d c, expr (d :: ts) c -> expr ts (Arr d c)
| Var       : forall {t}, mem t ts -> expr ts t
| PlusE     : expr ts (Arr Nat (Arr Nat Nat)).

Arguments ConstNat {_} _.
Arguments Plus {_} _ _.
Arguments ConstBool {_} _.
Arguments If {_ _} _ _ _.
Arguments App {_ _ _} _ _.
Arguments Abs {_ _ _} _.
Arguments Var {_ _} _.
Arguments PlusE {_}.

(* We can now write functions that map the syntactic definitions
 * into semantic ones.
 *)
Fixpoint typeD (t : type) : Type :=
  match t with
  | Nat => nat
  | Bool => bool
  | Arr l r => typeD l -> typeD r
  end.

Fixpoint exprD {ts : list type} {t : type} (e : expr ts t)
: hlist typeD ts -> typeD t :=
  match e in expr _ t return hlist typeD ts -> typeD t with
  | ConstNat n => fun _ => n
  | Plus a b => let aD := exprD a in let bD := exprD b in
                fun env => aD env + bD env
  | ConstBool b => fun _ => b
  | If t tr fa => let tD := exprD t in
                    let trD := exprD tr in
                    let faD := exprD fa in
                    fun env =>
                      if tD env then trD env else faD env
  | App f x => let fD := exprD f in
                   let xD := exprD x in
                   fun env => (fD env) (xD env)
  | Abs body => let bodyD := exprD body in
                    fun env => (fun x => bodyD (Hcons x env))
  | Var v => fun e => hlist_nth e v
  | PlusE => fun e => plus
  end.

Eval simpl in exprD (ConstNat 3) Hnil.
Eval simpl in exprD (Plus (ConstNat 3) (ConstNat 6)) Hnil.
Eval simpl in fun x : typeD Nat =>
                  exprD (Plus (Var MO) (ConstNat 6)) (Hcons x Hnil).





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
