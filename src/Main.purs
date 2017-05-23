module Main where

import Prelude
import Halogen.Aff as HA
import CodeMirror.Component (ui)
--import Control.Monad.Aff (Aff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
--import DOM.HTML.Types (HTMLElement)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Ports.CodeMirror (codeMirror, hasFocus)

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  mbody <- selectElement (QuerySelector "body")
  case mbody of
    Nothing -> log "body not found"
    Just body -> do
      cm <- runUI ui { code : initialCode, id : 42 } body
      editor <- liftEff $ codeMirror body { autofocus : true
                                          , mode : "text/x-ocaml"
                                          , value : initialCode
                                          }
      focused <- liftEff $ hasFocus editor
      log $ "Has focus? " <> show focused
--       cm.subscribe $ consumer $ case _ of
--         CodeMirrorComponent.AddedMarker id from to -> do
--           _ <- log $ "Added marker: " <> show id <> " from " <> show from <> " to " <> show to
--           pure Nothing
      pure unit

initialCode :: String
initialCode = """
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
