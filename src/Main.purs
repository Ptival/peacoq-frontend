module Main where

import Prelude
import Halogen.Aff as HA
import CodeMirror.Component (codeMirrorComponent)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Traversable (traverse_)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)

main :: Eff (HA.HalogenEffects (console :: CONSOLE)) Unit
main = HA.runHalogenAff do
  selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    runUI codeMirrorComponent { code : initialCode } body

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
