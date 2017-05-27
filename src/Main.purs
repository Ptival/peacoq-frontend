module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import CodeMirror.Component (codeMirrorComponent)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Traversable (traverse_)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import Ports.Sexp (sexp)
import SerAPI.Command (Command(..))
import SerAPI.Command.Control (Control(..), defaultAddOptions)
import SerAPI.Component (Query(..), serAPIComponent)

type AppEffects =
  ( ajax    :: AJAX
  , console :: CONSOLE
  )

stmAdd :: String -> Query Unit
stmAdd s = H.action $ Send $ Control $ StmAdd { addOptions : defaultAddOptions
                                              , sentence   : s
                                              }

stmQuit :: Query Unit
stmQuit = H.action $ Send $ Control $ Quit

ping :: Query Unit
ping = H.action Ping

main :: Eff (HA.HalogenEffects AppEffects) Unit
main = HA.runHalogenAff do
  log "\n\n\n"
  log $ show $ sexp "(Feedback((id(State 1))(contents(ProcessingIn master))(route 0)))"
  log "\n\n\n"
  selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    sapi <- runUI serAPIComponent     { }                    body
    cm   <- runUI codeMirrorComponent { code : initialCode } body
    sapi.query $ ping
    sapi.query $ stmQuit
    sapi.query $ ping
    sapi.query $ stmAdd "From Coq Require Import String."
    sapi.query $ ping
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
