module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import CodeMirror.Component as CM
import SerAPI.Component as SAPI
import Control.Coroutine (Consumer, consumer)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)
import Network.HTTP.Affjax (AJAX)
import SerAPI.Command (Command(..))
import SerAPI.Command.Control (Control(..), defaultAddOptions)

type AppEffects =
  ( ajax    :: AJAX
  , console :: CONSOLE
  )

stmAdd :: String -> SAPI.Query Unit
stmAdd s = H.action $ SAPI.Send $ Control $ StmAdd { addOptions : defaultAddOptions
                                                   , sentence   : s
                                                   }

stmQuit :: SAPI.Query Unit
stmQuit = H.action $ SAPI.Send $ Control $ Quit

ping :: SAPI.Query Unit
ping = H.action SAPI.Ping

consumeCM :: forall m o. Monad m => H.HalogenIO SAPI.Query o m -> Consumer CM.Message m Unit
consumeCM sapi = consumer $ case _ of
  CM.Sentence id sentence -> do
    sapi.query $ stmAdd sentence
    pure Nothing

consumeSAPI :: forall m o. Monad m => H.HalogenIO CM.Query o m -> Consumer SAPI.Message m Unit
consumeSAPI cm = consumer $ case _ of
  SAPI.Answer a -> do
    -- TODO
    pure Nothing
  SAPI.Feedback f -> do
    -- TODO
    pure Nothing

main :: Eff (HA.HalogenEffects AppEffects) Unit
main = HA.runHalogenAff do
  selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    sapi <- runUI SAPI.serAPIComponent   { }                    body
    cm   <- runUI CM.codeMirrorComponent { code : initialCode } body

    cm.subscribe   $ consumeCM   sapi
    sapi.subscribe $ consumeSAPI cm

    sapi.query $ ping
    sapi.query $ stmQuit
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
