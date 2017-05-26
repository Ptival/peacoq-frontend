module Ports.CodeMirror where

import Prelude
import CodeMirror.Position (Position)
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)

type Configuration =
  { autofocus  :: Boolean
  , lineNumbers :: Boolean
  , mode       :: String
  , value      :: String
  }

type TextMarkerOptions =
  { className :: String
  }

data CodeMirror
data Doc
data TextMarker

foreign import _codeMirror :: ∀ e. Fn2 HTMLElement Configuration (Eff e CodeMirror)
codeMirror :: ∀ e. HTMLElement -> Configuration -> Eff e CodeMirror
codeMirror = runFn2 _codeMirror

foreign import _getDoc :: forall e. Fn1 CodeMirror (Eff e Doc)
getDoc :: ∀ e. CodeMirror -> Eff e Doc
getDoc = runFn1 _getDoc

foreign import _getValue :: forall e. Fn2 Doc (Nullable String) (Eff e String)
getValue :: ∀ e. Doc -> Maybe String -> Eff e String
getValue d m = runFn2 _getValue d (toNullable m)

foreign import _hasFocus :: forall e. Fn1 CodeMirror (Eff e Boolean)
hasFocus :: ∀ e. CodeMirror -> Eff e Boolean
hasFocus = runFn1 _hasFocus

foreign import _markText ::
  ∀ e. Fn4 Doc Position Position (Nullable TextMarkerOptions) (Eff e TextMarker)
markText :: ∀ e. Doc -> Position -> Position -> Maybe TextMarkerOptions -> Eff e TextMarker
markText d p1 p2 m = runFn4 _markText d p1 p2 (toNullable m)

foreign import _onCodeMirror :: ∀ event e. Fn3 String CodeMirror (event -> Eff e Unit) (Eff e Unit)
foreign import _onDoc        :: ∀ event e. Fn3 String Doc        (event -> Eff e Unit) (Eff e Unit)

onCodeMirrorChange :: ∀ e. CodeMirror -> Eff e Unit -> Eff e Unit
onCodeMirrorChange cm h = runFn3 _onCodeMirror "change" cm (const h)

onDocChange :: ∀ e. Doc -> Eff e Unit -> Eff e Unit
onDocChange d h = runFn3 _onDoc "change" d (const h)
