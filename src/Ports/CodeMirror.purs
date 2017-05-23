module Ports.CodeMirror where

import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Function.Uncurried
-- import Position (Position)
-- import TextMarker (TextMarker, TextMarkerId)

type Configuration =
  { autofocus  :: Boolean
  , lineNumbers :: Boolean
  , mode       :: String
  , value      :: String
  }

data CodeMirror
data Doc

foreign import _codeMirror :: forall e. Fn2 HTMLElement Configuration (Eff (| e) CodeMirror)
codeMirror :: ∀ e. HTMLElement -> Configuration -> Eff (| e) CodeMirror
codeMirror = runFn2 _codeMirror

foreign import _getDoc :: forall e. Fn1 CodeMirror (Eff (| e) Doc)
getDoc :: ∀ e. CodeMirror -> Eff (| e) Doc
getDoc = runFn1 _getDoc

foreign import _hasFocus :: forall e. Fn1 CodeMirror (Eff (| e) Boolean)
hasFocus :: ∀ e. CodeMirror -> Eff (| e) Boolean
hasFocus = runFn1 _hasFocus
