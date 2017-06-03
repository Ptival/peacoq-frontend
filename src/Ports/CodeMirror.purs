module Ports.CodeMirror where

import Prelude
import Control.Monad.Eff.Uncurried as EU
import Ports.CodeMirror.Configuration as CFG
import Ports.CodeMirror.TextMarkerOptions as TMO
import CodeMirror.Position (Position)
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Foreign (Foreign)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Data.Options (opt, options, (:=))

type KeyMap = {}

data CodeMirror
data Doc
data TextMarker

foreign import _addKeyMap :: ∀ e. EU.EffFn3 e CodeMirror Foreign Boolean Unit
addKeyMap :: ∀ e. CodeMirror -> String -> Boolean -> (Unit -> Eff e Unit) -> Eff e Unit
addKeyMap cm key b k = EU.runEffFn3 _addKeyMap cm (options (opt key := EU.mkEffFn1 k)) b

foreign import _codeMirror :: ∀ e. EU.EffFn2 e HTMLElement CFG.RawConfiguration CodeMirror
codeMirror :: ∀ e. HTMLElement -> CFG.Configuration -> Eff e CodeMirror
codeMirror e c = EU.runEffFn2 _codeMirror e (CFG.toRaw c)

foreign import _getDoc :: ∀ e. EU.EffFn1 e CodeMirror Doc
getDoc :: ∀ e. CodeMirror -> Eff e Doc
getDoc = EU.runEffFn1 _getDoc

foreign import _getValue :: ∀ e. EU.EffFn2 e Doc (Nullable String) String
getValue :: ∀ e. Doc -> Maybe String -> Eff e String
getValue d m = EU.runEffFn2 _getValue d (toNullable m)

foreign import _hasFocus :: ∀ e. EU.EffFn1 e CodeMirror Boolean
hasFocus :: ∀ e. CodeMirror -> Eff e Boolean
hasFocus = EU.runEffFn1 _hasFocus

foreign import _markText ::
  ∀ e. EU.EffFn4 e Doc Position Position (Nullable TMO.RawTextMarkerOptions) TextMarker
markText :: ∀ e. Doc -> Position -> Position -> Maybe TMO.TextMarkerOptions -> Eff e TextMarker
markText d p1 p2 m = EU.runEffFn4 _markText d p1 p2 (toNullable $ TMO.toRaw <$> m)

type ChangeObj =
  { from    :: Position
  , to      :: Position
  , text    :: Array String
  , removed :: String
  , origin  :: String
  }

type CodeMirrorChange =
  { instance  :: CodeMirror
  , changeObj :: ChangeObj
  }

foreign import _onCodeMirrorChange :: ∀ e. EU.EffFn2 e CodeMirror (CodeMirrorChange -> Eff e Unit) Unit
onCodeMirrorChange :: ∀ e. CodeMirror -> (CodeMirrorChange -> Eff e Unit) -> Eff e Unit
onCodeMirrorChange cm h = EU.runEffFn2 _onCodeMirrorChange cm h
