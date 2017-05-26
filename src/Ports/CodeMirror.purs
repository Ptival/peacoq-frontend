module Ports.CodeMirror where

import Prelude
import CodeMirror.Position (Position)
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Foreign (Foreign)
import Data.Function.Uncurried (Fn1, Fn2, Fn3, Fn4, runFn1, runFn2, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Options (Options, opt, options, (:=))

type KeyMap = {}

type RawConfiguration =
  { autofocus     :: Nullable Boolean
  , extraKeys     :: Nullable Foreign
  , lineNumbers   :: Nullable Boolean
  , lineSeparator :: Nullable String
  , mode          :: Nullable String
  , value         :: Nullable String
  }

type Configuration =
  { autofocus     :: Maybe Boolean
  , extraKeys     :: Maybe (Options KeyMap)
  , lineNumbers   :: Maybe Boolean
  , lineSeparator :: Maybe String
  , mode          :: Maybe String
  , value         :: Maybe String
  }

toRawConfiguration :: Configuration -> RawConfiguration
toRawConfiguration c =
  { autofocus     : toNullable c.autofocus
  , extraKeys     : toNullable (options <$> c.extraKeys)
  , lineNumbers   : toNullable c.lineNumbers
  , lineSeparator : toNullable c.lineSeparator
  , mode          : toNullable c.mode
  , value         : toNullable c.value
  }

defaultConfiguration :: Configuration
defaultConfiguration =
  { autofocus     : Nothing
  , extraKeys     : Nothing
  , lineNumbers   : Nothing
  , lineSeparator : Nothing
  , mode          : Nothing
  , value         : Nothing
  }

type TextMarkerOptions =
  { className :: String
  }

data CodeMirror
data Doc
data TextMarker

foreign import _codeMirror :: ∀ e. Fn2 HTMLElement RawConfiguration (Eff e CodeMirror)
codeMirror :: ∀ e. HTMLElement -> Configuration -> Eff e CodeMirror
codeMirror e c = runFn2 _codeMirror e (toRawConfiguration c)

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
