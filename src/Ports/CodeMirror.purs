module Ports.CodeMirror where

import Prelude
import CodeMirror.Position (Position)
import Control.Monad.Eff (Eff)
import DOM.HTML.Types (HTMLElement)
import Data.Foreign (Foreign)
import Data.Function.Uncurried as FU
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Options (Options, options)

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

foreign import _codeMirror :: ∀ e. FU.Fn2 HTMLElement RawConfiguration (Eff e CodeMirror)
codeMirror :: ∀ e. HTMLElement -> Configuration -> Eff e CodeMirror
codeMirror e c = FU.runFn2 _codeMirror e (toRawConfiguration c)

foreign import _getDoc :: ∀ e. FU.Fn1 CodeMirror (Eff e Doc)
getDoc :: ∀ e. CodeMirror -> Eff e Doc
getDoc = FU.runFn1 _getDoc

foreign import _getValue :: ∀ e. FU.Fn2 Doc (Nullable String) (Eff e String)
getValue :: ∀ e. Doc -> Maybe String -> Eff e String
getValue d m = FU.runFn2 _getValue d (toNullable m)

foreign import _hasFocus :: ∀ e. FU.Fn1 CodeMirror (Eff e Boolean)
hasFocus :: ∀ e. CodeMirror -> Eff e Boolean
hasFocus = FU.runFn1 _hasFocus

foreign import _markText ::
  ∀ e. FU.Fn4 Doc Position Position (Nullable TextMarkerOptions) (Eff e TextMarker)
markText :: ∀ e. Doc -> Position -> Position -> Maybe TextMarkerOptions -> Eff e TextMarker
markText d p1 p2 m = FU.runFn4 _markText d p1 p2 (toNullable m)

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

foreign import _onCodeMirrorChange ::
  ∀ e. FU.Fn2 CodeMirror (CodeMirrorChange -> Eff e Unit) (Eff e Unit)

onCodeMirrorChange :: ∀ e. CodeMirror -> (CodeMirrorChange -> Eff e Unit) -> Eff e Unit
onCodeMirrorChange cm h = FU.runFn2 _onCodeMirrorChange cm h
