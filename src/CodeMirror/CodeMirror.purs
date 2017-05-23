module CodeMirror.CodeMirror where

import Data.Map as Map

import CodeMirror.Position (Position)
import CodeMirror.TextMarker (TextMarker, TextMarkerId)

type CodeMirrorId = Int

type TextMarkerOptions = {}

type CodeMirror =
  { code           :: String
  , cursorPosition :: Position
  , id             :: CodeMirrorId
  , markers        :: Map.Map TextMarkerId TextMarker
  , nextMarkerId   :: TextMarkerId
  , tip            :: Position
  }
