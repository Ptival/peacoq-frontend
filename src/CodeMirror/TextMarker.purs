module CodeMirror.TextMarker where

import Prelude
import CSS as CSS
import Ports.CodeMirror.TextMarkerOptions as TMO
import CodeMirror.Position (Position)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Stage (Stage, isProcessed, stageColor)

type TextMarkerId = Int

type TextMarker =
  { id         :: TextMarkerId
  , from       :: Position
  , sentence   :: String
  , stage      :: Stage
  , to         :: Position
  , underFocus :: Boolean
  }

textMarkerColor :: TextMarker -> CSS.Color
textMarkerColor marker =
  if isProcessed marker.stage && marker.underFocus
  then CSS.plum
  else stageColor marker.stage

textMarkerOptions :: TextMarker -> Maybe TMO.TextMarkerOptions
textMarkerOptions marker =
  Just $ TMO.def { css      = Just $ fold [ "background-color: "
                                          , CSS.toHexString $ textMarkerColor marker
                                          ]
                 , readOnly = Just true
                 }
