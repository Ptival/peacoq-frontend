module CodeMirror.TextMarker where

import CodeMirror.Position (Position)
import Stage (Stage)

type TextMarkerId = Int

type TextMarker =
  { id    :: TextMarkerId
  , from  :: Position
  , stage :: Stage
  , to    :: Position
  }
