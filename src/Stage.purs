module Stage where

import Prelude
import CSS as CSS
import Ports.CodeMirror.TextMarkerOptions as TMO
import Data.Foldable (fold)
import Data.Generic (class Generic, gEq)
import Data.Maybe (Maybe(..))
import SerAPI.Types (StateId)

data Stage
  = ToProcess
  | Processing StateId
  | Processed  StateId

derive instance genericStage :: Generic Stage

instance eqStage :: Eq Stage where
  eq = gEq

stageColor :: Stage -> CSS.Color
stageColor = case _ of
  ToProcess    -> CSS.deepskyblue
  Processing _ -> CSS.gold
  Processed  _ -> CSS.palegreen

textMarkerOptions :: Stage -> Maybe TMO.TextMarkerOptions
textMarkerOptions stage =
  Just $ TMO.def { css      = Just $ fold [ "background-color: "
                                          , CSS.toHexString $ stageColor stage
                                          ]
                 , readOnly = Just true
                 }

stageStateId :: Stage -> Maybe StateId
stageStateId = case _ of
  ToProcess      -> Nothing
  Processing sid -> Just sid
  Processed  sid -> Just sid
