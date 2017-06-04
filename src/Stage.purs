module Stage where

import CSS as CSS
import SerAPI.Types (StateId)

data Stage
  = ToProcess
  | Processing StateId
  | Processed  StateId

stageColor :: Stage -> CSS.Color
stageColor = case _ of
  ToProcess    -> CSS.deepskyblue
  Processing _ -> CSS.gold
  Processed  _ -> CSS.palegreen
