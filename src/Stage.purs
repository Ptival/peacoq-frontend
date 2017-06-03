module Stage where

import CSS as CSS

data Stage
  = ToProcess
  | Processing
  | Processed

stageColor :: Stage -> CSS.Color
stageColor = case _ of
  ToProcess  -> CSS.deepskyblue
  Processing -> CSS.gold
  Processed  -> CSS.palegreen
