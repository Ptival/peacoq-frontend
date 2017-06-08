module Stage where

import Prelude
import CSS as CSS
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

isProcessed :: Stage -> Boolean
isProcessed (Processed _) = true
isProcessed _             = false

stageStateId :: Stage -> Maybe StateId
stageStateId = case _ of
  ToProcess      -> Nothing
  Processing sid -> Just sid
  Processed  sid -> Just sid
