module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import PeaCoq.Component as PeaCoq
import Control.Monad.Aff (delay)
import Control.Monad.Eff (Eff)
import Control.Monad.Rec.Class (forever)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse_)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)

main :: ∀ e. Eff (HA.HalogenEffects (PeaCoq.PeaCoqEffects e)) Unit
main = HA.runHalogenAff do
  selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    peaCoq <- runUI PeaCoq.peaCoqComponent unit body
    _ <- forever $ do
      peaCoq.query $ H.action PeaCoq.SAPIPing
      delay (Milliseconds 250.0)
    pure unit
