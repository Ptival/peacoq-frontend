module Main where

import Prelude
import Halogen.Aff as HA
import PeaCoq.Component as PeaCoq
import Control.Monad.Eff (Eff)
import DOM.Node.ParentNode (QuerySelector(..))
import Data.Traversable (traverse_)
import Halogen.Aff.Util (selectElement)
import Halogen.VDom.Driver (runUI)

main :: ∀ e. Eff (HA.HalogenEffects (PeaCoq.PeaCoqEffects e)) Unit
main = HA.runHalogenAff do
  selectElement (QuerySelector "body") >>= traverse_ \ body -> do
    peaCoq <- runUI PeaCoq.peaCoqComponent unit body
    pure unit
