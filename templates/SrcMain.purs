module Main where

import Prelude

-- Components
import Component.Title as Title
-- Effect
import Effect ( Effect )
-- Halogen
import Halogen.Aff as HA
import Halogen.VDom.Driver ( runUI )

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Title.component unit body
