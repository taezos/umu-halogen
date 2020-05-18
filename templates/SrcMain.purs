module Main where

import Prelude

-- Components
import Page.Home as Home
-- Effect
import Effect ( Effect )
-- Halogen
import Halogen.Aff as HA
import Halogen.VDom.Driver ( runUI )

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Home.component unit body
