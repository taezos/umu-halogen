module Main where

import Prelude
import Data.Maybe ( Maybe(..) )
-- internal
import AppM ( runAppM )
-- internal service
import Service.Route as Route
-- internal components
import Component.Router as Router
-- effect
import Effect ( Effect )
import Effect.Class ( liftEffect )
-- aff
import Effect.Aff ( Aff, launchAff_ )
-- routing
import Routing.Hash ( matchesWith )
import Routing.Duplex ( parse )
-- halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver ( runUI )

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  let
    rootComponent :: H.Component HH.HTML Router.Query {} Void Aff
    rootComponent = H.hoist runAppM Router.component
  halogenIO <- runUI rootComponent {} body

  void $ liftEffect $ matchesWith ( parse Route.routeCodec ) \mOld new ->
    when ( mOld /= Just new ) do
      launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new
  pure unit
