module Component.Title where

import Prelude

-- Halogen
import Halogen as H
import Halogen.HTML as HH

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state action m. state -> H.ComponentHTML action () m
render _ =
  HH.h1_ [ HH.text "Hello, World!" ]
