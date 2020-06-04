module Component.Title where

import Prelude
import Data.Symbol
-- Halogen
import Halogen as H
import Halogen.HTML as HH

type Slot p = forall query. H.Slot query Void p

_title :: SProxy "title"
_title = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state action m. state -> H.ComponentHTML action () m
render _ =
  HH.h2_ [ HH.text "Hello, World!" ]
