module Page.About where

import Prelude
import Data.Symbol (SProxy(..))
-- halogen
import Halogen as H
import Halogen.HTML as HH

type Slot p = forall query. H.Slot query Void p

_about :: SProxy "about"
_about = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state action m. state -> H.ComponentHTML action () m
render _ =
  HH.div_
  [ HH.h1_
    [ HH.text "About"
    ]
  ]
