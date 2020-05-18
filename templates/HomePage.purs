module Page.Home where

import Prelude
import Data.Symbol ( SProxy(..) )
-- internal component
import Component.Title as Title
-- Halogen
import Halogen as H
import Halogen.HTML as HH

type Slot p = forall query. H.Slot query Void p

type ChildSlots =
  ( title :: Title.Slot Unit
  )

_home :: SProxy "home"
_home = SProxy

component :: forall q i o m. H.Component HH.HTML q i o m
component =
  H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
  }

render :: forall state action m. state -> H.ComponentHTML action ChildSlots m
render _ =
  HH.div_
  [ HH.h1_ [ HH.text "Home" ]
  , HH.slot Title._title unit Title.component {} absurd
  ]
