module Component.Router where

import Prelude
import Data.Maybe ( Maybe(..), fromMaybe )
import Data.Either ( hush )
-- internal common
import Common.Util as Util
-- internal service
import Service.Route ( routeCodec, Route(..) )
import Service.Navigate ( navigate, class Navigate )
-- internal component
import Page.Home as Home
import Page.About as About
-- effect
import Effect.Class ( class MonadEffect )
-- routing
import Routing.Duplex ( parse )
import Routing.Hash ( getHash )
-- Halogen
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

type State =
  { route :: Maybe Route
  }

type ChildSlots =
  ( home :: Home.Slot Unit
  , about :: About.Slot Unit
  )

data Action
  = Initialize
  | GoTo Route

data Query a = Navigate Route a

component
  :: forall i m
  . MonadEffect m
  => Navigate m
  => H.Component HH.HTML Query i Void m
component =
  H.mkComponent
  { initialState: const { route: Nothing }
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Action ChildSlots m
render { route } =  navbar $ pure $ case route of
  Nothing -> HH.h1_ [ HH.text "Oops! That page wasn't found" ]
  Just r -> case r of
    Home -> HH.slot Home._home unit Home.component {} absurd
    About -> HH.slot About._about unit About.component {} absurd

handleAction
  :: forall o m
  . MonadEffect m
  => Navigate m
  => Action
  -> H.HalogenM State Action ChildSlots o m Unit
handleAction = case _ of
  Initialize -> do
    initialRoute <- hush <<< ( parse routeCodec ) <$> H.liftEffect getHash
    navigate $ fromMaybe Home initialRoute
  GoTo route ->
    navigate route

handleQuery :: forall a m. Query a -> H.HalogenM State Action ChildSlots Void m ( Maybe a )
handleQuery = case _ of
  Navigate dest a -> do
    route <- H.gets _.route
    when ( route /= Just dest ) do
      H.modify_ _ { route = Just dest }
    pure ( Just a )

navbar :: forall a. Array ( HH.HTML a Action ) -> HH.HTML a Action
navbar html =
  HH.div_
  [ HH.ul_
    [ HH.li_
      [ HH.a
        [ Util.safeHref Home
        ]
        [ HH.text "Home" ]
      ]
    , HH.li_
      [ HH.a
        [ Util.safeHref About
        ]
        [ HH.text "About" ]
      ]
    ]
  , HH.div_ html
  ]
