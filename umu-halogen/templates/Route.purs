module Service.Route where

import Prelude hiding ( (/) )
-- Generic
import Data.Generic.Rep ( class Generic )
import Data.Generic.Rep.Show ( genericShow )
-- Routing
import Routing.Duplex ( root, RouteDuplex' )
import Routing.Duplex.Generic ( noArgs, sum )
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | About

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route
derive instance ordRoute :: Ord Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = root $ sum
  { "Home": noArgs
  , "About": "about" / noArgs
  }
