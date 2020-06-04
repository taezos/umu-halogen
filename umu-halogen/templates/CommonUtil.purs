module Common.Util where

import Prelude
-- internal service
import Service.Route ( Route, routeCodec )
-- routing
import Routing.Duplex as RD
-- halogen
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< RD.print routeCodec
