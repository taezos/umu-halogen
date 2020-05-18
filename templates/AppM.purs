module AppM where

import Prelude
-- internal service
import Service.Navigate
import Service.Route
-- Aff
import Effect.Aff ( Aff )
import Effect.Aff.Class ( class MonadAff )
-- Effect
import Effect.Class ( liftEffect, class MonadEffect )
-- Routing
import Routing.Hash ( setHash )
import Routing.Duplex ( print )

newtype AppM a = AppM ( Aff a )

runAppM :: AppM ~> Aff
runAppM ( AppM m ) = m

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applictiveAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance moandAffAppM :: MonadAff AppM

instance navigateAppM :: Navigate AppM where
  navigate = liftEffect <<< setHash <<< print routeCodec
