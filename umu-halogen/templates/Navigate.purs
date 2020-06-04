module Service.Navigate where

import Prelude
import Control.Monad.Trans.Class ( lift )
-- internal service
import Service.Route ( Route )
-- halogen
import Halogen ( HalogenM )

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate ( HalogenM state action slogs msg m ) where
  navigate = lift <<< navigate
