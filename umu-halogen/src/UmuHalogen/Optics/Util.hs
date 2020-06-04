module UmuHalogen.Optics.Util where

import           Import
import           Language.Haskell.TH
-- lens
import           Control.Lens.Operators
import           Control.Lens.TH

mkCustomLenses :: Language.Haskell.TH.Name -> DecsQ
mkCustomLenses = makeLensesWith
  $ lensRules
  & lensField
  .~ (\_ _ name -> [TopName ( mkName $ nameBase name ++ "L" )])
