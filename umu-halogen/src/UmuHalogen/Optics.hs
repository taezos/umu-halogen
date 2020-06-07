{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module UmuHalogen.Optics where

import           Control.Lens.TH
-- purescript
import           Language.PureScript.CST.Types
-- umu-halogen
import           UmuHalogen.Log
import           UmuHalogen.Optics.Util
import           UmuHalogen.Types

makeLenses ''Log
makeLenses ''LogMessage
makeLenses ''WriteDirReq
makeLenses ''WriteFileReq

makePrisms ''Declaration
makePrisms ''Guarded
makePrisms ''Expr

mkCustomLenses ''ValueBindingFields
mkCustomLenses ''Name
mkCustomLenses ''Ident
mkCustomLenses ''Labeled
mkCustomLenses ''Where
mkCustomLenses ''Wrapped
mkCustomLenses ''Separated
mkCustomLenses ''Module
