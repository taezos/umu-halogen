{-# LANGUAGE OverloadedStrings #-}
module Import
  ( module X
  , appName
  ) where

import           Relude as X

appName :: Text
appName = "umu-halogen"
