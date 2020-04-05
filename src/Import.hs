{-# LANGUAGE OverloadedStrings #-}
module Import
  ( module X
  , appName
  ) where

import           ClassyPrelude as X

appName :: Text
appName = "umu-halogen"
