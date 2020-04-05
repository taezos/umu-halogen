{-# LANGUAGE TemplateHaskell #-}
module UmuHalogen.Templates where

import           ClassyPrelude
import           UmuHalogen.TH


packageJsonFile :: Text
packageJsonFile = $(embedFileUtf8 "templates/package.json")

srcMainFile :: Text
srcMainFile = $(embedFileUtf8 "templates/SrcMain.purs")

spagoDhallFile :: Text
spagoDhallFile = $(embedFileUtf8 "templates/spago.dhall")

packagesDhallFile :: Text
packagesDhallFile = $(embedFileUtf8 "templates/packages.dhall")

indexHtmlFile :: Text
indexHtmlFile = $(embedFileUtf8 "templates/index.html")

testMainFile :: Text
testMainFile = $(embedFileUtf8 "templates/TestMain.purs")

titleComponentFile :: Text
titleComponentFile = $(embedFileUtf8 "templates/TitleComponent.purs")
