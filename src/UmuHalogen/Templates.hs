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

makeFile :: Text
makeFile = $(embedFileUtf8 "templates/Makefile")

indexJS :: Text
indexJS = $(embedFileUtf8 "templates/index.js")

componentTemplate :: Text -> Text
componentTemplate componentName = unlines
  [ "module " <> componentName <> " where"
  , mempty
  , "import Prelude"
  , "-- halogen"
  , "import Halogen as H"
  , "import Halogen.HTML as HH"
  , mempty
  , "component :: forall q i o m. H.Component HH.HTML q i o m"
  , "component = "
  , "  H.mkComponent"
  , "  { intialState: identity"
  , "  , render: const $ HH.h1 [ HH.text \"" <> componentName <> " Component\" ]"
  , "  , eval: H.mkEval H.defaultEval"
  , "  }"
  ]
