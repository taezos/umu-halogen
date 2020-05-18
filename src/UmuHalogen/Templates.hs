{-# LANGUAGE TemplateHaskell #-}
module UmuHalogen.Templates where

import           Import
-- text
import qualified Data.Text       as T
import           Text.Casing     (camel)
-- umu-halogen
import           UmuHalogen.TH
import           UmuHalogen.Util (toPascalCase)

--------------------------------------------------------------------------------
-- | Files
-- These are plain files that need no further input from the user.
--------------------------------------------------------------------------------
packageJsonFile :: Text
packageJsonFile = $(embedFileUtf8 "templates/package.json")

srcMainFile :: Text
srcMainFile = $(embedFileUtf8 "templates/SrcMain.purs")

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

appMfile :: Text
appMfile = $(embedFileUtf8 "templates/AppM.purs")

routerComponentFile :: Text
routerComponentFile = $(embedFileUtf8 "templates/Router.purs")

routeFile :: Text
routeFile = $(embedFileUtf8 "templates/Route.purs")

homePageFile :: Text
homePageFile = $(embedFileUtf8 "templates/HomePage.purs")

navigateFile :: Text
navigateFile = $(embedFileUtf8 "templates/Navigate.purs")

aboutFile :: Text
aboutFile = $(embedFileUtf8 "templates/About.purs")

utilFile :: Text
utilFile = $(embedFileUtf8 "templates/CommonUtil.purs")

--------------------------------------------------------------------------------
-- | Templates
-- templates are the files that need to be embedded with user input in this
-- context
--------------------------------------------------------------------------------
componentTemplate
  :: Text -- raw component name
  -> Text -- sanitized component name
  -> Text
componentTemplate rawComponentName componentName = unlines
  [ "module " <> componentName <> " where"
  , mempty
  , "import Prelude"
  , "import Data.Symbol ( SProxy(..) )"
  , "-- halogen"
  , "import Halogen as H"
  , "import Halogen.HTML as HH"
  , mempty
  , "type Slot p = forall query. H.Slot query Void p"
  , mempty
  , "_" <> toCamelCase rawComponentName <> " :: SProxy \"" <> toCamelCase rawComponentName <> "\""
  , "_" <> toCamelCase rawComponentName <> " = SProxy"
  , mempty
  , "component :: forall q i o m. H.Component HH.HTML q i o m"
  , "component = "
  , "  H.mkComponent"
  , "    { initialState: identity"
  , "    , render: const $ HH.h1_ [ HH.text \"" <> toPascalCase rawComponentName <> " Component\" ]"
  , "    , eval: H.mkEval H.defaultEval"
  , "    }"
  ]
  where
    toCamelCase :: Text -> Text
    toCamelCase = T.pack . camel . T.unpack

spagoTemplate :: Text -> Text
spagoTemplate projectName = unlines
  [ "{-"
  , "Welcome to a Spago project!"
  , "You can edit this file as you like."
  , "-}"
  , "{ name = \"" <> projectName <> "\""
  , ", dependencies ="
  , "    [ \"console\""
  , "    , \"effect\""
  , "    , \"halogen\""
  , "    , \"psci-support\""
  , "    , \"routing\""
  , "    , \"routing-duplex\""
  , "    ]"
  , ", packages = ./packages.dhall"
  , ", sources = [ \"src/**/*.purs\", \"test/**/*.purs\" ]"
  , "}"
  ]
