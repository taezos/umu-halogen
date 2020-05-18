{-# LANGUAGE TemplateHaskell #-}
module UmuHalogen.Templates where

import           Import
-- text
import qualified Data.Text       as T
import           Text.Casing     (camel)
-- umu-halogen
import           UmuHalogen.TH
import           UmuHalogen.Util (toPascalCase)


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

routerComponentTemplate :: Text
routerComponentTemplate = unlines
  [ "module Component.Router where"
  , mempty
  , "import Prelude"
  , "import Data.Maybe ( Maybe(..), fromMaybe )"
  , "import Data.Either ( hush )"
  , "-- internal route"
  , "import Service.Route ( Route(..), routeCodec )"
  , "import Service.Navigate ( navigate, class Navigate )"
  , "-- effect"
  , "import Effect.Class ( class MonadEffect )"
  , " -- routing"
  , "import Routing.Duplex ( parse )"
  , "import Routing.Hash ( getHash )"
  , "-- halogen"
  , "import Halogen as H"
  , "import Halogen.HTML as HH"
  , mempty
  , "type State ="
  , "  { route :: Maybe Route"
  , "  }"
  , mempty
  , "data Action"
  , "  = Initialize"
  , "  | GoTo Route"
  , mempty
  , "component"
  , "  :: forall q i o m"
  , "  . MonadEffect m"
  , "  => Navigate m"
  , "  => H.Component HH.HTML q i o m"
  , "component ="
  , "  H.mkComponent"
  , "  { initialState: const { route: Nothing }"
  , "  , render"
  , "  , eval: H.mkEval H.defaultEval"
  , "    { handleAction = handleAction"
  , "    , initialize = Just Initialize"
  , "    }"
  , "  }"
  , mempty
  , "render :: forall action m. State -> H.ComponentHTML action () m"
  , "render { route } = case route of"
  , "  Nothing -> HH.h1_ [ HH.text \"Oops! That page wasn't found\" ]"
  , "  Just r -> case r of"
  , "    Home -> HH.div_ []"
  , mempty
  , "handleAction"
  , "  :: forall o m"
  , "  . MonadEffect m"
  , "  => Navigate m"
  , "  => Action"
  , "  -> H.HalogenM State Action () o m Unit"
  , "handleAction = case _ of"
  , "  Initialize -> do"
  , "    initialRoute <- hush <<< ( parse routeCodec ) <$> H.liftEffect getHash"
  , "    navigate $ fromMaybe Home initialRoute"
  , "  GoTo route -> navigate route"
  ]

homePageTemplate :: Text
homePageTemplate = unlines
  [ "module Page.Home where"
  , mempty
  , "import Prelude"
  , "import Data.Symbol ( SProxy(..) )"
  , "-- internal component"
  , "import Component.Title as Title"
  , "-- halogen"
  , "import Halogen as H"
  , "import Halogen.HTML as HH"
  , mempty
  , "type Slot p = forall query. H.Slot query Void p"
  , mempty
  , "type ChildSlots ="
  , "  ( title :: Title.Slot Unit"
  , "  )"
  , mempty
  , "_home :: SProxy \"home\""
  , "_home = SProxy"
  , mempty
  , "component :: forall q i o m. H.Component HH.HTML q i o m"
  , "component ="
  , "  H.mkComponent"
  , "  { initialState: identity"
  , "  , render"
  , "  , eval: H.mkEval H.defaultEval"
  , "  }"
  , mempty
  , "render :: forall state action m. state -> H.ComponentHTML action ChildSlots m"
  , "render _ ="
  , "  HH.div_"
  , "  [ HH.h1_ [ HH.text \"Home\" ]"
  , "  , HH.slot Title._title unit Title.component {} absurd"
  , "  ]"
  ]

routeTemplate :: Text
routeTemplate = unlines
  [ "module Service.Route where"
  , mempty
  , "import Prelude hiding ((/))"
  , mempty
  , "import Data.Generic.Rep ( class Generic )"
  , "import Data.Generic.Rep.Show ( genericShow )"
  , "-- routing"
  , "import Routing.Duplex ( RouteDuplex', root )"
  , "import Routing.Duplex.Generic ( noArgs, sum )"
  , "import Routing.Duplex.Generic.Syntax ((/))"
  , mempty
  , "data Route = Home"
  , mempty
  , "derive instance genericRoute :: Generic Route _"
  , "derive instance eqRoute :: Eq Route"
  , "derive instance ordRoute :: Ord Route"
  , mempty
  , "instance showRoute :: Show Route where"
  , "  show = genericShow"
  , mempty
  , "routeCodec :: RouteDuplex' Route"
  , "routeCodec = root $ sum"
  , "  { \"Home\": noArgs }"
  ]

navigateTemplate :: Text
navigateTemplate = unlines
  [ "module Service.Navigate where"
  , mempty
  , "import Prelude"
  , "import Control.Monad.Trans.Class ( lift )"
  , "-- internal service"
  , "import Service.Route ( Route )"
  , "-- halogen"
  , "import Halogen ( HalogenM )"
  , mempty
  , "class Monad m <= Navigate m where"
  , "  navigate :: Route -> m Unit"
  , mempty
  , "instance navigateHalogenM :: Navigate m => Navigate ( HalogenM state action slogs msg m ) where"
  , "  navigate = lift <<< navigate"
  ]
