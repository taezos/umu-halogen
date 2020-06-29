{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuHalogen ( startApp ) where

-- prelude
import           Import

-- mtl
import           Control.Monad.Except

-- optparse-applicative
import           Options.Applicative

-- umu-halogen
import           UmuHalogen.Capability.Generation
import           UmuHalogen.Capability.Log
import           UmuHalogen.Error
import           UmuHalogen.Parser.Command
import           UmuHalogen.Parser.Route

newtype AppM m a
  = AppM
  { unAppM :: ( ExceptT UmuError m ) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError UmuError )

runAppM :: MonadIO m => Command -> ExceptT UmuError m [ UmuResponse ]
runAppM comm = unAppM $ convertApp comm

convertApp :: MonadIO m => Command -> AppM m [ UmuResponse ]
convertApp comm =
  case comm of
    CommandInit mPath -> either
      throwError
      (\p -> generateProject p generateDirectories writeInitialDir generateFiles )
      ( sequenceA mPath )
    CommandComponent path componentName -> either
      throwError
      (\p -> pure <$> generateComponent p componentName writeComponentFile )
      path
    CommandRoute path routeName -> either
      throwError
      (\p -> pure <$> generateRoute p routeName updateRouteFile parseRouteFile )
      path

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseVersion <*> parseCommand )
      ( fullDesc <> progDesc umuProgDesc <> header umuHeader ))
  res <- runExceptT $ runAppM comm
  either
    ( mkTerminalLogEff <=< logErrorNoEff . umuErrorToText )
    ( traverse_ ( mkTerminalLogEff <=< logInfoNoEff . umuResponseToText ) )
    res

instance MonadIO m => ManageGeneration ( AppM m ) where
  generateProject = genProject
  generateComponent = genComponent
  generateRoute = genRoute

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-halogen to generate a scaffold for a halogen project"

umuHeader :: String
umuHeader = "umu-halogen: Generate Halogen Project"
