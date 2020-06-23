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

newtype AppM m a
  = AppM
  { unAppM :: ( ExceptT UmuError m ) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError UmuError )

runAppM :: MonadIO m => Command -> ExceptT UmuError m [ UmuResponse ]
runAppM comm = unAppM $ convertApp comm

convertApp :: MonadIO m => Command -> AppM m [ UmuResponse ]
convertApp comm =
  case comm of
    CommandInit mPath -> either throwError generateProject ( sequenceA mPath )
    CommandComponent path componentName -> either
      throwError
      ( ( pure <$> ) . flip generateComponent componentName )
      path
    CommandRoute path routeName -> either
      throwError
      ( ( pure <$> ) . flip generateRoute routeName )
      path

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseVersion <*> parseCommand )
      ( fullDesc <> progDesc umuProgDesc <> header umuHeader ))
  res <- runExceptT $ runAppM comm
  either
    ( logError . umuErrorToText )
    ( traverse_ ( logInfo . umuResponseToText ) )
    res

instance MonadIO m => ManageGeneration ( AppM m ) where
  generateProject = genProject
  generateComponent = genComponent
  generateRoute = genRoute

instance MonadIO m => LogMessage ( AppM m ) where
  logMessage = logMessageImpl

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-halogen to generate a scaffold for a halogen project"

umuHeader :: String
umuHeader = "umu-halogen: Generate Halogen Project"
