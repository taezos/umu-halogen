{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module UmuHalogen ( startApp ) where

import           Control.Monad.Except
import           Import
import           Options.Applicative
-- umu
import           UmuHalogen.Capability.Generation
import           UmuHalogen.Capability.Log
import           UmuHalogen.Error
import           UmuHalogen.Parser.Command

newtype AppM m a
  = AppM
  { unAppM :: ReaderT Command ( ExceptT UmuError m ) a
  } deriving ( Functor, Applicative, Monad, MonadIO, MonadError UmuError, MonadReader Command )

runAppM :: MonadIO m => Command -> ExceptT UmuError m [ UmuResponse ]
runAppM comm = runReaderT ( unAppM $ convertApp comm ) comm

convertApp :: MonadIO m => Command -> AppM m [ UmuResponse ]
convertApp comm =
  case comm of
    CommandInit mPath -> generateProject mPath
    CommandComponent path componentName -> either
      throwError
      ( ( pure <$> ) . flip generateComponent componentName )
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

instance MonadIO m => LogMessage ( AppM m ) where
  logMessage = logMessageImpl

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-halogen to generate a scaffold for a halogen project"

umuHeader :: String
umuHeader = "umu-halogen: Generate Halogen Project"
