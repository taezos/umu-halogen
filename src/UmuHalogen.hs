{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuHalogen where

import           Import
import           Options.Applicative
import           UmuHalogen.Capability.ManageCommand
import           UmuHalogen.Command

newtype AppM m a
  = AppM
  { unAppM :: m a
  } deriving ( Functor, Applicative, Monad, MonadIO )

runAppM :: MonadIO m => AppM m a -> m a
runAppM app = unAppM app

startApp :: IO ()
startApp = do
  comm <- showHelpOnErrorExecParser
    ( info ( helper <*> parseVersion <*> parseCommand )
      ( fullDesc <> progDesc umuProgDesc <> header umuHeader ))
  runAppM $ run comm
  where
    run :: MonadIO m => Command -> AppM m ()
    run comm = case comm of
      CommandInit mLoc -> do
        generateProject mLoc

instance MonadIO m => ManageCommand ( AppM m ) where
  generateProject = genProj

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-halogen to generate a scaffold for a halogen project"

umuHeader :: String
umuHeader = "umu-halogen: Generate Halogen Project"
