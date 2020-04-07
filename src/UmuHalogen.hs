{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuHalogen where

import           Import
import           Options.Applicative
-- Lens
import           Lens.Micro
-- Umu
import           UmuHalogen.Capability.LogMessage
import           UmuHalogen.Capability.ManageCommand
import           UmuHalogen.Command
import           UmuHalogen.Log

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

instance MonadIO m => LogMessage ( AppM m ) where
  logMessage l = case l ^. logReason of
    Info  -> mkTerminalLog
      ( l ^. logMsg . logMessageText )
      Info
      ( l ^. logMsg . logMessageHeader )
    Debug -> mkTerminalLog
      ( l ^. logMsg . logMessageText )
      Debug
      ( l ^. logMsg . logMessageHeader )
    Error -> mkTerminalLog
      ( l ^. logMsg . logMessageText )
      Error
      ( l ^. logMsg . logMessageHeader )
    Warn  -> mkTerminalLog
      ( l ^. logMsg . logMessageText )
      Warn
      ( l ^. logMsg . logMessageHeader )

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

umuProgDesc :: String
umuProgDesc = "Use umu-halogen to generate a scaffold for a halogen project"

umuHeader :: String
umuHeader = "umu-halogen: Generate Halogen Project"
