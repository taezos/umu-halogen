module UmuHalogen.Capability.Log where

import           Import
-- lens
import           Control.Lens.Operators
-- ansi-terminal
import           System.Console.ANSI    as ANSI
-- text
import qualified Data.Text              as T
-- umu
import           UmuHalogen.Log
import           UmuHalogen.Optics

class Monad m => ManageLog m where
  logMessage :: Log -> m Log

instance ManageLog IO where
  logMessage = logMessageImpl

logMessageImpl :: ManageLog m => Log -> m Log
logMessageImpl logDesc = case logDesc ^. logReason of
  Info  -> logInfoNoEff $ logDesc ^. logMsg . logMessageText
  Debug -> logDebugNoEff $ logDesc ^. logMsg . logMessageText
  Warn  -> logWarnNoEff $ logDesc ^. logMsg . logMessageText
  Error -> logErrorNoEff $ logDesc ^. logMsg . logMessageText

logNoEff :: ( Monad m, ManageLog m ) => LogReason -> Text -> m Log
logNoEff reason msg = logMessage =<< mkLog reason msg

logInfoNoEff :: ( Monad m, ManageLog m ) => Text -> m Log
logInfoNoEff msg = logNoEff Info msg

logWarnNoEff :: ( Monad m, ManageLog m ) => Text -> m Log
logWarnNoEff msg = logNoEff Warn msg

logDebugNoEff :: ( Monad m, ManageLog m ) => Text -> m Log
logDebugNoEff msg = logNoEff Debug msg

logErrorNoEff :: ( Monad m, ManageLog m ) => Text -> m Log
logErrorNoEff msg = logNoEff Error msg

mkTerminalLogEff :: MonadIO m => Log -> m ()
mkTerminalLogEff logDesc = do
  liftIO
    $ ANSI.setSGR
    [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( reasonToColor $  logDesc ^. logReason ) ]
  putStr $ T.unpack $ logDesc ^. logMsg . logMessageHeader
  liftIO $ ANSI.setSGR []
  putStrLn $ T.unpack $ logDesc ^. logMsg . logMessageText
  where
    reasonToColor :: LogReason -> Color
    reasonToColor lr = case lr of
      Info  -> ANSI.Green
      Debug -> ANSI.Blue
      Error -> ANSI.Red
      Warn  -> ANSI.Yellow
