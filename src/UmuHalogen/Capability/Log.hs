module UmuHalogen.Capability.Log where

import           Import         hiding (log)
import           UmuHalogen.Log

class Monad m => LogMessage m where
  logMessage :: Log -> m ()

log :: ( MonadIO m, LogMessage m ) => LogReason -> Text -> m ()
log reason = logMessage <=< mkLog reason

logInfo :: ( MonadIO m, LogMessage m ) => Text -> m ()
logInfo = log Info

logWarn :: ( MonadIO m, LogMessage m ) => Text -> m ()
logWarn = log Warn

logDebug :: ( MonadIO m, LogMessage m ) => Text -> m ()
logDebug = log Debug

logError :: ( MonadIO m, LogMessage m ) => Text -> m ()
logError = log Error
