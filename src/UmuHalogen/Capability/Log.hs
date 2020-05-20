module UmuHalogen.Capability.Log where

import           Import
-- lens
import           Lens.Micro
-- umu
import           UmuHalogen.Log

class Monad m => LogMessage m where
  logMessage :: Log -> m ()

instance LogMessage IO where
  logMessage = logMessageImpl

logMessageImpl :: MonadIO m => Log -> m ()
logMessageImpl l = case l ^. logReason of
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

log :: ( MonadIO m, LogMessage m ) => LogReason -> Text ->  m ()
log reason msg = logMessage =<< mkLog reason msg

logInfo :: ( MonadIO m, LogMessage m ) => Text ->  m ()
logInfo msg  = log Info msg

logWarn :: ( MonadIO m, LogMessage m ) => Text ->  m ()
logWarn msg  = log Warn msg

logDebug :: ( MonadIO m, LogMessage m ) => Text ->  m ()
logDebug msg  = log Debug msg

logError :: ( MonadIO m, LogMessage m ) => Text ->  m ()
logError msg  = log Error msg
