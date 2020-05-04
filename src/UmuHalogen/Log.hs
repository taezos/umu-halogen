{-# LANGUAGE TemplateHaskell #-}
module UmuHalogen.Log
  ( LogReason (..)
  , Log
  , mkLog
  , logReason
  , logMsg
  , logMessageText
  , logMessageHeader
  , mkTerminalLog
  ) where

import           Import
import           Lens.Micro
import           System.Console.ANSI as ANSI

data LogReason
  = Debug
  | Info
  | Warn
  | Error
  deriving ( Eq, Show )

data Log = Log
  { _logReason :: LogReason
  , _logMsg    :: LogMessage
  } deriving ( Eq, Show )

data LogMessage = LogMessage
  { _logMessageText   :: Text
  , _logMessageHeader :: Text
  } deriving ( Eq, Show )

mkLog :: MonadIO m => LogReason -> Text -> m Log
mkLog reason msg = do
  pure $ Log
    { _logReason = reason
    , _logMsg = LogMessage
      { _logMessageText = msg
      , _logMessageHeader = mkHeader reason
      }
    }
  where
    mkHeader :: LogReason -> Text
    mkHeader res = case res of
      Debug -> "[DEBUG]: "
      Info  -> "[INFO]: "
      Warn  -> "[WARN]: "
      Error -> "[ERROR]: "

mkTerminalLog :: MonadIO m => Text -> LogReason -> Text -> m ()
mkTerminalLog msg reason logHeader = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( reasonToColor reason ) ]
  putStr logHeader
  liftIO $ ANSI.setSGR []
  putStrLn msg
  where
    reasonToColor :: LogReason -> Color
    reasonToColor lr = case lr of
      Info  -> ANSI.Green
      Debug -> ANSI.Blue
      Error -> ANSI.Red
      Warn  -> ANSI.Yellow

-- lens
logMessageText :: Lens' LogMessage Text
logMessageText fn logMessage@LogMessage{ _logMessageText = msg } =
  fn msg <&> \newMsg -> logMessage { _logMessageText = newMsg }

logMessageHeader :: Lens' LogMessage Text
logMessageHeader fn logMessage@LogMessage { _logMessageHeader = header  } =
  fn header <&> \newHeader -> logMessage { _logMessageHeader = newHeader }

logReason :: Lens' Log LogReason
logReason fn logR@Log{ _logReason = reason } =
  fn reason <&> \newReason -> logR { _logReason = newReason }

logMsg :: Lens' Log LogMessage
logMsg fn logR@Log{ _logMsg = msg } =
  fn msg <&> \newMessage -> logR { _logMsg = newMessage }
