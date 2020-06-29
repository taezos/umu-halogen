module UmuHalogen.Log
  ( LogReason (..)
  , LogMessage (..)
  , Log
  , mkLog
  , mkTerminalLog
  ) where

import           Import
-- text
import qualified Data.Text           as T
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

mkLog :: Monad m => LogReason -> Text -> m Log
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
  putStr $ T.unpack logHeader
  liftIO $ ANSI.setSGR []
  putStrLn $ T.unpack msg
  where
    reasonToColor :: LogReason -> Color
    reasonToColor lr = case lr of
      Info  -> ANSI.Green
      Debug -> ANSI.Blue
      Error -> ANSI.Red
      Warn  -> ANSI.Yellow

