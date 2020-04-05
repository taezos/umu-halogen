{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Util
  ( message
  ) where

import           Import
import           System.Console.ANSI as ANSI

message :: MonadIO m => Text -> m ()
message msg = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green ]
  putStr "[INFO]: "
  liftIO $ ANSI.setSGR [ ]
  putStrLn msg
