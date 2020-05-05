{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Util
  ( mkPathName
  , isFileExists
  , generateFile
  ) where

import           Import
import qualified Turtle
import qualified Turtle.Prelude                   as TP
import           UmuHalogen.Capability.LogMessage

mkPathName :: Maybe Text -> Text -> Text
mkPathName mDirPathInput filePath =
  -- If the user doesn't provide mDirPathInput which is the target directory,
  -- then generate in the current directory.
  maybe "./" (\loc -> "./" <> loc <> "/") mDirPathInput <> filePath

isFileExists :: MonadIO m => Maybe Text -> Text -> m Bool
isFileExists mPathInput filePath =
  TP.testfile $ Turtle.fromText ( mkPathName mPathInput filePath )

generateFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> Text -> Text -> m ()
generateFile mPathInput filePath file = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mPathInput filePath ) file
  logInfo ( "Generated " <> filePath )
