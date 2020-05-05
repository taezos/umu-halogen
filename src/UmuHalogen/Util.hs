{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Util
  ( mkPathName
  , isFileExists
  , generateFile
  , generateWhenFileNotExists
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

generateWhenFileNotExists
  :: ( MonadIO m, LogMessage m )
  => Bool
  -> Maybe Text
  -> Text
  -> Text
  -> m ()
generateWhenFileNotExists isExists mPathInput filePath file
  | isExists = logError ( filePath <> " already exists! " )
  | otherwise = generateFile mPathInput filePath file
