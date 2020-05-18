{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Util
  ( mkPathName
  , isFileExists
  , generateFile
  , generateWhenFileNotExists
  , generateDir
  , dirResHandler
  , toPascalCase
  , discardFirstDot
  , filterLower
  , concatWithDot
  , splitAtPathSeparator
  ) where

import           Import
-- text
import           Data.Char
import qualified Data.Text                 as T
import           Text.Casing               (pascal)
-- turtle
import qualified Turtle
import qualified Turtle.Prelude            as TP
-- umu-halogen
import           UmuHalogen.Capability.Log
--filepath
import qualified System.FilePath           as FP

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

-- Right is considered the success case here, and means the directory was
-- created. Left will be the error.
generateDir :: MonadIO m => Maybe Text -> Text -> m ( Either () () )
generateDir mPathInput dirName = liftIO
  $ tryJust ( guard . isAlreadyExistsError )
  $ TP.mkdir ( Turtle.fromText $ mkPathName mPathInput dirName )

-- Directory generation response handler
dirResHandler :: ( MonadIO m, LogMessage m ) => Text -> Either () () -> m ()
dirResHandler dirName res = either
  ( const $ logError $ dirName <> " directory already exists!" )
  ( const $ logInfo $ "Generated " <> dirName )
  res

toPascalCase :: Text -> Text
toPascalCase = T.pack . pascal . T.unpack

discardFirstDot :: Text -> Maybe ( Char, Text )
discardFirstDot = T.uncons

filterLower :: [ Text ]  -> [ Text ]
filterLower = filter ( not . all isLower )

concatWithDot :: [ Text ] -> Text
concatWithDot = concat . fmap ( "." <> )

splitAtPathSeparator :: Text -> [ Text ]
splitAtPathSeparator = T.split ( FP.pathSeparator == )
