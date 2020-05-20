{-# LANGUAGE FlexibleContexts  #-}
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

import           Control.Exception         (tryJust)
import           Control.Monad.Except
import           Import
-- text
import           Data.Char                 (isLower)
import qualified Data.Text                 as T
import           Text.Casing               (pascal)
-- turtle
import qualified Turtle
import qualified Turtle.Prelude            as TP
--filepath
import qualified System.FilePath           as FP
-- system
import           System.IO.Error           (isAlreadyExistsError)
-- umu-halogen
import           UmuHalogen.Capability.Log
import           UmuHalogen.Error

mkPathName :: Maybe Text -> Text -> Text
mkPathName mDirPathInput filePath =
  -- If the user doesn't provide mDirPathInput which is the target directory,
  -- then generate in the current directory.
  maybe "./" (\loc -> "./" <> loc <> "/") mDirPathInput <> filePath

isFileExists :: MonadIO m => Maybe Text -> Text -> m Bool
isFileExists mPathInput filePath =
  TP.testfile $ Turtle.fromText ( mkPathName mPathInput filePath )

generateFile
  :: ( MonadIO m, LogMessage m )
  => Maybe Text
  -> Text
  -> Text
  -> m UmuResponse
generateFile mPathInput filePath file = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mPathInput filePath ) file
  pure $ FileGenerationSuccess $ "Generated " <> filePath

generateWhenFileNotExists
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Bool
  -> Maybe Text
  -> Text
  -> Text
  -> m UmuResponse
generateWhenFileNotExists isExists mPathInput filePath file
  | isExists = throwError $ FileGenerationError $ filePath <> " already exists!"
  | otherwise = generateFile mPathInput filePath file

-- Right is considered the success case here, and means the directory was
-- created. Left will be the error.
generateDir :: MonadIO m => Maybe Text -> Text -> m ( Either () () )
generateDir mPathInput dirName = liftIO
  $ tryJust ( guard . isAlreadyExistsError )
  $ TP.mkdir ( Turtle.fromText $ mkPathName mPathInput dirName )

-- Directory generation response handler
dirResHandler
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Text
  -> Either () ()
  -> m UmuResponse
dirResHandler dirName res = either
  ( const $ throwError $ DirectoryGenerationError $ dirName <> " directory already exists!" )
  ( const $ pure $ DirectoryGenerationSuccess $ "Generated " <> dirName )
  res

toPascalCase :: Text -> Text
toPascalCase = T.pack . pascal . T.unpack

discardFirstDot :: Text -> Maybe ( Char, Text )
discardFirstDot = T.uncons

filterLower :: [ Text ]  -> [ Text ]
filterLower = filter ( not . T.all isLower )

concatWithDot :: [ Text ] -> Text
concatWithDot = T.concat . fmap ( "." <> )

splitAtPathSeparator :: Text -> [ Text ]
splitAtPathSeparator = T.split ( FP.pathSeparator == )
