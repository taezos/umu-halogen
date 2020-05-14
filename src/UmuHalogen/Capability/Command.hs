{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.Command
  ( genProj
  , ManageCommand (..)
  , genComponent
  ) where

import           Import
-- text
import           Data.Char
import qualified Data.Text                        as T
-- filepath
import qualified System.FilePath                  as FP
-- turtle
import qualified Turtle
import           Turtle.Prelude                   as TP
-- umu
import           UmuHalogen.Capability.LogMessage
import           UmuHalogen.Templates
import           UmuHalogen.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()
  generateComponent :: Text -> Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject
  generateComponent path = liftIO . generateComponent path

genProj :: ( MonadIO m, LogMessage m, ManageCommand m ) => Maybe Text -> m ()
genProj mLoc = case mLoc of
  Nothing -> baseGeneration mLoc
  Just loc -> do
    writeInitialDir loc
    baseGeneration mLoc

genComponent :: ( MonadIO m, LogMessage m, ManageCommand m ) => Text -> Text -> m ()
genComponent path componentName =
  writeComponentFile path componentName

baseGeneration
  :: ( MonadIO m, LogMessage m, ManageCommand m )
  => Maybe Text
  -> m ()
baseGeneration mLoc = do
  traverse_ ( $ mLoc )
    [ writeSrcDir
    , writeSrcMainFile
    , writeSpagoFile
    , writePackagesFile
    , writeAssetsDir
    , writeIndexHTMLFile
    , writeIndexJSFile
    , writeTestDir
    , writeTestMainFile
    , writeComponentDir
    , writeTitleComponentFile
    , writePackageJson
    , writeMakeFile
    ]

-----------------------------------------------------------
-- Directory Generation
-----------------------------------------------------------
writeInitialDir :: ( MonadIO m, LogMessage m ) => Text -> m ()
writeInitialDir loc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText loc )
  either
    ( const $ logWarn warningMessage )
    ( const $ logInfo $ "Generated " <> loc  )
    res
  where
    warningMessage :: Text
    warningMessage = loc
      <> " already exists but "
      <> appName
      <> " will continue to generate in that directory"

writeSrcDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "src"

writeAssetsDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeAssetsDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "assets"

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "test"

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mPathInput = do
  res <- isDirGenerated mPathInput dirName
  dirResHandler dirName res
  where
    dirName :: Text
    dirName = "src/Component"

-----------------------------------------------------------
-- File Generation
-----------------------------------------------------------
writeComponentFile :: ( MonadIO m, LogMessage m ) => Text -> Text -> m ()
writeComponentFile path componentName = do
  fileExists <- TP.testfile $ Turtle.fromText sanitizedFilePath
  dirExists <- TP.testdir $ Turtle.fromText sanitizedDirPath
  if | fileExists -> logError ( sanitizedFilePath <> " already exists!" )
     | dirExists && not fileExists -> do
         liftIO $ TP.writeTextFile ( Turtle.fromText sanitizedFilePath )
          ( componentTemplate componentName sanitizedComponentName )
         logInfo ( "Generated " <> sanitizedComponentName <> " component to " <> sanitizedDirPath )
     | otherwise -> logError $ sanitizedDirPath <> " does not exist!"
  where
    sanitizedComponentName :: Text
    sanitizedComponentName =
      maybe "" ( <> "." <> toPascalCase componentName )
      $ snd
      <$> ( discardFirstDot . concatWithDot . filterLower . splitAtPathSeparator $ path )

    discardFirstDot :: Text -> Maybe ( Char, Text )
    discardFirstDot = T.uncons

    filterLower :: [ Text ]  -> [ Text ]
    filterLower = filter ( not . all isLower )

    concatWithDot :: [ Text ] -> Text
    concatWithDot = concat . fmap ( "." <> )

    splitAtPathSeparator :: Text -> [ Text ]
    splitAtPathSeparator = T.split ( FP.pathSeparator == )

    sanitizedDirPath :: Text
    sanitizedDirPath = snoc path FP.pathSeparator

    pursFileName :: Text
    pursFileName = toPascalCase componentName <> ".purs"

    sanitizedFilePath :: Text
    sanitizedFilePath = snoc path FP.pathSeparator <> pursFileName

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath srcMainFile
  where
    filePath :: Text
    filePath = "src/Main.purs"

writeSpagoFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath spagoDhallFile
  where
    filePath :: Text
    filePath = "spago.dhall"

writePackagesFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath packagesDhallFile
  where
    filePath :: Text
    filePath = "packages.dhall"

writeIndexHTMLFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHTMLFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath indexHtmlFile
  where
    filePath :: Text
    filePath = "assets/index.html"

writeIndexJSFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexJSFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath indexJS
  where
    filePath :: Text
    filePath = "assets/index.js"

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath testMainFile
  where
    filePath :: Text
    filePath = "test/Main.purs"

writeTitleComponentFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath titleComponentFile
  where
    filePath :: Text
    filePath = "src/Component/Title.purs"

writePackageJson :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJson mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath packageJsonFile
  where
    filePath :: Text
    filePath = "package.json"

writeMakeFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakeFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath makeFile
  where
    filePath :: Text
    filePath = "Makefile"

