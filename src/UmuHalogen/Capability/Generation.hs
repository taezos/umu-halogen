{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UmuHalogen.Capability.Generation
  ( genProj
  , ManageGeneration (..)
  , genComponent
  ) where

import           Import
-- text
import           Data.Char
import qualified Data.Text                 as T
-- filepath
import qualified System.FilePath           as FP
-- directory
import qualified System.Directory          as Directory
-- turtle
import qualified Turtle
import           Turtle.Prelude            as TP
-- umu
import           UmuHalogen.Capability.Log
import           UmuHalogen.Templates
import           UmuHalogen.Types
import           UmuHalogen.Util

class Monad m => ManageGeneration m where
  generateProject :: Maybe Text -> m ()
  generateComponent :: PathInput -> ComponentName -> m ()

instance ManageGeneration IO where
  generateProject = liftIO . generateProject
  generateComponent path = liftIO . generateComponent path

genProj :: ( MonadIO m, LogMessage m, ManageGeneration m ) => Maybe Text -> m ()
genProj mLoc = case mLoc of
  Nothing -> baseGeneration mLoc
  Just loc -> do
    writeInitialDir loc
    baseGeneration mLoc

genComponent
  :: ( MonadIO m, LogMessage m, ManageGeneration m )
  => PathInput
  -> ComponentName
  -> m ()
genComponent pathInput componentName =
  writeComponentFile pathInput componentName

baseGeneration
  :: ( MonadIO m, LogMessage m, ManageGeneration m )
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
      <> " will continue to generate the scaffold in that directory"

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
writeComponentFile :: ( MonadIO m, LogMessage m ) => PathInput -> ComponentName -> m ()
writeComponentFile path componentName = do
  fileExists <- TP.testfile $ Turtle.fromText sanitizedFilePath
  dirExists <- TP.testdir $ Turtle.fromText sanitizedDirPath
  if | fileExists -> logError ( sanitizedFilePath <> " already exists!" )
     | dirExists && not fileExists -> do
         liftIO $ TP.writeTextFile ( Turtle.fromText sanitizedFilePath )
          ( componentTemplate ( fromComponentName componentName ) sanitizedComponentName )
         logInfo ( "Generated " <> sanitizedComponentName <> " component to " <> sanitizedDirPath )
     | otherwise -> logError $ sanitizedDirPath <> " does not exist!"
  where
    sanitizedComponentName :: Text
    sanitizedComponentName =
      maybe "" ( <> "." <> ( fromComponentName componentName ))
      $ snd
      <$> ( discardFirstDot . concatWithDot . filterLower . splitAtPathSeparator . fromPathInput $ path )

    discardFirstDot :: Text -> Maybe ( Char, Text )
    discardFirstDot = T.uncons

    filterLower :: [ Text ]  -> [ Text ]
    filterLower = filter ( not . all isLower )

    concatWithDot :: [ Text ] -> Text
    concatWithDot = concat . fmap ( "." <> )

    splitAtPathSeparator :: Text -> [ Text ]
    splitAtPathSeparator = T.split ( FP.pathSeparator == )

    sanitizedDirPath :: Text
    sanitizedDirPath = snoc ( fromPathInput path ) FP.pathSeparator

    pursFileName :: Text
    pursFileName = ( fromComponentName componentName ) <> ".purs"

    sanitizedFilePath :: Text
    sanitizedFilePath = snoc ( fromPathInput path ) FP.pathSeparator <> pursFileName

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mPathInput = do
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists isExists mPathInput filePath srcMainFile
  where
    filePath :: Text
    filePath = "src/Main.purs"

writeSpagoFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoFile mPathInput = do
  mCurrentDirectory <- liftIO
    $ listToMaybe
    . reverse
    . T.split ( == FP.pathSeparator )
    . T.pack
    <$> Directory.getCurrentDirectory
  isExists <- isFileExists mPathInput filePath
  generateWhenFileNotExists
    isExists
    mPathInput
    filePath
    ( spagoTemplate
      $ fromMaybe mempty
      $ flip fromMaybe mPathInput <$> mCurrentDirectory )
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

