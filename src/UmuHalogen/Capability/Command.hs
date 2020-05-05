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
genComponent path componentName = do
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
writeSrcDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  either
    ( const $ logError "src directory already exists!" )
    ( const $ logInfo "Generated src" )
    res

writeAssetsDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeAssetsDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "assets" )
  either
    ( const $ logError "assets directory already exists!" )
    ( const $ logInfo "Generated assets" )
    res

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  either
    ( const $ logError "test directory already exists!" )
    ( const $ logInfo "Generated test" )
    res

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src/Component" )
  either
    ( const $ logError "src/Component already exists!" )
    ( const $ logInfo "Generated src/Component" )
    res

-----------------------------------------------------------
-- File Generation
-----------------------------------------------------------
writeComponentFile :: ( MonadIO m, LogMessage m ) => Text -> Text -> m ()
writeComponentFile path componentName = do
  fileExists <- TP.testfile $ Turtle.fromText verifiedFilePath
  dirExists <- TP.testdir $ Turtle.fromText verifiedDirPath
  if | fileExists -> logError ( verifiedFilePath <> " already exists!" )
     | dirExists && not fileExists -> do
         liftIO $ TP.writeTextFile ( Turtle.fromText verifiedFilePath )
          ( componentTemplate verifiedComponentName )
         logInfo ( "Generated " <> verifiedComponentName <> " component to " <> verifiedDirPath )
     | otherwise -> logError $ verifiedDirPath <> " does not exist!"
  where
    verifiedComponentName :: Text
    verifiedComponentName =
      maybe "" ( <> "." <> componentName )
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

    verifiedDirPath :: Text
    verifiedDirPath = snoc path FP.pathSeparator

    pursFileName :: Text
    pursFileName = componentName <> ".purs"

    verifiedFilePath :: Text
    verifiedFilePath = snoc path FP.pathSeparator <> pursFileName

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

