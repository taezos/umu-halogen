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
-- Turtle
import qualified Turtle
import           Turtle.Prelude                   as TP
-- Umu
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
    ( const $ logInfo $ "Generating " <> loc <> "..." )
    res
  where
    warningMessage :: Text
    warningMessage = loc
      <> " already exists but "
      <> appName
      <> " will continue to generate in that directory..."

writeSrcDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  either
    ( const $ logError "src directory already exists!" )
    ( const $ logInfo "Generating src..." )
    res

writeAssetsDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeAssetsDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "assets" )
  either
    ( const $ logError "assets directory already exists!" )
    ( const $ logInfo "Generating assets..." )
    res

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  either
    ( const $ logError "test directory already exists!" )
    ( const $ logInfo "Generating test..." )
    res

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src/Component" )
  either
    ( const $ logError "src/Component already exists!" )
    ( const $ logInfo "Generating src/Component..." )
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
         logInfo ( "Generating " <> verifiedComponentName <> " component to " <> verifiedDirPath )
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
writeSrcMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "src/Main.purs"
  if isExists
    then logError "src/Main.purs already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "src/Main.purs" ) srcMainFile
      logInfo "Generating src/Main.purs..."

writeSpagoFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "spago.dhall"
  if isExists
    then logError "spago.dhall already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
      logInfo "Generating spago.dhall..."

writePackagesFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "packages.dhall"
  if isExists
    then logError "packages.dhall already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "packages.dhall") packagesDhallFile
      logInfo "Generating packages.dhall..."

writeIndexHTMLFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHTMLFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc fileLocation
  if isExists
    then logError $ fileLocation <> " already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc fileLocation ) indexHtmlFile
      logInfo $ "Generating " <> fileLocation <> "..."
  where
    fileLocation :: Text
    fileLocation = "assets/index.html"

writeIndexJSFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexJSFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc fileLocation
  if isExists
    then logError $ fileLocation <> " already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc fileLocation ) indexJS
      logInfo $ "Generating " <> fileLocation <> "..."
  where
    fileLocation = "assets/index.js"

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "test/Main.purs"
  if isExists
    then logError "test/Main.purs already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "test/Main.purs" ) testMainFile
      logInfo "Generating test/Main.purs..."

writeTitleComponentFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "src/Component/Title.purs"
  if isExists
    then logError "src/Component/Title.purs already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "src/Component/Title.purs" ) titleComponentFile
      logInfo "Generating src/Component/Title.purs..."

writePackageJson :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJson mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "package.json"
  if isExists
    then logError "package.json already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "package.json" ) packageJsonFile
      logInfo "Generating package.json..."

writeMakeFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakeFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "Makefile"
  if isExists
    then logError "Makefile already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "Makefile" ) makeFile
      logInfo "Generating Makefile..."