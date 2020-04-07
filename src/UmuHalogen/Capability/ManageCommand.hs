{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.ManageCommand
  ( genProj
  , ManageCommand (..)
  ) where

import           Import
-- Turtle
import qualified Turtle
import           Turtle.Prelude                   as TP
-- Umu
import           UmuHalogen.Capability.LogMessage
import           UmuHalogen.Templates
import           UmuHalogen.Util


class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

genProj :: ( MonadIO m, LogMessage m, ManageCommand m ) => Maybe Text -> m ()
genProj mLoc = do
  writeSrcDir mLoc
  writeSrcMainFile mLoc
  writeSpagoFile mLoc
  writePackagesFile mLoc
  writeHTMLDir mLoc
  writeIndexHTML mLoc
  writeTestDir mLoc
  writeTestMainFile mLoc
  writeComponentDir mLoc
  writeTitleComponentFile mLoc
  writePackageJson mLoc
  writeMakeFile mLoc

writeSrcDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  either
    ( const $ logError "src directory already exists!" )
    ( const $ logInfo "Generating src..." )
    res

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

writeHTMLDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeHTMLDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "html" )
  either
    ( const $ logError "html directory already exists!" )
    ( const $ logInfo "Generating html..." )
    res

writeIndexHTML :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHTML mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "html/index.html"
  if isExists
    then logError "html/index.html already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "html/index.html" ) indexHtmlFile
      logInfo "Generating html/index.html..."

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  either
    ( const $ logError "test directory already exists!" )
    ( const $ logInfo "Generating test..." )
    res

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mLoc = do
  isExists <- TP.testfile $ Turtle.fromText $ mkPathName mLoc "test/Main.purs"
  if isExists
    then logError "test/Main.purs already exists!"
    else do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "test/Main.purs" ) testMainFile
      logInfo "Generating test/Main.purs..."

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src/Component" )
  either
    ( const $ logError "src/Component already exists!" )
    ( const $ logInfo "Generating src/Component..." )
    res

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
