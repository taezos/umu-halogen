{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.ManageCommand
  ( genProj
  , ManageCommand (..)
  ) where

import           Import
import           System.IO                        (hSetEncoding, utf8)
import qualified Turtle
import           Turtle.Prelude                   as TP hiding (stderr, stdout)
import           UmuHalogen.Capability.LogMessage
import           UmuHalogen.Templates
import           UmuHalogen.Util


class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

genProj :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
genProj mLoc = do
  liftIO $ hSetEncoding stdout utf8
  liftIO $ hSetEncoding stderr utf8
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
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  logInfo "Generating src..."

writeSrcMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
  logInfo "Generating src/Main.purs..."

writeSpagoFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeSpagoFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
  logInfo "Generating spago.dhall..."

writePackagesFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackagesFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "packages.dhall") packagesDhallFile
  logInfo "Generating packages.dhall..."

writeHTMLDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeHTMLDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "html" )
  logInfo "Generating html..."

writeIndexHTML :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeIndexHTML mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/html/index.html" ) indexHtmlFile
  logInfo "Generating html/index.html..."

writeTestDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  logInfo "Generating test..."

writeTestMainFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTestMainFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/test/Main.purs" ) testMainFile
  logInfo "Generating test/Main.purs..."

writeComponentDir :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeComponentDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "/src/Component" )
  logInfo "Generating src/Component..."

writeTitleComponentFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/src/Component/Title.purs" ) titleComponentFile
  logInfo "Generating src/Component/Title.purs..."

writePackageJson :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writePackageJson mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "package.json" ) packageJsonFile
  logInfo "Generating package.json..."

writeMakeFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
writeMakeFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "Makefile" ) makeFile
  logInfo "Generating Makefile..."

