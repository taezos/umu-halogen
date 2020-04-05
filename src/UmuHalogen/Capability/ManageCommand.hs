{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.ManageCommand
  ( genProj
  , ManageCommand (..)
  ) where

import           Import
import           System.IO            (hSetEncoding, utf8)
import qualified Turtle
import           Turtle.Prelude       as TP hiding (stderr, stdout)
import           UmuHalogen.Templates
import           UmuHalogen.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

genProj :: MonadIO m => Maybe Text -> m ()
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

writeSrcDir :: MonadIO m => Maybe Text -> m ()
writeSrcDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "src" )
  message $ "Generating src..."

writeSrcMainFile :: MonadIO m => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
  message $ "Generating src/Main.purs..."

writeSpagoFile :: MonadIO m => Maybe Text -> m ()
writeSpagoFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
  message $ "Generating spago.dhall..."

writePackagesFile :: MonadIO m => Maybe Text -> m ()
writePackagesFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "packages.dhall") packagesDhallFile
  message $ "Generating packages.dhall..."

writeHTMLDir :: MonadIO m => Maybe Text -> m ()
writeHTMLDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "html" )
  message $ "Generating html..."

writeIndexHTML :: MonadIO m => Maybe Text -> m ()
writeIndexHTML mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/html/index.html" ) indexHtmlFile
  message $ "Generating html/index.html..."

writeTestDir :: MonadIO m => Maybe Text -> m ()
writeTestDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "test" )
  message $ "Generating test..."

writeTestMainFile :: MonadIO m => Maybe Text -> m ()
writeTestMainFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/test/Main.purs" ) testMainFile
  message $ "Generating test/Main.purs..."

writeComponentDir :: MonadIO m => Maybe Text -> m ()
writeComponentDir mLoc = do
  liftIO $ TP.mkdir ( Turtle.fromText $ mkPathName mLoc "/src/Component" )
  message $ "Generating src/Component..."

writeTitleComponentFile :: MonadIO m => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "/src/Component/Title.purs" ) titleComponentFile
  message $ "Generating src/Component/Title.purs..."

writePackageJson :: MonadIO m => Maybe Text -> m ()
writePackageJson mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "package.json" ) packageJsonFile
  message $ "Generating package.json..."

writeMakeFile :: MonadIO m => Maybe Text -> m ()
writeMakeFile mLoc = do
  liftIO $ TP.writeTextFile ( Turtle.fromText $ mkPathName mLoc "Makefile" ) makeFile
  message $ "Generating Makefile..."

mkPathName :: Maybe Text -> Text -> Text
mkPathName mLoc fileName =
  maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> fileName
