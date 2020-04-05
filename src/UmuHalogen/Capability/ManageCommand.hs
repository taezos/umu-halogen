{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.ManageCommand
  ( genProj
  , ManageCommand (..)
  ) where

import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Import
import           System.Directory     (createDirectory)
import           UmuHalogen.Templates
import           UmuHalogen.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

genProj :: MonadIO m => Maybe Text -> m ()
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

writeSrcDir :: MonadIO m => Maybe Text -> m ()
writeSrcDir mLoc = do
  liftIO $ createDirectory ( T.unpack $ mkPathName mLoc "src" )
  message $ "Generating src..."

writeSrcMainFile :: MonadIO m => Maybe Text -> m ()
writeSrcMainFile mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "/src/Main.purs" ) srcMainFile
  message $ "Generating src/Main.purs..."

writeSpagoFile :: MonadIO m => Maybe Text -> m ()
writeSpagoFile mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "spago.dhall" ) spagoDhallFile
  message $ "Generating spago.dhall..."

writePackagesFile :: MonadIO m => Maybe Text -> m ()
writePackagesFile mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "packages.dhall") packagesDhallFile
  message $ "Generating packages.dhall..."

writeHTMLDir :: MonadIO m => Maybe Text -> m ()
writeHTMLDir mLoc = do
  liftIO $ createDirectory ( T.unpack $ mkPathName mLoc "html" )
  message $ "Generating html..."

writeIndexHTML :: MonadIO m => Maybe Text -> m ()
writeIndexHTML mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "/html/index.html" ) indexHtmlFile
  message $ "Generating html/index.html..."

writeTestDir :: MonadIO m => Maybe Text -> m ()
writeTestDir mLoc = do
  liftIO $ createDirectory ( T.unpack $ mkPathName mLoc "test" )
  message $ "Generating test..."

writeTestMainFile :: MonadIO m => Maybe Text -> m ()
writeTestMainFile mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "/test/Main.purs" ) testMainFile
  message $ "Generating test/Main.purs..."

writeComponentDir :: MonadIO m => Maybe Text -> m ()
writeComponentDir mLoc = do
  liftIO $ createDirectory ( T.unpack $ mkPathName mLoc "/src/Component" )
  message $ "Generating src/Component..."

writeTitleComponentFile :: MonadIO m => Maybe Text -> m ()
writeTitleComponentFile mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "/src/Component/Title.purs" ) titleComponentFile
  message $ "Generating src/Component/Title.purs..."

writePackageJson :: MonadIO m => Maybe Text -> m ()
writePackageJson mLoc = do
  liftIO $ TIO.writeFile ( T.unpack $ mkPathName mLoc "package.json" ) packageJsonFile
  message $ "Generating package.json..."

mkPathName :: Maybe Text -> Text -> Text
mkPathName mLoc fileName =
  maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> fileName
