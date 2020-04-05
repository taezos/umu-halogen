{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.ManageCommand where

import qualified Data.Text        as T
import qualified Data.Text.IO     as TIO
import           Import
import           System.Directory (createDirectory)
import           UmuHalogen.Util

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

writeSrc :: MonadIO m => Maybe Text -> m ()
writeSrc mLoc = do
  let pathName = maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> "src"
  message $ "Generating src..."
  liftIO $ createDirectory ( T.unpack pathName )

writeSpagoFile :: MonadIO m => Maybe Text -> m ()
writeSpagoFile mLoc = do
  let pathName = maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> "spago.dhall"
  spagoFile <- liftIO $ TIO.readFile "./templates/spago.dhall"
  liftIO $ TIO.writeFile ( T.unpack pathName ) spagoFile
  message $ "Generating spago.dhall..."

writePackagesFile :: MonadIO m => Maybe Text -> m ()
writePackagesFile mLoc = do
  let pathName = maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> "packages.dhall"
  packagesFile <- liftIO $ TIO.readFile "./templates/packages.dhall"
  liftIO $ TIO.writeFile ( T.unpack pathName ) packagesFile
  message $ "Generating packages.dhall..."
