{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Capability.ManageCommand where

import qualified Data.Text        as T
import           Import
import           System.Directory (createDirectory)

class Monad m => ManageCommand m where
  generateProject :: Maybe Text -> m ()

instance ManageCommand IO where
  generateProject = liftIO . generateProject

createSpagoFile :: MonadIO m => Maybe Text -> m ()
createSpagoFile mLoc = do
  let pathName = maybe "./" (\loc -> "./" <> loc <> "/") mLoc <> "spago.dhall"
  liftIO $ createDirectory ( T.unpack pathName )
