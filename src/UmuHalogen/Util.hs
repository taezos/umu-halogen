{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Util
  ( mkPathName
  , isFileExists
  ) where

import           Import
import qualified Turtle
import qualified Turtle.Prelude as TP

mkPathName :: Maybe Text -> Text -> Text
mkPathName mPathInput fileName =
  -- If the user doesn't provide mPathInput, generate in the current directory.
  maybe "./" (\loc -> "./" <> loc <> "/") mPathInput <> fileName

isFileExists :: MonadIO m => Maybe Text -> Text -> m Bool
isFileExists mPathInput filePath =
  TP.testfile $ Turtle.fromText ( mkPathName mPathInput filePath )
