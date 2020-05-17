module UmuHalogen.Types
  ( PathInput
  , ComponentName
  , PathInputError
  , fromPathInput
  , validatePathInput
  , toComponentName
  , fromComponentName
  ) where

import           Import
-- text
import qualified Data.Text       as T
import           Text.Casing     (pascal)
-- filepath
import qualified System.FilePath as FP

newtype ComponentName
  = ComponentName Text
  deriving ( Eq, Show )

newtype PathInput
  = PathInput Text
  deriving ( Eq, Show )

data PathInputError = PathInputError
  deriving ( Eq, Show )

fromPathInput :: PathInput -> Text
fromPathInput ( PathInput txt ) = txt

validatePathInput :: Text -> Either PathInputError PathInput
validatePathInput txt
  | FP.isValid $ T.unpack txt = Right $ PathInput txt
  | otherwise = Left PathInputError

toComponentName :: Text -> ComponentName
toComponentName = ComponentName . T.pack . pascal . T.unpack

fromComponentName :: ComponentName -> Text
fromComponentName ( ComponentName txt ) = txt
