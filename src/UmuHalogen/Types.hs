module UmuHalogen.Types
  ( PathInput
  , ComponentName
  , WriteFileReq
  , WriteDirReq
  , RouteName
  , FileExistence(..)
  , fromPathInput
  , validatePathInput
  , toComponentName
  , fromComponentName
  , defaultWriteFileReq
  , defaultWriteDirReq
  , toRouteName
  , fromRouteName
  , boolToFileExistence
  ) where

import           Import
-- text
import qualified Data.Text        as T
import           Text.Casing      (pascal)
-- filepath
import qualified System.FilePath  as FP
-- umu
import           UmuHalogen.Error

newtype RouteName = RouteName Text
  deriving ( Eq, Show )

newtype ComponentName
  = ComponentName Text
  deriving ( Eq, Show )

newtype PathInput
  = PathInput Text
  deriving ( Eq, Show )

toRouteName :: Text -> RouteName
toRouteName = RouteName . T.pack . pascal . T.unpack

fromRouteName :: RouteName -> Text
fromRouteName ( RouteName txt ) = txt

fromPathInput :: PathInput -> Text
fromPathInput ( PathInput txt ) = txt

validatePathInput :: Text -> Either UmuError PathInput
validatePathInput txt
  | FP.isValid $ T.unpack txt = Right $ PathInput txt
  | otherwise = Left PathInputError

toComponentName :: Text -> ComponentName
toComponentName = ComponentName . T.pack . pascal . T.unpack

fromComponentName :: ComponentName -> Text
fromComponentName ( ComponentName txt ) = txt

data WriteFileReq = WriteFileReq
  { _writeFileReqFilePath :: Text
  , _writeFileReqFile     :: Text
  } deriving ( Eq, Show )

defaultWriteFileReq :: WriteFileReq
defaultWriteFileReq = WriteFileReq
  mempty
  mempty

data WriteDirReq = WriteDirReq
  { _writeDirReqDirName :: Text
  } deriving  ( Eq, Show )

defaultWriteDirReq :: WriteDirReq
defaultWriteDirReq = WriteDirReq mempty

data FileExistence
  = FileExist
  | FileNotExist
  deriving ( Eq, Show )

-- | True will return FileExists, FileNotExist otherwise
boolToFileExistence :: Bool -> FileExistence
boolToFileExistence b = if b then FileExist  else FileNotExist
