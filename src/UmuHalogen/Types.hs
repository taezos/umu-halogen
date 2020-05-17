module UmuHalogen.Types
  ( PathInput
  , ComponentName
  , PathInputError
  , WriteFileReq
  , WriteDirReq
  , fromPathInput
  , validatePathInput
  , toComponentName
  , fromComponentName
  , defaultWriteFileReq
  , defaultWriteDirReq
  , writeFileReqFilePath
  , writeFileReqFile
  , writeDirReqDirName
  ) where

import           Import
-- lens
import           Lens.Micro
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

-----------------------------------------------------------
-- lens
-----------------------------------------------------------
writeFileReqFilePath :: Lens' WriteFileReq Text
writeFileReqFilePath fn wrf@WriteFileReq{ _writeFileReqFilePath = filePath } =
  fn filePath <&> \newFilePath -> wrf { _writeFileReqFilePath = newFilePath }

writeFileReqFile :: Lens' WriteFileReq Text
writeFileReqFile fn wrf@WriteFileReq{ _writeFileReqFile = file } =
  fn file <&> \newFile -> wrf { _writeFileReqFile = newFile }

writeDirReqDirName :: Lens' WriteDirReq Text
writeDirReqDirName fn wdr@WriteDirReq{ _writeDirReqDirName = dirName } =
  fn dirName <&>  \newDirName -> wdr { _writeDirReqDirName = newDirName }
