{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UmuHalogen.Capability.Generation
  ( ManageGeneration (..)
  , genComponent
  , genProject
  -- , genRoute
  ) where

import           Control.Monad.Except
import           Import
-- lens
import           Lens.Micro
-- text
import qualified Data.Text                 as T
-- filepath
import qualified System.FilePath           as FP
-- directory
import qualified System.Directory          as Directory
-- turtle
import qualified Turtle
import           Turtle.Prelude            as TP
-- umu
import           UmuHalogen.Capability.Log
import           UmuHalogen.Error
import           UmuHalogen.Templates
import           UmuHalogen.Types
import           UmuHalogen.Util

class Monad m => ManageGeneration m where
  generateProject :: Maybe Text -> m [ UmuResponse ]
  generateComponent :: PathInput -> ComponentName -> m UmuResponse
  -- generateRoute :: PathInput -> m ()

-- | generate project implementation
genProject
  :: ( MonadIO m, LogMessage m, ManageGeneration m, MonadError UmuError m )
  => Maybe Text
  -> m [ UmuResponse ]
genProject mPath = case mPath of
  Nothing -> generateDirectories mPath
  Just loc -> do
    writeDirRes <- writeInitialDir loc
    genDirRes <- generateDirectories mPath
    genFilesRes <- generateFiles mPath
    pure $ writeDirRes <> genDirRes <> genFilesRes

-- | generate component implementation
genComponent
  :: ( MonadIO m, LogMessage m, ManageGeneration m, MonadError UmuError m )
  => PathInput
  -> ComponentName
  -> m UmuResponse
genComponent pathInput componentName =
  writeComponentFile pathInput componentName

generateDirectories
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe Text
  -> m [ UmuResponse ]
generateDirectories mPathInput =
  traverse ( umuWriteDirectory mPathInput )
    [ srcDirReq
    , assetDirReq
    , testDirReq
    , componentDirReq
    , pageDirReq
    , serviceDirReq
    , commonDirReq
    ]

generateFiles
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe Text
  -> m [ UmuResponse ]
generateFiles mPathInput = do
  mDefaultDirectory <- defaultDirectory
  traverse ( umuWriteFile mPathInput )
    [ srcMainFileReq
    , mkSpagoFileReq mDefaultDirectory mPathInput
    , packagesFileReq
    , indexHTMLFileReq
    , indexJSFileReq
    , testMainFileReq
    , titleComponentFileReq
    , homePageFileReq
    , packageJsonReq
    , makeFileReq
    , routeFileReq
    , navigateFileReq
    , routerFileReq
    , appmFileReq
    , aboutPageFileReq
    , commonUtilFileReq
    ]

-- | Used when there is no directory input. It will retreive the directory name
-- where the project is generated.
defaultDirectory :: MonadIO m => m ( Maybe Text )
defaultDirectory = liftIO
  $ listToMaybe
  . reverse
  . T.split ( == FP.pathSeparator )
  . T.pack
  <$> Directory.getCurrentDirectory

-----------------------------------------------------------
-- Directory Generation
-----------------------------------------------------------
writeInitialDir
  :: ( MonadIO m, MonadError UmuError m, LogMessage m )
  => Text
  -> m [ UmuResponse ]
writeInitialDir loc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText loc )
  either
    ( const $ pure $ [ DirectoryGenerationWarning warningMessage ] )
    ( const $ pure $ [ DirectoryGenerationSuccess ( "Generated " <> loc ) ] )
    res
  where
    warningMessage :: Text
    warningMessage = loc
      <> " already exists but "
      <> appName
      <> " will continue to generate the scaffold in that directory"

umuWriteDirectory
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe Text
  -> WriteDirReq
  -> m UmuResponse
umuWriteDirectory mPathInput wdr = do
  dirResHandler ( wdr ^. writeDirReqDirName )
    =<< generateDir mPathInput ( wdr ^. writeDirReqDirName )

srcDirReq :: WriteDirReq
srcDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src"

assetDirReq :: WriteDirReq
assetDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "assets"

testDirReq :: WriteDirReq
testDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "test"

componentDirReq :: WriteDirReq
componentDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Component"

pageDirReq :: WriteDirReq
pageDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Page"

serviceDirReq :: WriteDirReq
serviceDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Service"

commonDirReq :: WriteDirReq
commonDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Common"
-----------------------------------------------------------
-- File Generation
-----------------------------------------------------------
umuWriteFile
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe Text
  -> WriteFileReq
  -> m UmuResponse
umuWriteFile mPathInput wrf = do
  isExists <- isFileExists mPathInput ( wrf ^. writeFileReqFilePath )
  generateWhenFileNotExists
    isExists
    mPathInput
    ( wrf ^. writeFileReqFilePath )
    ( wrf ^. writeFileReqFile )

writeComponentFile
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => PathInput
  -> ComponentName
  -> m UmuResponse
writeComponentFile path componentName = do
  fileExists <- TP.testfile $ Turtle.fromText sanitizedFilePath
  dirExists <- TP.testdir $ Turtle.fromText sanitizedDirPath
  if | fileExists -> throwError $ FileGenerationError $ sanitizedFilePath <> " alread exists!"
     | dirExists && not fileExists -> do
         liftIO $ TP.writeTextFile ( Turtle.fromText sanitizedFilePath )
           ( componentTemplate ( fromComponentName componentName ) sanitizedComponentName )
         pure $ FileGenerationSuccess $ "Genereated " <> sanitizedComponentName <> " component to " <> sanitizedDirPath
     | otherwise -> throwError $ DirectoryGenerationError $ sanitizedDirPath <> " does not exists!"
  where
    sanitizedComponentName :: Text
    sanitizedComponentName =
      maybe mempty ( <> "." <> ( fromComponentName componentName ))
      $ snd
      <$> ( discardFirstDot
          . concatWithDot
          . filterLower
          . splitAtPathSeparator
          . fromPathInput $ path )

    sanitizedDirPath :: Text
    sanitizedDirPath = snoc ( fromPathInput path ) FP.pathSeparator

    pursFileName :: Text
    pursFileName = ( fromComponentName componentName ) <> ".purs"

    sanitizedFilePath :: Text
    sanitizedFilePath = snoc ( fromPathInput path ) FP.pathSeparator
      <> pursFileName

appmFileReq :: WriteFileReq
appmFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/AppM.purs"
  & writeFileReqFile .~ appMfile

homePageFileReq :: WriteFileReq
homePageFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Page/Home.purs"
  & writeFileReqFile .~ homePageFile

aboutPageFileReq :: WriteFileReq
aboutPageFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Page/About.purs"
  & writeFileReqFile .~ aboutFile

routeFileReq :: WriteFileReq
routeFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Service/Route.purs"
  & writeFileReqFile .~ routeFile

navigateFileReq :: WriteFileReq
navigateFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Service/Navigate.purs"
  & writeFileReqFile .~ navigateFile

routerFileReq :: WriteFileReq
routerFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Component/Router.purs"
  & writeFileReqFile .~ routerComponentFile

srcMainFileReq :: WriteFileReq
srcMainFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Main.purs"
  & writeFileReqFile .~ srcMainFile

commonUtilFileReq :: WriteFileReq
commonUtilFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Common/Util.purs"
  & writeFileReqFile .~ utilFile

mkSpagoFileReq :: Maybe Text -> Maybe Text -> WriteFileReq
mkSpagoFileReq mDirectory mPathInput = defaultWriteFileReq
  & writeFileReqFilePath .~ "spago.dhall"
  & writeFileReqFile .~ spagoTemplateFile
  where
    spagoTemplateFile :: Text
    spagoTemplateFile = spagoTemplate
      $ fromMaybe mempty
      $ flip fromMaybe mPathInput
      <$> mDirectory

packagesFileReq :: WriteFileReq
packagesFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "packages.dhall"
  & writeFileReqFile .~ packagesDhallFile

indexHTMLFileReq :: WriteFileReq
indexHTMLFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "assets/index.html"
  & writeFileReqFile .~ indexHtmlFile

indexJSFileReq :: WriteFileReq
indexJSFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "assets/index.js"
  & writeFileReqFile .~ indexJS

testMainFileReq :: WriteFileReq
testMainFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "test/Main.purs"
  & writeFileReqFile .~ testMainFile

titleComponentFileReq :: WriteFileReq
titleComponentFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Component/Title.purs"
  & writeFileReqFile .~ titleComponentFile

packageJsonReq :: WriteFileReq
packageJsonReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "package.json"
  & writeFileReqFile .~ packageJsonFile

makeFileReq :: WriteFileReq
makeFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "Makefile"
  & writeFileReqFile .~ makeFile
