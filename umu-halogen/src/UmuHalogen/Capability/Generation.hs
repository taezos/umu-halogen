{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}
module UmuHalogen.Capability.Generation
  ( ManageGeneration (..)
  , genProject
  , genRoute
  , genComponent
  , writeComponentFile
  , generateDirectories
  , writeInitialDir
  , generateFiles
  , updateRouteFile
  ) where

import           Control.Exception                      (tryJust)
import           Control.Monad.Except
import           Import

-- lens
import           Control.Lens.Operators

-- text
import qualified Data.Text                              as T

-- filepath
import qualified System.FilePath                        as FP

-- directory
import qualified System.Directory                       as Directory

-- system
import           System.IO.Error                        (isAlreadyExistsError)

-- turtle
import qualified Turtle
import           Turtle.Prelude                         as TP

-- purescript
import           Language.PureScript.CST.Print

-- umu
import           UmuHalogen.Capability.Generation.Route
import           UmuHalogen.Capability.Log
import           UmuHalogen.Error
import           UmuHalogen.Optics
import           UmuHalogen.Parser.Path
import           UmuHalogen.Parser.Route
import           UmuHalogen.Templates
import           UmuHalogen.Types
import           UmuHalogen.Util

class Monad m => ManageGeneration m where
  generateProject
    :: Maybe PathInput
    -> ( Maybe PathInput -> m [ UmuResponse ] )
    -> ( PathInput -> m [ UmuResponse ] )
    -> ( Maybe PathInput -> m [ UmuResponse ] )
    -> m [ UmuResponse ]
  generateComponent
    :: PathInput
    -> ComponentName
    -> ( PathInput -> ComponentName -> m UmuResponse )
    -> m UmuResponse
  generateRoute
    :: PathInput
    -> RouteName
    -> ( PathInput -> RouteName -> Text -> m UmuResponse )
    -> ( Text -> m ( Module () ) )
    -> m UmuResponse

-- | generate project implementation
genProject
  :: ( Monad m, LogMessage m, ManageGeneration m, MonadError UmuError m )
  => Maybe PathInput
  -> ( Maybe PathInput -> m [ UmuResponse ] )
  -> ( PathInput -> m [ UmuResponse ] )
  -> ( Maybe PathInput -> m [ UmuResponse ] )
  -> m [ UmuResponse ]
genProject mPath genDirectoryEff writeInitialDirEff genFilesEff = case mPath of
  Nothing -> genDirectoryEff mPath
  Just loc -> do
    writeDirRes <- writeInitialDirEff loc
    genDirRes <- genDirectoryEff mPath
    genFilesRes <- genFilesEff mPath
    pure $ writeDirRes <> genDirRes <> genFilesRes


-- | generate component implementation
genComponent
  :: Monad m
  => PathInput
  -> ComponentName
  -> ( PathInput -> ComponentName -> m UmuResponse )
  -> m UmuResponse
genComponent pathInput componentName writeEff =
  writeEff pathInput componentName

-- | generate route implementation
genRoute
  :: Monad m
  => PathInput
  -> RouteName
  -> ( PathInput -> RouteName -> Text -> m UmuResponse )
  -> ( Text -> m ( Module () ) )
  -> m UmuResponse
genRoute path routeName updateRouteFileEff parseRouteFileEff = do
  u <- updateRouteModule
    <$> ( parseRouteFileEff ( fromPathInput path ) )
    <*> ( pure $ fromRouteName routeName )
  updateRouteFileEff
    path
    routeName
    ( printModule $ updateRouteCodec ( fromRouteName routeName ) u )

-- | generate directories that runs over IO
generateDirectories
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe PathInput
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

-- | generate files that runs over IO
generateFiles
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe PathInput
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
  => PathInput
  -> m [ UmuResponse ]
writeInitialDir loc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText $ fromPathInput loc )
  either
    ( const $ pure $ [ DirectoryGenerationWarning warningMessage ] )
    ( const $ pure $ [ DirectoryGenerationSuccess ( "Generated " <> fromPathInput loc ) ] )
    res
  where
    warningMessage :: Text
    warningMessage = fromPathInput loc
      <> " already exists but "
      <> appName
      <> " will continue to generate the scaffold in that directory"

-- | writes directory and runs it over IO
umuWriteDirectory
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe PathInput
  -> WriteDirReq
  -> m UmuResponse
umuWriteDirectory mPathInput wdr = do
  dirResHandler ( wdr ^. writeDirReqDirName )
    =<< generateDir ( fromPathInput <$> mPathInput ) ( wdr ^. writeDirReqDirName )

-- | description of the src directory
srcDirReq :: WriteDirReq
srcDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src"

-- | description of asset directory
assetDirReq :: WriteDirReq
assetDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "assets"

-- | description of test directory
testDirReq :: WriteDirReq
testDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "test"

-- | description of component directory
componentDirReq :: WriteDirReq
componentDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Component"

-- | description of page directory
pageDirReq :: WriteDirReq
pageDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Page"

-- | description of service directory
serviceDirReq :: WriteDirReq
serviceDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Service"

-- | description of common directory
commonDirReq :: WriteDirReq
commonDirReq = defaultWriteDirReq
  & writeDirReqDirName .~ "src/Common"

-----------------------------------------------------------
-- File Generation
-----------------------------------------------------------

-- | writes the file when they don't exist, otherwise will return an error.
-- effect that runs on IO
umuWriteFile
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => Maybe PathInput
  -> WriteFileReq
  -> m UmuResponse
umuWriteFile mPathInput wrf = do
  isExists <- isFileExists mPathInput ( wrf ^. writeFileReqFilePath )
  generateWhenFileNotExists
    isExists
    mPathInput
    ( wrf ^. writeFileReqFilePath )
    ( wrf ^. writeFileReqFile )

-- | write the component files and runs it in IO
writeComponentFile
  :: ( MonadIO m, MonadError UmuError m )
  => PathInput
  -> ComponentName
  -> m UmuResponse
writeComponentFile path componentName = do
  fileExists <- TP.testfile $ Turtle.fromText sanitizedFilePath
  dirExists <- TP.testdir $ Turtle.fromText sanitizedDirPath
  if | fileExists -> throwError $ FileGenerationError $ sanitizedFilePath <> " already exists!"
     | dirExists && not fileExists -> case mkModuleName path componentName of
         Left _                    -> throwError PathInputError
         Right componentModuleName -> do
           liftIO $ TP.writeTextFile ( Turtle.fromText sanitizedFilePath ) ( componentTemplate componentName componentModuleName )
           pure $ FileGenerationSuccess $ "Generated " <> componentModuleName <> " component to " <> sanitizedDirPath
     | otherwise -> throwError
       $ DirectoryGenerationError
       $ sanitizedDirPath <> " does not exists!"
  where
    sanitizedDirPath :: Text
    sanitizedDirPath = T.snoc ( fromPathInput path ) FP.pathSeparator

    pursFileName :: Text
    pursFileName = ( fromComponentName componentName ) <> ".purs"

    sanitizedFilePath :: Text
    sanitizedFilePath = T.snoc ( fromPathInput path ) FP.pathSeparator
      <> pursFileName

-- | updates the route file and runs it in IO
updateRouteFile
  :: ( MonadIO m, LogMessage m, MonadError UmuError m )
  => PathInput
  -> RouteName
  -> Text
  -> m UmuResponse
updateRouteFile dirPath routeName file = do
  -- obtain directory path then append the location of the route file.
  -- Which is assumed to be in src/Service/Route.purs
  -- [DIRECTORY_PATH]<>"src/Service/Route.purs"
  fileExistence <- isFileExists ( Just dirPath ) routeFilePath
  case fileExistence of
    FileExist -> do
      liftIO $ TP.writeTextFile ( Turtle.fromText $ T.snoc ( fromPathInput dirPath ) FP.pathSeparator <> routeFilePath ) file
      pure $ FileUpdateSuccess $ "Updated Route.purs with new route " <> fromRouteName routeName
    FileNotExist -> throwError $ UpdateFailed "Failed to update Route.purs"
  where
    routeFilePath :: Text
    routeFilePath = "src/Service/Route.purs"

-- | description of AppM.purs file
appmFileReq :: WriteFileReq
appmFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/AppM.purs"
  & writeFileReqFile .~ appMfile

-- | description of Home.purs file
homePageFileReq :: WriteFileReq
homePageFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Page/Home.purs"
  & writeFileReqFile .~ homePageFile

-- | description of About.purs file
aboutPageFileReq :: WriteFileReq
aboutPageFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Page/About.purs"
  & writeFileReqFile .~ aboutFile

-- | description of Route.purs file
routeFileReq :: WriteFileReq
routeFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Service/Route.purs"
  & writeFileReqFile .~ routeFile

-- | description of Navigate.purs file
navigateFileReq :: WriteFileReq
navigateFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Service/Navigate.purs"
  & writeFileReqFile .~ navigateFile

-- | description of Router.purs file
routerFileReq :: WriteFileReq
routerFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Component/Router.purs"
  & writeFileReqFile .~ routerComponentFile

-- | description of Main.purs fils
srcMainFileReq :: WriteFileReq
srcMainFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Main.purs"
  & writeFileReqFile .~ srcMainFile

-- | description of Util.purs file
commonUtilFileReq :: WriteFileReq
commonUtilFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Common/Util.purs"
  & writeFileReqFile .~ utilFile

-- | description of spago.dhall file
mkSpagoFileReq :: Maybe Text -> Maybe PathInput -> WriteFileReq
mkSpagoFileReq mDirectory mPathInput = defaultWriteFileReq
  & writeFileReqFilePath .~ "spago.dhall"
  & writeFileReqFile .~ spagoTemplateFile
  where
    spagoTemplateFile :: Text
    spagoTemplateFile = spagoTemplate
      $ fromMaybe mempty
      $ flip fromMaybe ( fromPathInput <$> mPathInput )
      <$> mDirectory

-- | description of packages.dhall file
packagesFileReq :: WriteFileReq
packagesFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "packages.dhall"
  & writeFileReqFile .~ packagesDhallFile

-- | description of index.html file
indexHTMLFileReq :: WriteFileReq
indexHTMLFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "assets/index.html"
  & writeFileReqFile .~ indexHtmlFile

-- | description of index.js file
indexJSFileReq :: WriteFileReq
indexJSFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "assets/index.js"
  & writeFileReqFile .~ indexJS

-- | description of Main.purs file
testMainFileReq :: WriteFileReq
testMainFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "test/Main.purs"
  & writeFileReqFile .~ testMainFile

-- | description of Title.purs file
titleComponentFileReq :: WriteFileReq
titleComponentFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Component/Title.purs"
  & writeFileReqFile .~ titleComponentFile

-- | description of package.json file
packageJsonReq :: WriteFileReq
packageJsonReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "package.json"
  & writeFileReqFile .~ packageJsonFile

-- | description of Makefile file
makeFileReq :: WriteFileReq
makeFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "Makefile"
  & writeFileReqFile .~ makeFile

