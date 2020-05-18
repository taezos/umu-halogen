{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module UmuHalogen.Capability.Generation
  ( ManageGeneration (..)
  , genComponent
  , genProject
  , genRoute
  ) where

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
import           UmuHalogen.Templates
import           UmuHalogen.Types
import           UmuHalogen.Util

class Monad m => ManageGeneration m where
  generateProject :: Maybe Text -> m ()
  generateComponent :: PathInput -> ComponentName -> m ()
  generateRoute :: PathInput -> m ()

genProject
  :: ( MonadIO m, LogMessage m, ManageGeneration m )
  => Maybe Text
  -> m ()
genProject mLoc = case mLoc of
  Nothing -> generateDirectories mLoc
  Just loc -> do
    writeInitialDir loc
    generateDirectories mLoc
    generateFiles mLoc

genComponent
  :: ( MonadIO m, LogMessage m, ManageGeneration m )
  => PathInput
  -> ComponentName
  -> m ()
genComponent pathInput componentName =
  writeComponentFile pathInput componentName

generateDirectories :: ( MonadIO m, LogMessage m ) => Maybe Text -> m ()
generateDirectories mPathInput =
  traverse_ ( umuWriteDirectory mPathInput )
    [ srcDirReq
    , assetDirReq
    , testDirReq
    , componentDirReq
    , pageDirReq
    ]

generateFiles
  :: ( MonadIO m, LogMessage m )
  => Maybe Text
  -> m ()
generateFiles mPathInput = do
  mDefaultDirectory <- defaultDirectory
  traverse_ ( umuWriteFile mPathInput )
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
   ]

-- | generate route and router files
genRoute :: ( MonadIO m, LogMessage m ) => PathInput -> m ()
genRoute pathInput = do
  umuWriteDirectory ( Just $ fromPathInput pathInput ) serviceDirReq
  traverse_
    ( umuWriteFile ( Just $ fromPathInput pathInput ) )
    [ routeFileReq,  navigateFileReq, routerFileReq ]

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
writeInitialDir :: ( MonadIO m, LogMessage m ) => Text -> m ()
writeInitialDir loc = do
  res <- liftIO
    $ tryJust ( guard . isAlreadyExistsError )
    $ TP.mkdir ( Turtle.fromText loc )
  either
    ( const $ logWarn warningMessage )
    ( const $ logInfo $ "Generated " <> loc  )
    res
  where
    warningMessage :: Text
    warningMessage = loc
      <> " already exists but "
      <> appName
      <> " will continue to generate the scaffold in that directory"

umuWriteDirectory
  :: ( MonadIO m, LogMessage m )
  => Maybe Text
  -> WriteDirReq
  -> m ()
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

-----------------------------------------------------------
-- File Generation
-----------------------------------------------------------
umuWriteFile :: ( MonadIO m, LogMessage m ) => Maybe Text -> WriteFileReq -> m ()
umuWriteFile mPathInput wrf = do
  isExists <- isFileExists mPathInput ( wrf ^. writeFileReqFilePath )
  generateWhenFileNotExists
    isExists
    mPathInput
    ( wrf ^. writeFileReqFilePath )
    ( wrf ^. writeFileReqFile )

writeComponentFile :: ( MonadIO m, LogMessage m ) => PathInput -> ComponentName -> m ()
writeComponentFile path componentName = do
  fileExists <- TP.testfile $ Turtle.fromText sanitizedFilePath
  dirExists <- TP.testdir $ Turtle.fromText sanitizedDirPath
  if | fileExists -> logError ( sanitizedFilePath <> " already exists!" )
     | dirExists && not fileExists -> do
         liftIO $ TP.writeTextFile ( Turtle.fromText sanitizedFilePath )
          ( componentTemplate ( fromComponentName componentName ) sanitizedComponentName )
         logInfo ( "Generated " <> sanitizedComponentName <> " component to " <> sanitizedDirPath )
     | otherwise -> logError $ sanitizedDirPath <> " does not exist!"
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

-- writeRouteFile :: ( MonadIO m, LogMessage m ) => PathInput -> m ()
-- writeRouteFile pathInput = do
--   dirExists <- TP.testdir $ Turtle.fromText ( fromPathInput pathInput )
--   if | dirExists -> do
--          liftIO $ TP.writeTextFile
--            ( Turtle.fromText $ sanitizedFilePath <> fileName )
--            ( routeTemplate )
--          logInfo ( "Generated " <> fileName <> " to " <> sanitizedFilePath  )
--      | otherwise -> logError $ sanitizedFilePath <> " does not exists!"
--   where
--     fileName :: Text
--     fileName = "Route.purs"

--     sanitizedComponentName :: Text
--     sanitizedComponentName =
--       fromMaybe mempty
--       $ snd
--       <$> ( discardFirstDot
--           . concatWithDot
--           . filterLower
--           . splitAtPathSeparator
--           . fromPathInput $ pathInput )

--     sanitizedFilePath :: Text
--     sanitizedFilePath = snoc
--       ( fromPathInput pathInput ) FP.pathSeparator

homePageFileReq :: WriteFileReq
homePageFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Page/Home.purs"
  & writeFileReqFile .~ homePageTemplate

routeFileReq :: WriteFileReq
routeFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Service/Route.purs"
  & writeFileReqFile .~ routeTemplate

navigateFileReq :: WriteFileReq
navigateFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Service/Navigate.purs"
  & writeFileReqFile .~ navigateTemplate

routerFileReq :: WriteFileReq
routerFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Component/Router.purs"
  & writeFileReqFile .~ routerComponentTemplate

srcMainFileReq :: WriteFileReq
srcMainFileReq = defaultWriteFileReq
  & writeFileReqFilePath .~ "src/Main.purs"
  & writeFileReqFile .~ srcMainFile

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
