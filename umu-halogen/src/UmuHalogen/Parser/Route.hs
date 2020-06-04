{-# LANGUAGE FlexibleContexts #-}
module UmuHalogen.Parser.Route
  ( parseRouteFile
  ) where

import           Control.Monad.Except
import           Import
-- filepath
import qualified System.FilePath                as FP
-- text
import qualified Data.Text                      as T
-- turtle
import qualified Turtle
import qualified Turtle.Prelude                 as TP
-- umu
import           UmuHalogen.Error
-- purescript
import qualified Language.PureScript.CST.Parser as CST
import           Language.PureScript.CST.Types

-- | takes in directory path then searches src/Service/Route.hs
-- [DIRECTORY_PATH] <> src/Service/Route.hs is the assumed full path.
parseRouteFile :: ( MonadIO m, MonadError UmuError m ) => Text -> m ( Module () )
parseRouteFile projectDir = do
  file <- liftIO $ TP.readTextFile ( Turtle.fromText $ T.snoc projectDir FP.pathSeparator <> routeFile )
  either
    ( throwError . ParseError . show )
    pure
    ( snd $ CST.parse file )
  where
    routeFile :: Text
    routeFile = "src/Service/Route.purs"
