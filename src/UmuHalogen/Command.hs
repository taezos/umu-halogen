{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Command where

import           Data.Version        (showVersion)
import           Import
import           Options.Applicative
import           Paths_umu_halogen   (version)

data Command
  = CommandInit ( Maybe Text )
  deriving ( Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "init" $ parseCommandInit `withInfo` "Initialize scaffold" )

parseCommandInit :: Parser Command
parseCommandInit = CommandInit <$> initParser

initParser :: Parser ( Maybe Text )
initParser = optional $
  argument str ( metavar "LOCATION" <> help "Location to generate scaffold" )

parseVersion :: Parser ( a -> a )
parseVersion =
  infoOption ( concat [ showVersion version ] )
  ( short 'v' <> long "version" <> help "Show version" <> hidden )

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc
