{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Parser.Command where

import           Data.Version        (showVersion)
import           Import
import           Paths_umu_halogen   (version)
-- optsparse-applicative
import           Options.Applicative
-- umu
import           UmuHalogen.Types

data Command
  = CommandInit ( Maybe Text )
  | CommandComponent ( Either PathInputError PathInput ) ComponentName
  | CommandRouter ( Either PathInputError PathInput )
  deriving ( Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "init" $ parseCommandInit `withInfo` "Initialize scaffold" )
  <>
  ( command "component" $ parseCommandComponent `withInfo` "Generate a component" )
  <>
  ( command "router" $ parseCommandRouter `withInfo` "Generate router" )

parseCommandInit :: Parser Command
parseCommandInit = CommandInit <$> initParser
  where
    initParser :: Parser ( Maybe Text )
    initParser = optional $
      argument str ( metavar "LOCATION" <> help "Location to generate scaffold" )

parseCommandComponent :: Parser Command
parseCommandComponent = CommandComponent <$> pathParser <*> nameParser
  where
    pathParser :: Parser ( Either PathInputError PathInput )
    pathParser = argument
      ( validatePathInput <$> str )
      ( metavar "LOCATION" <> help "Location to generate the component" )

    nameParser :: Parser ComponentName
    nameParser = argument
      ( toComponentName <$> str )
      ( metavar "COMPONENT_NAME"  <> help "Name of the component to be generated" )

parseCommandRouter :: Parser Command
parseCommandRouter = CommandRouter <$> routerParser
  where
    routerParser :: Parser ( Either PathInputError PathInput )
    routerParser = argument
      ( validatePathInput <$> str )
      ( metavar "PROJECT_LOCATION" <> help "Location to generate the router" )

parseVersion :: Parser ( a -> a )
parseVersion =
  infoOption ( concat [ showVersion version ] )
  ( short 'v' <> long "version" <> help "Show version" <> hidden )

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc
