{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Parser.Command where

import           Data.Version        (showVersion)
import           Import
import           Paths_umu_halogen   (version)
-- optsparse-applicative
import           Options.Applicative
-- umu
import           UmuHalogen.Error
import           UmuHalogen.Types

data Command
  = CommandInit ( Maybe ( Either UmuError PathInput ) )
  | CommandComponent ( Either UmuError PathInput ) ComponentName
  | CommandRoute ( Either UmuError PathInput ) RouteName
  deriving ( Eq, Show )

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "init" $ parseCommandInit `withInfo` "Initialize scaffold" )
  <>
  ( command "component" $ parseCommandComponent `withInfo` "Generate a component" )
  <>
  ( command "route" $ parseCommandRoute `withInfo` "Generate a route" )

parseCommandInit :: Parser Command
parseCommandInit = CommandInit <$> initParser
  where
    initParser :: Parser ( Maybe ( Either UmuError PathInput ) )
    initParser = optional $
      argument ( validatePathInput <$> str ) ( metavar "LOCATION" <> help "Location to generate scaffold" )

parseCommandComponent :: Parser Command
parseCommandComponent = CommandComponent <$> pathParser <*> nameParser
  where
    nameParser :: Parser ComponentName
    nameParser = argument
      ( toComponentName <$> str )
      ( metavar "COMPONENT_NAME"  <> help "Name of the component to be generated" )

parseCommandRoute :: Parser Command
parseCommandRoute = CommandRoute <$> pathParser <*> routeNameParser
  where
    routeNameParser :: Parser RouteName
    routeNameParser = argument
      ( toRouteName <$> str )
      ( metavar "ROUTE_NAME" <> help "Name of the route to be generated" )

pathParser :: Parser ( Either UmuError PathInput )
pathParser = argument
  ( validatePathInput <$> str )
  ( metavar "LOCATION" <> help "Location to generate the component" )

parseVersion :: Parser ( a -> a )
parseVersion =
  infoOption ( concat [ showVersion version ] )
  ( short 'v' <> long "version" <> help "Show version" <> hidden )

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc
