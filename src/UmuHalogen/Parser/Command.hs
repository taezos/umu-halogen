{-# LANGUAGE OverloadedStrings #-}
module UmuHalogen.Parser.Command where

import           Data.Version        (showVersion)
import           Import
import           Options.Applicative
import           Paths_umu_halogen   (version)

data Command
  = CommandInit ( Maybe Text )
  | CommandComponent Path ComponentName
  deriving ( Show )

-- TODO: Refactor to newtype. For now, use Text. The immediate goal is to generate
-- the component.
type Path = Text
type ComponentName = Text

parseCommand :: Parser Command
parseCommand = subparser $
  ( command "init" $ parseCommandInit `withInfo` "Initialize scaffold" )
  <>
  ( command "component" $ parseCommandComponent `withInfo` "Generate a component" )

parseCommandInit :: Parser Command
parseCommandInit = CommandInit <$> initParser
  where
    initParser :: Parser ( Maybe Text )
    initParser = optional $
      argument str ( metavar "LOCATION" <> help "Location to generate scaffold" )

parseCommandComponent :: Parser Command
parseCommandComponent = CommandComponent <$> pathParser <*> nameParser
  where
    pathParser :: Parser Text
    pathParser =
      argument str ( metavar "LOCATION" <> help "Location to generate the component" )

    nameParser :: Parser Text
    nameParser =
      argument str ( metavar "COMPONENT_NAME"  <> help "Name of the component to be generated" )

parseVersion :: Parser ( a -> a )
parseVersion =
  infoOption ( concat [ showVersion version ] )
  ( short 'v' <> long "version" <> help "Show version" <> hidden )

withInfo :: Parser Command -> String -> ParserInfo Command
withInfo opts desc = info ( helper <*> opts ) $ progDesc desc
