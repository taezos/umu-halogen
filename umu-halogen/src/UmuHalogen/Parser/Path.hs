module UmuHalogen.Parser.Path
  ( mkModuleName
  ) where

import           Import
-- text
import qualified Data.Text        as T
-- filepath
import qualified System.FilePath  as FP
-- parsec
import           Text.Parsec      (ParsecT)
import qualified Text.Parsec      as Parsec
-- umu
import           UmuHalogen.Types

-- parses the path input to a module name
-- e.g example/src/Component/Button Blue
-- will result to Component.Button.Blue
mkModuleName :: PathInput -> ComponentName -> Either Parsec.ParseError Text
mkModuleName path componentName =
  -- parse the path content then intercalite with dot
  T.intercalate "." . removeEmptyText <$> Parsec.parse
    ( pathParser $ fromComponentName componentName )
    "( unknown )"
    ( T.unpack $ fromPathInput path )

pathParser :: Text -> ParsecT String st Identity [ Text ]
pathParser txt = do
  -- parsing the items between the path separator that are pascal cased.
  -- eg. example/src/Component/Button will result in [ "Component", "Button" ]
  p <- Parsec.sepBy pathContent ( Parsec.char FP.pathSeparator )
  pure $ T.pack <$> p <> ( singleton $ T.unpack txt )
  where
    singleton :: a -> [a]
    singleton = pure

pathContent :: ParsecT String st Identity String
pathContent = do
  skipLower
  Parsec.many $ Parsec.satisfy ( FP.pathSeparator /= )

skipLower :: ParsecT String st Identity ()
skipLower = Parsec.skipMany Parsec.lower
  >> Parsec.skipMany ( Parsec.oneOf symbols )
  >> Parsec.skipMany Parsec.lower

removeEmptyText :: [ Text ] -> [ Text ]
removeEmptyText = filter ( not . T.null )

symbols :: String
symbols = ":!#$%&*+.<=>?@^|-~"
