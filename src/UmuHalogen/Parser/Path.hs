module UmuHalogen.Parser.Path
  ( mkModuleName
  ) where

import           Import
-- text
import qualified Data.Text           as T
-- filepath
import qualified System.FilePath     as FP
-- parsec
import           Text.Parsec         (ParsecT)
import qualified Text.Parsec         as Parsec
-- purescript
import           Language.PureScript (ModuleName (..), ProperName (..),
                                      runModuleName)
-- umu
import           UmuHalogen.Types

-- parses the path input to a module name
-- e.g example/src/Component/Button Blue
-- will result to Component.Button.Blue
mkModuleName :: PathInput -> ComponentName -> Either Parsec.ParseError Text
mkModuleName path componentName =
  runModuleName <$> ModuleName . filterEmptyProperName <$> Parsec.parse
    ( pathParser $ fromComponentName componentName ) "( unknown )"
    ( T.unpack $ fromPathInput path )

pathParser :: Text -> ParsecT String st Identity [ ProperName a ]
pathParser txt = do
  p <- Parsec.sepBy pathContent ( Parsec.char FP.pathSeparator )
  pure
    $ ( ProperName . T.pack ) <$> ( p <> ( singleton $ T.unpack txt ) )
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

-- skipping unwanted characters returns an empty string and it results to
-- ProperName [{ runProperName = "" }]. This will filter out those empty
-- ProperNames.
filterEmptyProperName :: [ ProperName a ] -> [ ProperName a ]
filterEmptyProperName = filter (\pName -> ( runProperName pName ) /= "" )

symbols :: String
symbols = ":!#$%&*+.<=>?@^|-~"
