{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UmuHalogen where

import           Import
import           Options.Applicative
import           UmuHalogen.Capability.ManageCommand
newtype AppM m a
  = AppM
  { unAppM :: m a
  } deriving ( Functor, Applicative, Monad, MonadIO )

runAppM :: MonadIO m => AppM m a -> m a
runAppM app = unAppM app

-- startApp ;: IO ()
-- startApp = do
--   comm <- showHelpOnErrorExecParser
--     ( info ( helper <*> par))

instance MonadIO m => ManageCommand ( AppM m ) where
  generateProject mLoc = do
    createSpagoFile mLoc

showHelponErrorExecParser :: ParserInfo a -> IO a
showHelponErrorExecParser = customExecParser ( prefs showHelpOnError )
