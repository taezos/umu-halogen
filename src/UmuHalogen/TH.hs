{-# LANGUAGE TemplateHaskell #-}
module UmuHalogen.TH where

import           ClassyPrelude
import           Data.FileEmbed             (embedFile, makeRelativeToProject)
import           Data.Text.Encoding         as TE
import           Language.Haskell.TH.Syntax (Exp, Q)

embedFileUtf8 :: FilePath -> Q Exp
embedFileUtf8 filePath =
  [| TE.decodeUtf8 $( makeRelativeToProject filePath >>= embedFile ) |]
