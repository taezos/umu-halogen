module UmuHalogen.Error where

import           Import

data UmuError
  = FileGenerationError Text
  | DirectoryGenerationError Text
  | PathInputError
  deriving ( Eq, Show )

umuErrorToText :: UmuError -> Text
umuErrorToText err = case err of
  FileGenerationError msg      -> msg
  DirectoryGenerationError msg -> msg
  PathInputError               -> "PathInputError"

data UmuResponse
  = FileGenerationSuccess Text
  | FileGenerationWarning Text
  | DirectoryGenerationSuccess Text
  | DirectoryGenerationWarning Text
  deriving ( Eq, Show )

umuResponseToText :: UmuResponse -> Text
umuResponseToText res = case res of
  FileGenerationSuccess msg      -> msg
  FileGenerationWarning msg      -> msg
  DirectoryGenerationSuccess msg -> msg
  DirectoryGenerationWarning msg -> msg
