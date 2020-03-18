module Spec.Name
  where

import           Relude

newtype CName = CName { unCName :: Text }
  deriving (Show, Eq, Ord, IsString, Hashable)

