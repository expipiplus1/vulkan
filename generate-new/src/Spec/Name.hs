module Spec.Name
  where

import           Relude

newtype CName = CName { unCName :: Text }
  deriving stock (Eq, Ord)
  deriving newtype (Show, IsString, Hashable)

