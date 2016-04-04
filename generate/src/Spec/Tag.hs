module Spec.Tag where

import           Spec.ExtensionTag

data Tag = Tag{ tName    :: ExtensionTag
              , tAuthor  :: String
              , tContact :: String
              }
  deriving(Show)

