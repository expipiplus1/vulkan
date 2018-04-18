module Spec.Platform where

import           Data.Text

data Platform = Platform
  { pName    :: Text
  , pProtect :: Text
  , pComment :: Text
  }
  deriving(Show)
