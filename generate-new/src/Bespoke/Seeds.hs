module Bespoke.Seeds
  ( ModulePlacement(..)
  ) where

import           Relude
import           Data.Version

data ModulePlacement
  = CoreMod Version Text
  | ExtensionMod Text
  | BespokeMod Text
  deriving(Show)

