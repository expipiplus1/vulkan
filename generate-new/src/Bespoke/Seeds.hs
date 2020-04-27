module Bespoke.Seeds
  ( ModulePlacement(..)
  ) where

import           Data.Version
import           Relude

data ModulePlacement
  = CoreMod Version Text
  | ExtensionMod Text
  | BespokeMod Text
  deriving(Show)

