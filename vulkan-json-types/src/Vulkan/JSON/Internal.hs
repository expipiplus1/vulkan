module Vulkan.JSON.Internal
  ( vkAesonOptions
  , dropTrailingTick
  ) where

import           Data.Aeson.TH (Options (..), defaultOptions)

vkAesonOptions :: Options
vkAesonOptions = defaultOptions
  { fieldLabelModifier = dropTrailingTick
  , omitNothingFields  = True
  }

dropTrailingTick :: String -> String
dropTrailingTick s = case reverse s of
  '\'' : rest -> reverse rest
  _           -> s
