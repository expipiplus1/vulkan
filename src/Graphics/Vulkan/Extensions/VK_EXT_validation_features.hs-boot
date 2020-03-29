{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_validation_features  (ValidationFeaturesEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ValidationFeaturesEXT

instance ToCStruct ValidationFeaturesEXT
instance Show ValidationFeaturesEXT

instance FromCStruct ValidationFeaturesEXT

