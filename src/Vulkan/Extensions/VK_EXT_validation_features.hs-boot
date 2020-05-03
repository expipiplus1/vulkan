{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_validation_features  (ValidationFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ValidationFeaturesEXT

instance ToCStruct ValidationFeaturesEXT
instance Show ValidationFeaturesEXT

instance FromCStruct ValidationFeaturesEXT

