{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_custom_border_color"
module Vulkan.Extensions.VK_EXT_custom_border_color  ( PhysicalDeviceCustomBorderColorFeaturesEXT
                                                     , PhysicalDeviceCustomBorderColorPropertiesEXT
                                                     , SamplerCustomBorderColorCreateInfoEXT
                                                     ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceCustomBorderColorFeaturesEXT

instance ToCStruct PhysicalDeviceCustomBorderColorFeaturesEXT
instance Show PhysicalDeviceCustomBorderColorFeaturesEXT

instance FromCStruct PhysicalDeviceCustomBorderColorFeaturesEXT


data PhysicalDeviceCustomBorderColorPropertiesEXT

instance ToCStruct PhysicalDeviceCustomBorderColorPropertiesEXT
instance Show PhysicalDeviceCustomBorderColorPropertiesEXT

instance FromCStruct PhysicalDeviceCustomBorderColorPropertiesEXT


data SamplerCustomBorderColorCreateInfoEXT

instance ToCStruct SamplerCustomBorderColorCreateInfoEXT
instance Show SamplerCustomBorderColorCreateInfoEXT

