{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_robustness2"
module Vulkan.Extensions.VK_EXT_robustness2  ( PhysicalDeviceRobustness2FeaturesEXT
                                             , PhysicalDeviceRobustness2PropertiesEXT
                                             ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceRobustness2FeaturesEXT

instance ToCStruct PhysicalDeviceRobustness2FeaturesEXT
instance Show PhysicalDeviceRobustness2FeaturesEXT

instance FromCStruct PhysicalDeviceRobustness2FeaturesEXT


data PhysicalDeviceRobustness2PropertiesEXT

instance ToCStruct PhysicalDeviceRobustness2PropertiesEXT
instance Show PhysicalDeviceRobustness2PropertiesEXT

instance FromCStruct PhysicalDeviceRobustness2PropertiesEXT

