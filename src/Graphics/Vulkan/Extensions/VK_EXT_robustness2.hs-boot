{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_robustness2  ( PhysicalDeviceRobustness2FeaturesEXT
                                                      , PhysicalDeviceRobustness2PropertiesEXT
                                                      ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceRobustness2FeaturesEXT

instance ToCStruct PhysicalDeviceRobustness2FeaturesEXT
instance Show PhysicalDeviceRobustness2FeaturesEXT

instance FromCStruct PhysicalDeviceRobustness2FeaturesEXT


data PhysicalDeviceRobustness2PropertiesEXT

instance ToCStruct PhysicalDeviceRobustness2PropertiesEXT
instance Show PhysicalDeviceRobustness2PropertiesEXT

instance FromCStruct PhysicalDeviceRobustness2PropertiesEXT

