{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_tooling_info  (PhysicalDeviceToolPropertiesEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceToolPropertiesEXT

instance ToCStruct PhysicalDeviceToolPropertiesEXT
instance Show PhysicalDeviceToolPropertiesEXT

instance FromCStruct PhysicalDeviceToolPropertiesEXT

