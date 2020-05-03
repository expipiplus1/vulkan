{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_tooling_info  (PhysicalDeviceToolPropertiesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceToolPropertiesEXT

instance ToCStruct PhysicalDeviceToolPropertiesEXT
instance Show PhysicalDeviceToolPropertiesEXT

instance FromCStruct PhysicalDeviceToolPropertiesEXT

