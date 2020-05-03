{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_shader_core_properties  (PhysicalDeviceShaderCorePropertiesAMD) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceShaderCorePropertiesAMD

instance ToCStruct PhysicalDeviceShaderCorePropertiesAMD
instance Show PhysicalDeviceShaderCorePropertiesAMD

instance FromCStruct PhysicalDeviceShaderCorePropertiesAMD

