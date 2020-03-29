{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties  (PhysicalDeviceShaderCorePropertiesAMD) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceShaderCorePropertiesAMD

instance ToCStruct PhysicalDeviceShaderCorePropertiesAMD
instance Show PhysicalDeviceShaderCorePropertiesAMD

instance FromCStruct PhysicalDeviceShaderCorePropertiesAMD

