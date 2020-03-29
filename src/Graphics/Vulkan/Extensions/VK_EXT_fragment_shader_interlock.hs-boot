{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_fragment_shader_interlock  (PhysicalDeviceFragmentShaderInterlockFeaturesEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT
instance Show PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance FromCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT

