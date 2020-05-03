{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_fragment_shader_interlock  (PhysicalDeviceFragmentShaderInterlockFeaturesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance ToCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT
instance Show PhysicalDeviceFragmentShaderInterlockFeaturesEXT

instance FromCStruct PhysicalDeviceFragmentShaderInterlockFeaturesEXT

