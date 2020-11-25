{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_shader_float16_int8"
module Vulkan.Extensions.VK_KHR_shader_float16_int8  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES_KHR
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
                                                     , PhysicalDeviceShaderFloat16Int8FeaturesKHR
                                                     , PhysicalDeviceFloat16Int8FeaturesKHR
                                                     , KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
                                                     , pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION
                                                     , KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
                                                     , pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8 (PhysicalDeviceShaderFloat16Int8Features)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderFloat16Int8FeaturesKHR"
type PhysicalDeviceShaderFloat16Int8FeaturesKHR = PhysicalDeviceShaderFloat16Int8Features


-- No documentation found for TopLevel "VkPhysicalDeviceFloat16Int8FeaturesKHR"
type PhysicalDeviceFloat16Int8FeaturesKHR = PhysicalDeviceShaderFloat16Int8Features


type KHR_SHADER_FLOAT16_INT8_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_SPEC_VERSION"
pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_FLOAT16_INT8_SPEC_VERSION = 1


type KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME = "VK_KHR_shader_float16_int8"

-- No documentation found for TopLevel "VK_KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME"
pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME = "VK_KHR_shader_float16_int8"

