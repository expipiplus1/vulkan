{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shader_atomic_int64  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
                                                     , PhysicalDeviceShaderAtomicInt64FeaturesKHR
                                                     , KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
                                                     , pattern KHR_SHADER_ATOMIC_INT64_SPEC_VERSION
                                                     , KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
                                                     , pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64 (PhysicalDeviceShaderAtomicInt64Features)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceShaderAtomicInt64FeaturesKHR"
type PhysicalDeviceShaderAtomicInt64FeaturesKHR = PhysicalDeviceShaderAtomicInt64Features


type KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_ATOMIC_INT64_SPEC_VERSION"
pattern KHR_SHADER_ATOMIC_INT64_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_ATOMIC_INT64_SPEC_VERSION = 1


type KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME = "VK_KHR_shader_atomic_int64"

-- No documentation found for TopLevel "VK_KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME"
pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME = "VK_KHR_shader_atomic_int64"

