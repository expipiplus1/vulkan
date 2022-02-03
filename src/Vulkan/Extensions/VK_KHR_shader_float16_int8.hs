{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_float16_int8 - device extension
--
-- == VK_KHR_shader_float16_int8
--
-- [__Name String__]
--     @VK_KHR_shader_float16_int8@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     83
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Alexander Galazin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_float16_int8] @alegal-arm%0A<<Here describe the issue or question you have about the VK_KHR_shader_float16_int8 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-03-07
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension interacts with @VK_KHR_8bit_storage@
--
--     -   This extension interacts with @VK_KHR_16bit_storage@
--
--     -   This extension interacts with @VK_KHR_shader_float_controls@
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_shader_explicit_arithmetic_types.txt GL_EXT_shader_explicit_arithmetic_types>
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alexander Galazin, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Graeme Leese, Broadcom
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- The @VK_KHR_shader_float16_int8@ extension allows use of 16-bit
-- floating-point types and 8-bit integer types in shaders for arithmetic
-- operations.
--
-- It introduces two new optional features @shaderFloat16@ and @shaderInt8@
-- which directly map to the @Float16@ and the @Int8@ SPIR-V capabilities.
-- The @VK_KHR_shader_float16_int8@ extension also specifies precision
-- requirements for half-precision floating-point SPIR-V operations. This
-- extension does not enable use of 8-bit integer types or 16-bit
-- floating-point types in any
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-iointerfaces shader input and output interfaces>
-- and therefore does not supersede the @VK_KHR_8bit_storage@ or
-- @VK_KHR_16bit_storage@ extensions.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, both the @shaderFloat16@ and @shaderInt8@ capabilities
-- are optional. The original type, enum and command names are still
-- available as aliases of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFloat16Int8FeaturesKHR'
--
--     -   'PhysicalDeviceShaderFloat16Int8FeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_FLOAT16_INT8_EXTENSION_NAME'
--
-- -   'KHR_SHADER_FLOAT16_INT8_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-03-07 (Alexander Galazin)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceFloat16Int8FeaturesKHR',
-- 'PhysicalDeviceShaderFloat16Int8FeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_float16_int8 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

