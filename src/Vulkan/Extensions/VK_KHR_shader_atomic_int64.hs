{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_atomic_int64 - device extension
--
-- == VK_KHR_shader_atomic_int64
--
-- [__Name String__]
--     @VK_KHR_shader_atomic_int64@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     181
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
--     -   Aaron Hagan
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_shader_atomic_int64:%20&body=@ahagan%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension enables
--         <https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_gpu_shader_int64.txt GL_ARB_gpu_shader_int64>
--         and
--         <https://raw.githubusercontent.com/KhronosGroup/GLSL/master/extensions/ext/GL_EXT_shader_atomic_int64.txt GL_EXT_shader_atomic_int64>
--         for GLSL source languages.
--
-- [__Contributors__]
--
--     -   Aaron Hagan, AMD
--
--     -   Daniel Rakos, AMD
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Neil Henning, Codeplay
--
-- == Description
--
-- This extension advertises the SPIR-V __Int64Atomics__ capability for
-- Vulkan, which allows a shader to contain 64-bit atomic operations on
-- signed and unsigned integers. The supported operations include
-- OpAtomicMin, OpAtomicMax, OpAtomicAnd, OpAtomicOr, OpAtomicXor,
-- OpAtomicAdd, OpAtomicExchange, and OpAtomicCompareExchange.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the @shaderBufferInt64Atomics@ capability is optional.
-- The original type, enum and command names are still available as aliases
-- of the core functionality.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicInt64FeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_ATOMIC_INT64_EXTENSION_NAME'
--
-- -   'KHR_SHADER_ATOMIC_INT64_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-Int64Atomics Int64Atomics>
--
-- == Version History
--
-- -   Revision 1, 2018-07-05 (Aaron Hagan)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'PhysicalDeviceShaderAtomicInt64FeaturesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_atomic_int64 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

