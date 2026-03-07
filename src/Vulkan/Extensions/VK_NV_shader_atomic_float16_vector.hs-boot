{-# language CPP #-}
-- | = Name
--
-- VK_NV_shader_atomic_float16_vector - device extension
--
-- = VK_NV_shader_atomic_float16_vector
--
-- [__Name String__]
--     @VK_NV_shader_atomic_float16_vector@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     564
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_shader_atomic_fp16_vector.html SPV_NV_shader_atomic_fp16_vector>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_shader_atomic_float16_vector] @jeffbolznv%0A*Here describe the issue or question you have about the VK_NV_shader_atomic_float16_vector extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-03
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://registry.khronos.org/OpenGL/extensions/NV/NV_shader_atomic_fp16_vector.txt GL_NV_shader_atomic_fp16_vector>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows a shader to perform atomic add, min, max, and
-- exchange operations on 2- and 4-component vectors of float16. Buffer,
-- workgroup, and image storage classes are all supported.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_SHADER_ATOMIC_FLOAT16_VECTOR_EXTENSION_NAME'
--
-- -   'NV_SHADER_ATOMIC_FLOAT16_VECTOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_FLOAT16_VECTOR_FEATURES_NV'
--
-- == Issues
--
-- None.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-AtomicFloat16VectorNV AtomicFloat16VectorNV>
--
-- == Version History
--
-- -   Revision 1, 2024-02-03 (Jeff Bolz)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_shader_atomic_float16_vector Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_shader_atomic_float16_vector  (PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV

instance ToCStruct PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV
instance Show PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV

instance FromCStruct PhysicalDeviceShaderAtomicFloat16VectorFeaturesNV

