{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_uniform_buffer_unsized_array - device extension
--
-- = VK_EXT_shader_uniform_buffer_unsized_array
--
-- [__Name String__]
--     @VK_EXT_shader_uniform_buffer_unsized_array@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     643
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_uniform_buffer_unsized_array] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_shader_uniform_buffer_unsized_array extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_uniform_buffer_unsized_array.adoc VK_EXT_shader_uniform_buffer_unsized_array>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-28
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pranjal Dubey, NVIDIA
--
-- == Description
--
-- This extension allows the last member of a uniform buffer block to be
-- declared as an unsized array. This capability enables applications to
-- create flexible buffer layouts where the array size can vary based on
-- runtime requirements.
--
-- When the last member of a uniform buffer block is declared as an unsized
-- array, the effective array size is inferred at runtime from the size of
-- the buffer object backing the uniform buffer block. Such unsized arrays
-- can be indexed with general integer expressions, but may not be passed
-- as arguments to functions or indexed with negative constant expressions.
--
-- This extension leverages existing SPIR-V capabilities, allowing use of
-- @OpTypeRuntimeArray@ as the last member of a uniform buffer block
-- structure while prohibiting @OpArrayLength@.
--
-- Applications needing to know array sizes at runtime should calculate the
-- length and pass it to shaders via a separate uniform. This calculation
-- can be performed using the formula: max((buffer_object_size -
-- offset_of_array) \/ stride_of_array, 0), where buffer_object_size is the
-- size of the bound buffer, offset_of_array is the byte offset of the
-- array in the block, and stride_of_array is the byte stride between
-- consecutive array elements.
--
-- Uniform buffers have traditionally required explicit sizes for all
-- arrays which limits flexibility. With this extension, developers can
-- create a single shader that adapts to different data set sizes at
-- runtime by binding differently sized buffers.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_UNIFORM_BUFFER_UNSIZED_ARRAY_EXTENSION_NAME'
--
-- -   'EXT_SHADER_UNIFORM_BUFFER_UNSIZED_ARRAY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_UNIFORM_BUFFER_UNSIZED_ARRAY_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-05-28 (Pranjal Dubey)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_uniform_buffer_unsized_array Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_uniform_buffer_unsized_array  (PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT

instance ToCStruct PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT
instance Show PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT

instance FromCStruct PhysicalDeviceShaderUniformBufferUnsizedArrayFeaturesEXT

