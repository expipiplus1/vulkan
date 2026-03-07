{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_64bit_indexing - device extension
--
-- = VK_EXT_shader_64bit_indexing
--
-- [__Name String__]
--     @VK_EXT_shader_64bit_indexing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     628
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
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_shader_64bit_indexing.html SPV_EXT_shader_64bit_indexing>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_64bit_indexing] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_shader_64bit_indexing extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_64bit_indexing.adoc VK_EXT_shader_64bit_indexing>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-02
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GL_EXT_shader_64bit_indexing.txt GL_EXT_shader_64bit_indexing>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension relaxes the maxStorageBufferRange limit, allowing more
-- than 4GB to be accessed through a buffer binding (or through a buffer
-- device address). It adds pipeline and shader creation flags that request
-- 64-bit addressing support, and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-64bindexing defines>
-- which addressing calculations use 64 bits of range.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShader64BitIndexingFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_64BIT_INDEXING_EXTENSION_NAME'
--
-- -   'EXT_SHADER_64BIT_INDEXING_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_64_BIT_INDEXING_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_64_BIT_INDEXING_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_64_BIT_INDEXING_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2025-05-02 (Jeff Bolz)
--
--     -   Initial revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_64bit_indexing Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_64bit_indexing  (PhysicalDeviceShader64BitIndexingFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShader64BitIndexingFeaturesEXT

instance ToCStruct PhysicalDeviceShader64BitIndexingFeaturesEXT
instance Show PhysicalDeviceShader64BitIndexingFeaturesEXT

instance FromCStruct PhysicalDeviceShader64BitIndexingFeaturesEXT

