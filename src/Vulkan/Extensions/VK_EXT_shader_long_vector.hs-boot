{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_long_vector - device extension
--
-- = VK_EXT_shader_long_vector
--
-- [__Name String__]
--     @VK_EXT_shader_long_vector@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     636
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Vulkan Version 1.2>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_KHR_workgroup_memory_explicit_layout
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_long_vector.html SPV_EXT_long_vector>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_long_vector] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_shader_long_vector extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_long_vector.adoc VK_EXT_shader_long_vector>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-24
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_long_vector.html SPV_EXT_long_vector>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_long_vector.txt GL_EXT_long_vector>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
-- == Description
--
-- This extension adds support for using vector types with more than four
-- components in SPIR-V.
--
-- Long vector types are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_long_vector.html SPV_EXT_long_vector>
-- SPIR-V extension and can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/ext/GLSL_EXT_long_vector.txt GL_EXT_long_vector>
-- GLSL extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderLongVectorFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderLongVectorPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_LONG_VECTOR_EXTENSION_NAME'
--
-- -   'EXT_SHADER_LONG_VECTOR_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_LONG_VECTOR_PROPERTIES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-LongVectorEXT LongVectorEXT>
--
-- == Version History
--
-- -   Revision 1, 2025-06-24 (Jeff Bolz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_shader_long_vector Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_long_vector  ( PhysicalDeviceShaderLongVectorFeaturesEXT
                                                    , PhysicalDeviceShaderLongVectorPropertiesEXT
                                                    ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderLongVectorFeaturesEXT

instance ToCStruct PhysicalDeviceShaderLongVectorFeaturesEXT
instance Show PhysicalDeviceShaderLongVectorFeaturesEXT

instance FromCStruct PhysicalDeviceShaderLongVectorFeaturesEXT


data PhysicalDeviceShaderLongVectorPropertiesEXT

instance ToCStruct PhysicalDeviceShaderLongVectorPropertiesEXT
instance Show PhysicalDeviceShaderLongVectorPropertiesEXT

instance FromCStruct PhysicalDeviceShaderLongVectorPropertiesEXT

