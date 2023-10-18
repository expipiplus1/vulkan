{-# language CPP #-}
-- | = Name
--
-- VK_EXT_shader_module_identifier - device extension
--
-- == VK_EXT_shader_module_identifier
--
-- [__Name String__]
--     @VK_EXT_shader_module_identifier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     463
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_cache_control VK_EXT_pipeline_creation_cache_control>
--
-- [__Contact__]
--
--     -   Hans-Kristian Arntzen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_shader_module_identifier] @HansKristian-Work%0A*Here describe the issue or question you have about the VK_EXT_shader_module_identifier extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_shader_module_identifier.adoc VK_EXT_shader_module_identifier>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Ricardo Garcia, Igalia
--
--     -   Piers Daniell, NVIDIA
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Tom Olson, Arm
--
--     -   Faith Ekstrand, Collabora
--
-- == Description
--
-- Some applications generate SPIR-V code at runtime. When pipeline caches
-- are primed, either explicitly through e.g.
-- 'Vulkan.Core10.Handles.PipelineCache' mechanisms, or implicitly through
-- driver managed caches, having to re-generate SPIR-V modules is
-- redundant. SPIR-V modules could be cached on disk by an application, but
-- the extra disk size requirement might be prohibitive in some use cases.
--
-- This extension adds the ability for an application to query a small
-- identifier associated with a 'Vulkan.Core10.Handles.ShaderModule'. On
-- subsequent runs of the application, the same identifier /can/ be
-- provided in lieu of a 'Vulkan.Core10.Handles.ShaderModule' object. A
-- pipeline creation call with such a module /may/ succeed if a pipeline
-- could be created without invoking compilation, and information inside
-- the SPIR-V module is not required by the implementation.
--
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
-- /must/ be used if only the identifier is provided, and this use case is
-- intended to work like a non-blocking, speculative compile. Applications
-- /can/ fallback as necessary.
--
-- The main motivation for identifying the module itself and not the entire
-- pipeline is that pipeline identifiers change when a driver is updated,
-- but module identifiers are expected to be stable for any particular
-- driver implementation. This approach is helpful for shader
-- pre-compilation systems which can prime pipeline caches ahead of time.
-- When on-disk pipeline caches are updated, the same shader identifiers
-- could lead to a pipeline cache hit.
--
-- == New Commands
--
-- -   'getShaderModuleCreateInfoIdentifierEXT'
--
-- -   'getShaderModuleIdentifierEXT'
--
-- == New Structures
--
-- -   'ShaderModuleIdentifierEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderModuleIdentifierFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderModuleIdentifierPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo':
--
--     -   'PipelineShaderStageModuleIdentifierCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SHADER_MODULE_IDENTIFIER_EXTENSION_NAME'
--
-- -   'EXT_SHADER_MODULE_IDENTIFIER_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_MODULE_IDENTIFIER_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_MODULE_IDENTIFIER_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_MODULE_IDENTIFIER_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-03-16 (Hans-Kristian Arntzen)
--
--     -   Initial draft
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_SHADER_MODULE_IDENTIFIER_SIZE_EXT',
-- 'PhysicalDeviceShaderModuleIdentifierFeaturesEXT',
-- 'PhysicalDeviceShaderModuleIdentifierPropertiesEXT',
-- 'PipelineShaderStageModuleIdentifierCreateInfoEXT',
-- 'ShaderModuleIdentifierEXT', 'getShaderModuleCreateInfoIdentifierEXT',
-- 'getShaderModuleIdentifierEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_shader_module_identifier Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_shader_module_identifier  ( PhysicalDeviceShaderModuleIdentifierFeaturesEXT
                                                          , PhysicalDeviceShaderModuleIdentifierPropertiesEXT
                                                          , PipelineShaderStageModuleIdentifierCreateInfoEXT
                                                          , ShaderModuleIdentifierEXT
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderModuleIdentifierFeaturesEXT

instance ToCStruct PhysicalDeviceShaderModuleIdentifierFeaturesEXT
instance Show PhysicalDeviceShaderModuleIdentifierFeaturesEXT

instance FromCStruct PhysicalDeviceShaderModuleIdentifierFeaturesEXT


data PhysicalDeviceShaderModuleIdentifierPropertiesEXT

instance ToCStruct PhysicalDeviceShaderModuleIdentifierPropertiesEXT
instance Show PhysicalDeviceShaderModuleIdentifierPropertiesEXT

instance FromCStruct PhysicalDeviceShaderModuleIdentifierPropertiesEXT


data PipelineShaderStageModuleIdentifierCreateInfoEXT

instance ToCStruct PipelineShaderStageModuleIdentifierCreateInfoEXT
instance Show PipelineShaderStageModuleIdentifierCreateInfoEXT

instance FromCStruct PipelineShaderStageModuleIdentifierCreateInfoEXT


data ShaderModuleIdentifierEXT

instance ToCStruct ShaderModuleIdentifierEXT
instance Show ShaderModuleIdentifierEXT

instance FromCStruct ShaderModuleIdentifierEXT

