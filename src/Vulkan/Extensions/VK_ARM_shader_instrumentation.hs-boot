{-# language CPP #-}
-- | = Name
--
-- VK_ARM_shader_instrumentation - device extension
--
-- = VK_ARM_shader_instrumentation
--
-- [__Name String__]
--     @VK_ARM_shader_instrumentation@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     608
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
-- [__API Interactions__]
--
--     -   Interacts with VK_EXT_shader_object
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_shader_instrumentation] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_ARM_shader_instrumentation extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_shader_instrumentation.adoc VK_ARM_shader_instrumentation>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-02-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Embla Flatlandsmo, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Mikel Garai, Arm Ltd.
--
--     -   Peter Harris, Arm Ltd.
--
--     -   Ting Wei, Arm Ltd.
--
--     -   Torbjörn Nilsson, Arm Ltd.
--
-- == Description
--
-- This extension provides the ability to instrument shaders and capture
-- performance metrics per shader type from commands executed by a queue.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.ShaderInstrumentationARM'
--
-- == New Commands
--
-- -   'clearShaderInstrumentationMetricsARM'
--
-- -   'cmdBeginShaderInstrumentationARM'
--
-- -   'cmdEndShaderInstrumentationARM'
--
-- -   'createShaderInstrumentationARM'
--
-- -   'destroyShaderInstrumentationARM'
--
-- -   'enumeratePhysicalDeviceShaderInstrumentationMetricsARM'
--
-- -   'getShaderInstrumentationValuesARM'
--
-- == New Structures
--
-- -   'ShaderInstrumentationCreateInfoARM'
--
-- -   'ShaderInstrumentationMetricDataHeaderARM'
--
-- -   'ShaderInstrumentationMetricDescriptionARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderInstrumentationFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderInstrumentationPropertiesARM'
--
-- == New Bitmasks
--
-- -   'ShaderInstrumentationValuesFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_SHADER_INSTRUMENTATION_EXTENSION_NAME'
--
-- -   'ARM_SHADER_INSTRUMENTATION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_SHADER_INSTRUMENTATION_ARM'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_INSTRUMENT_SHADERS_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INSTRUMENTATION_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_INSTRUMENTATION_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SHADER_INSTRUMENTATION_METRIC_DESCRIPTION_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_shader_object VK_EXT_shader_object>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_shader_object.ShaderCreateFlagBitsEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_shader_object.SHADER_CREATE_INSTRUMENT_SHADER_BIT_ARM'
--
-- == Version History
--
-- -   Revision 1, 2026-02-26 (Embla Flatlandsmo, Jan-Harald Fredriksen)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_shader_instrumentation Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_shader_instrumentation  ( PhysicalDeviceShaderInstrumentationFeaturesARM
                                                        , PhysicalDeviceShaderInstrumentationPropertiesARM
                                                        , ShaderInstrumentationCreateInfoARM
                                                        , ShaderInstrumentationMetricDataHeaderARM
                                                        , ShaderInstrumentationMetricDescriptionARM
                                                        , ShaderInstrumentationValuesFlagsARM
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderInstrumentationFeaturesARM

instance ToCStruct PhysicalDeviceShaderInstrumentationFeaturesARM
instance Show PhysicalDeviceShaderInstrumentationFeaturesARM

instance FromCStruct PhysicalDeviceShaderInstrumentationFeaturesARM


data PhysicalDeviceShaderInstrumentationPropertiesARM

instance ToCStruct PhysicalDeviceShaderInstrumentationPropertiesARM
instance Show PhysicalDeviceShaderInstrumentationPropertiesARM

instance FromCStruct PhysicalDeviceShaderInstrumentationPropertiesARM


data ShaderInstrumentationCreateInfoARM

instance ToCStruct ShaderInstrumentationCreateInfoARM
instance Show ShaderInstrumentationCreateInfoARM

instance FromCStruct ShaderInstrumentationCreateInfoARM


data ShaderInstrumentationMetricDataHeaderARM

instance ToCStruct ShaderInstrumentationMetricDataHeaderARM
instance Show ShaderInstrumentationMetricDataHeaderARM

instance FromCStruct ShaderInstrumentationMetricDataHeaderARM


data ShaderInstrumentationMetricDescriptionARM

instance ToCStruct ShaderInstrumentationMetricDescriptionARM
instance Show ShaderInstrumentationMetricDescriptionARM

instance FromCStruct ShaderInstrumentationMetricDescriptionARM


data ShaderInstrumentationValuesFlagsARM

