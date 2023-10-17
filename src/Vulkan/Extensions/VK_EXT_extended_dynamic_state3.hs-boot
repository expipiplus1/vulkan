{-# language CPP #-}
-- | = Name
--
-- VK_EXT_extended_dynamic_state3 - device extension
--
-- == VK_EXT_extended_dynamic_state3
--
-- [__Name String__]
--     @VK_EXT_extended_dynamic_state3@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     456
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_extended_dynamic_state3] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_extended_dynamic_state3 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_extended_dynamic_state3.adoc VK_EXT_extended_dynamic_state3>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-09-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
--     -   Jamie Madill, Google
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Faith Ekstrand, Collabora
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Ricardo Garcia, Igalia
--
--     -   Samuel Pitoiset, Valve
--
--     -   Shahbaz Youssefi, Google
--
--     -   Stu Smith, AMD
--
--     -   Tapani Pälli, Intel
--
-- == Description
--
-- This extension adds almost all of the remaining pipeline state as
-- dynamic state to help applications further reduce the number of
-- monolithic pipelines they need to create and bind.
--
-- == New Commands
--
-- -   'cmdSetAlphaToCoverageEnableEXT'
--
-- -   'cmdSetAlphaToOneEnableEXT'
--
-- -   'cmdSetColorBlendAdvancedEXT'
--
-- -   'cmdSetColorBlendEnableEXT'
--
-- -   'cmdSetColorBlendEquationEXT'
--
-- -   'cmdSetColorWriteMaskEXT'
--
-- -   'cmdSetConservativeRasterizationModeEXT'
--
-- -   'cmdSetCoverageModulationModeNV'
--
-- -   'cmdSetCoverageModulationTableEnableNV'
--
-- -   'cmdSetCoverageModulationTableNV'
--
-- -   'cmdSetCoverageReductionModeNV'
--
-- -   'cmdSetCoverageToColorEnableNV'
--
-- -   'cmdSetCoverageToColorLocationNV'
--
-- -   'cmdSetDepthClampEnableEXT'
--
-- -   'cmdSetDepthClipEnableEXT'
--
-- -   'cmdSetDepthClipNegativeOneToOneEXT'
--
-- -   'cmdSetExtraPrimitiveOverestimationSizeEXT'
--
-- -   'cmdSetLineRasterizationModeEXT'
--
-- -   'cmdSetLineStippleEnableEXT'
--
-- -   'cmdSetLogicOpEnableEXT'
--
-- -   'cmdSetPolygonModeEXT'
--
-- -   'cmdSetProvokingVertexModeEXT'
--
-- -   'cmdSetRasterizationSamplesEXT'
--
-- -   'cmdSetRasterizationStreamEXT'
--
-- -   'cmdSetRepresentativeFragmentTestEnableNV'
--
-- -   'cmdSetSampleLocationsEnableEXT'
--
-- -   'cmdSetSampleMaskEXT'
--
-- -   'cmdSetShadingRateImageEnableNV'
--
-- -   'cmdSetTessellationDomainOriginEXT'
--
-- -   'cmdSetViewportSwizzleNV'
--
-- -   'cmdSetViewportWScalingEnableNV'
--
-- == New Structures
--
-- -   'ColorBlendAdvancedEXT'
--
-- -   'ColorBlendEquationEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExtendedDynamicState3FeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceExtendedDynamicState3PropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_3_EXTENSION_NAME'
--
-- -   'EXT_EXTENDED_DYNAMIC_STATE_3_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_COVERAGE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_ALPHA_TO_ONE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ADVANCED_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_BLEND_EQUATION_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COLOR_WRITE_MASK_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_CONSERVATIVE_RASTERIZATION_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_MODE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_ENABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_MODULATION_TABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_REDUCTION_MODE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_ENABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_COVERAGE_TO_COLOR_LOCATION_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLAMP_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_DEPTH_CLIP_NEGATIVE_ONE_TO_ONE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_EXTRA_PRIMITIVE_OVERESTIMATION_SIZE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_RASTERIZATION_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LINE_STIPPLE_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_LOGIC_OP_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_POLYGON_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_PROVOKING_VERTEX_MODE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_SAMPLES_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RASTERIZATION_STREAM_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_REPRESENTATIVE_FRAGMENT_TEST_ENABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_LOCATIONS_ENABLE_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SAMPLE_MASK_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_SHADING_RATE_IMAGE_ENABLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_TESSELLATION_DOMAIN_ORIGIN_EXT'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_SWIZZLE_NV'
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VIEWPORT_W_SCALING_ENABLE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_3_PROPERTIES_EXT'
--
-- == Issues
--
-- 1) What about the VkPipelineMultisampleStateCreateInfo state
-- @sampleShadingEnable@ and @minSampleShading@?
--
-- [UNRESOLVED]
--
--     -   @sampleShadingEnable@ and @minSampleShading@ are required when
--         compiling the fragment shader, and it is not meaningful to set
--         them dynamically since they always need to match the fragment
--         shader state, so this hardware state may as well just come from
--         the pipeline with the fragment shader.
--
-- == Version History
--
-- -   Revision 2, 2022-07-18 (Piers Daniell)
--
--     -   Added rasterizationSamples
--
-- -   Revision 1, 2022-05-18 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'ColorBlendAdvancedEXT', 'ColorBlendEquationEXT',
-- 'PhysicalDeviceExtendedDynamicState3FeaturesEXT',
-- 'PhysicalDeviceExtendedDynamicState3PropertiesEXT',
-- 'cmdSetAlphaToCoverageEnableEXT', 'cmdSetAlphaToOneEnableEXT',
-- 'cmdSetColorBlendAdvancedEXT', 'cmdSetColorBlendEnableEXT',
-- 'cmdSetColorBlendEquationEXT', 'cmdSetColorWriteMaskEXT',
-- 'cmdSetConservativeRasterizationModeEXT',
-- 'cmdSetCoverageModulationModeNV',
-- 'cmdSetCoverageModulationTableEnableNV',
-- 'cmdSetCoverageModulationTableNV', 'cmdSetCoverageReductionModeNV',
-- 'cmdSetCoverageToColorEnableNV', 'cmdSetCoverageToColorLocationNV',
-- 'cmdSetDepthClampEnableEXT', 'cmdSetDepthClipEnableEXT',
-- 'cmdSetDepthClipNegativeOneToOneEXT',
-- 'cmdSetExtraPrimitiveOverestimationSizeEXT',
-- 'cmdSetLineRasterizationModeEXT', 'cmdSetLineStippleEnableEXT',
-- 'cmdSetLogicOpEnableEXT', 'cmdSetPolygonModeEXT',
-- 'cmdSetProvokingVertexModeEXT', 'cmdSetRasterizationSamplesEXT',
-- 'cmdSetRasterizationStreamEXT',
-- 'cmdSetRepresentativeFragmentTestEnableNV',
-- 'cmdSetSampleLocationsEnableEXT', 'cmdSetSampleMaskEXT',
-- 'cmdSetShadingRateImageEnableNV', 'cmdSetTessellationDomainOriginEXT',
-- 'cmdSetViewportSwizzleNV', 'cmdSetViewportWScalingEnableNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_extended_dynamic_state3 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_extended_dynamic_state3  ( ColorBlendAdvancedEXT
                                                         , ColorBlendEquationEXT
                                                         , PhysicalDeviceExtendedDynamicState3FeaturesEXT
                                                         , PhysicalDeviceExtendedDynamicState3PropertiesEXT
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ColorBlendAdvancedEXT

instance ToCStruct ColorBlendAdvancedEXT
instance Show ColorBlendAdvancedEXT

instance FromCStruct ColorBlendAdvancedEXT


data ColorBlendEquationEXT

instance ToCStruct ColorBlendEquationEXT
instance Show ColorBlendEquationEXT

instance FromCStruct ColorBlendEquationEXT


data PhysicalDeviceExtendedDynamicState3FeaturesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicState3FeaturesEXT
instance Show PhysicalDeviceExtendedDynamicState3FeaturesEXT

instance FromCStruct PhysicalDeviceExtendedDynamicState3FeaturesEXT


data PhysicalDeviceExtendedDynamicState3PropertiesEXT

instance ToCStruct PhysicalDeviceExtendedDynamicState3PropertiesEXT
instance Show PhysicalDeviceExtendedDynamicState3PropertiesEXT

instance FromCStruct PhysicalDeviceExtendedDynamicState3PropertiesEXT

