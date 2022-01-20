{-# language CPP #-}
-- | = Name
--
-- VK_KHR_fragment_shading_rate - device extension
--
-- == VK_KHR_fragment_shading_rate
--
-- [__Name String__]
--     @VK_KHR_fragment_shading_rate@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     227
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_create_renderpass2@
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_fragment_shading_rate] @tobski%0A<<Here describe the issue or question you have about the VK_KHR_fragment_shading_rate extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_fragment_shading_rate.asciidoc VK_KHR_fragment_shading_rate>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-30
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_fragment_shading_rate.html SPV_KHR_fragment_shading_rate>.
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_fragment_shading_rate.txt GL_EXT_fragment_shading_rate>
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Guennadi Riguer, AMD
--
--     -   Matthaeus Chajdas, AMD
--
--     -   Pat Brown, Nvidia
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Slawomir Grajewski, Intel
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Jeff Bolz, Nvidia
--
--     -   Arseny Kapoulkine, Roblox
--
--     -   Contributors to the VK_NV_shading_rate_image specification
--
--     -   Contributors to the VK_EXT_fragment_density_map specification
--
-- == Description
--
-- This extension adds the ability to change the rate at which fragments
-- are shaded. Rather than the usual single fragment invocation for each
-- pixel covered by a primitive, multiple pixels can be shaded by a single
-- fragment shader invocation.
--
-- Up to three methods are available to the application to change the
-- fragment shading rate:
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-pipeline>,
--     which allows the specification of a rate per-draw.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive>,
--     which allows the specification of a rate per primitive, specified
--     during shading.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment>,
--     which allows the specification of a rate per-region of the
--     framebuffer, specified in a specialized image attachment.
--
-- Additionally, these rates can all be specified and combined in order to
-- adjust the overall detail in the image at each point.
--
-- This functionality can be used to focus shading efforts where higher
-- levels of detail are needed in some parts of a scene compared to others.
-- This can be particularly useful in high resolution rendering, or for XR
-- contexts.
--
-- This extension also adds support for the @SPV_KHR_fragment_shading_rate@
-- extension which enables setting the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-primitive primitive fragment shading rate>,
-- and allows querying the final shading rate from a fragment shader.
--
-- == New Commands
--
-- -   'cmdSetFragmentShadingRateKHR'
--
-- -   'getPhysicalDeviceFragmentShadingRatesKHR'
--
-- == New Structures
--
-- -   'PhysicalDeviceFragmentShadingRateKHR'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineFragmentShadingRateStateCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShadingRateFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentShadingRatePropertiesKHR'
--
-- -   Extending
--     'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2':
--
--     -   'FragmentShadingRateAttachmentInfoKHR'
--
-- == New Enums
--
-- -   'FragmentShadingRateCombinerOpKHR'
--
-- == New Enum Constants
--
-- -   'KHR_FRAGMENT_SHADING_RATE_EXTENSION_NAME'
--
-- -   'KHR_FRAGMENT_SHADING_RATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_FRAGMENT_SHADING_RATE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ImageLayout.ImageLayout':
--
--     -   'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_FRAGMENT_SHADING_RATE_ATTACHMENT_OPTIMAL_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ImageUsageFlagBits.ImageUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ImageUsageFlagBits.IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAGMENT_SHADING_RATE_ATTACHMENT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_STATE_CREATE_INFO_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.FormatFeatureFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-05-06 (Tobias Hector)
--
--     -   Initial revision
--
-- -   Revision 2, 2021-09-30 (Jon Leech)
--
--     -   Add interaction with @VK_KHR_format_feature_flags2@ to @vk.xml@
--
-- == See Also
--
-- 'FragmentShadingRateAttachmentInfoKHR',
-- 'FragmentShadingRateCombinerOpKHR',
-- 'PhysicalDeviceFragmentShadingRateFeaturesKHR',
-- 'PhysicalDeviceFragmentShadingRateKHR',
-- 'PhysicalDeviceFragmentShadingRatePropertiesKHR',
-- 'PipelineFragmentShadingRateStateCreateInfoKHR',
-- 'cmdSetFragmentShadingRateKHR',
-- 'getPhysicalDeviceFragmentShadingRatesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_fragment_shading_rate  ( FragmentShadingRateAttachmentInfoKHR
                                                       , PhysicalDeviceFragmentShadingRateFeaturesKHR
                                                       , PhysicalDeviceFragmentShadingRateKHR
                                                       , PhysicalDeviceFragmentShadingRatePropertiesKHR
                                                       , PipelineFragmentShadingRateStateCreateInfoKHR
                                                       , FragmentShadingRateCombinerOpKHR
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FragmentShadingRateAttachmentInfoKHR

instance ToCStruct FragmentShadingRateAttachmentInfoKHR
instance Show FragmentShadingRateAttachmentInfoKHR

instance FromCStruct FragmentShadingRateAttachmentInfoKHR


data PhysicalDeviceFragmentShadingRateFeaturesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRateFeaturesKHR
instance Show PhysicalDeviceFragmentShadingRateFeaturesKHR

instance FromCStruct PhysicalDeviceFragmentShadingRateFeaturesKHR


data PhysicalDeviceFragmentShadingRateKHR

instance ToCStruct PhysicalDeviceFragmentShadingRateKHR
instance Show PhysicalDeviceFragmentShadingRateKHR

instance FromCStruct PhysicalDeviceFragmentShadingRateKHR


data PhysicalDeviceFragmentShadingRatePropertiesKHR

instance ToCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR
instance Show PhysicalDeviceFragmentShadingRatePropertiesKHR

instance FromCStruct PhysicalDeviceFragmentShadingRatePropertiesKHR


data PipelineFragmentShadingRateStateCreateInfoKHR

instance ToCStruct PipelineFragmentShadingRateStateCreateInfoKHR
instance Show PipelineFragmentShadingRateStateCreateInfoKHR

instance FromCStruct PipelineFragmentShadingRateStateCreateInfoKHR


data FragmentShadingRateCombinerOpKHR

