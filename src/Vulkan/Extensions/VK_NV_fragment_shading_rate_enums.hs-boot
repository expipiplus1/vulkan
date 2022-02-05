{-# language CPP #-}
-- | = Name
--
-- VK_NV_fragment_shading_rate_enums - device extension
--
-- == VK_NV_fragment_shading_rate_enums
--
-- [__Name String__]
--     @VK_NV_fragment_shading_rate_enums@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     327
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_fragment_shading_rate@
--
-- [__Contact__]
--
--     -   Pat Brown
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_fragment_shading_rate_enums] @nvpbrown%0A<<Here describe the issue or question you have about the VK_NV_fragment_shading_rate_enums extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-09-02
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension builds on the fragment shading rate functionality
-- provided by the VK_KHR_fragment_shading_rate extension, adding support
-- for “supersample” fragment shading rates that trigger multiple fragment
-- shader invocations per pixel as well as a “no invocations” shading rate
-- that discards any portions of a primitive that would use that shading
-- rate.
--
-- == New Commands
--
-- -   'cmdSetFragmentShadingRateEnumNV'
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo':
--
--     -   'PipelineFragmentShadingRateEnumStateCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV'
--
-- == New Enums
--
-- -   'FragmentShadingRateNV'
--
-- -   'FragmentShadingRateTypeNV'
--
-- == New Enum Constants
--
-- -   'NV_FRAGMENT_SHADING_RATE_ENUMS_EXTENSION_NAME'
--
-- -   'NV_FRAGMENT_SHADING_RATE_ENUMS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADING_RATE_ENUMS_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_FRAGMENT_SHADING_RATE_ENUM_STATE_CREATE_INFO_NV'
--
-- == Issues
--
-- 1.  Why was this extension created? How should it be named?
--
--     __RESOLVED__: The primary goal of this extension was to expose
--     support for supersample and “no invocations” shading rates, which
--     are supported by the VK_NV_shading_rate_image extension but not by
--     VK_KHR_fragment_shading_rate. Because VK_KHR_fragment_shading_rate
--     specifies the primitive shading rate using a fragment size in
--     pixels, it lacks a good way to specify supersample rates. To deal
--     with this, we defined enums covering shading rates supported by the
--     KHR extension as well as the new shading rates and added structures
--     and APIs accepting shading rate enums instead of fragment sizes.
--
--     Since this extension adds two different types of shading rates, both
--     expressed using enums, we chose the extension name
--     VK_NV_fragment_shading_rate_enums.
--
-- 2.  Is this a standalone extension?
--
--     __RESOLVED__: No, this extension requires
--     VK_KHR_fragment_shading_rate. In order to use the features of this
--     extension, applications must enable the relevant features of KHR
--     extension.
--
-- 3.  How are the shading rate enums used, and how were the enum values
--     assigned?
--
--     __RESOLVED__: The shading rates supported by the enums in this
--     extension are accepted as pipeline, primitive, and attachment
--     shading rates and behave identically. For the shading rates also
--     supported by the KHR extension, the values assigned to the
--     corresponding enums are identical to the values already used for the
--     primitive and attachment shading rates in the KHR extension. For
--     those enums, bits 0 and 1 specify the base two logarithm of the
--     fragment height and bits 2 and 3 specify the base two logarithm of
--     the fragment width. For the new shading rates added by this
--     extension, we chose to use 11 through 14 (10 plus the base two
--     logarithm of the invocation count) for the supersample rates and 15
--     for the “no invocations” rate. None of those values are supported as
--     primitive or attachment shading rates by the KHR extension.
--
-- 4.  Between this extension, VK_KHR_fragment_shading_rate, and
--     VK_NV_shading_rate_image, there are three different ways to specify
--     shading rate state in a pipeline. How should we handle this?
--
--     __RESOLVED__: We do not allow the concurrent use of
--     VK_NV_shading_rate_image and VK_KHR_fragment_shading_rate; it is an
--     error to enable shading rate features from both extensions. But we
--     do allow applications to enable this extension together with
--     VK_KHR_fragment_shading_rate together. While we expect that
--     applications will never attach pipeline CreateInfo structures for
--     both this extension and the KHR extension concurrently, Vulkan does
--     not have any precedent forbidding such behavior and instead
--     typically treats a pipeline created without an extension-specific
--     CreateInfo structure as equivalent to one containing default values
--     specified by the extension. Rather than adding such a rule
--     considering the presence or absence of our new CreateInfo structure,
--     we instead included a @shadingRateType@ member to
--     'PipelineFragmentShadingRateEnumStateCreateInfoNV' that selects
--     between using state specified by that structure and state specified
--     by
--     'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PipelineFragmentShadingRateStateCreateInfoKHR'.
--
-- == Version History
--
-- -   Revision 1, 2020-09-02 (pbrown)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'FragmentShadingRateNV', 'FragmentShadingRateTypeNV',
-- 'PhysicalDeviceFragmentShadingRateEnumsFeaturesNV',
-- 'PhysicalDeviceFragmentShadingRateEnumsPropertiesNV',
-- 'PipelineFragmentShadingRateEnumStateCreateInfoNV',
-- 'cmdSetFragmentShadingRateEnumNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_fragment_shading_rate_enums Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_fragment_shading_rate_enums  ( PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
                                                            , PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
                                                            , PipelineFragmentShadingRateEnumStateCreateInfoNV
                                                            , FragmentShadingRateNV
                                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceFragmentShadingRateEnumsFeaturesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV
instance Show PhysicalDeviceFragmentShadingRateEnumsFeaturesNV

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsFeaturesNV


data PhysicalDeviceFragmentShadingRateEnumsPropertiesNV

instance ToCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV
instance Show PhysicalDeviceFragmentShadingRateEnumsPropertiesNV

instance FromCStruct PhysicalDeviceFragmentShadingRateEnumsPropertiesNV


data PipelineFragmentShadingRateEnumStateCreateInfoNV

instance ToCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV
instance Show PipelineFragmentShadingRateEnumStateCreateInfoNV

instance FromCStruct PipelineFragmentShadingRateEnumStateCreateInfoNV


data FragmentShadingRateNV

