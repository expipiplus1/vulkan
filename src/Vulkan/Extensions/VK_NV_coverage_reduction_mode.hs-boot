{-# language CPP #-}
-- | = Name
--
-- VK_NV_coverage_reduction_mode - device extension
--
-- == VK_NV_coverage_reduction_mode
--
-- [__Name String__]
--     @VK_NV_coverage_reduction_mode@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     251
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_NV_framebuffer_mixed_samples@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Kedarnath Thangudu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_coverage_reduction_mode] @kthangudu%0A*Here describe the issue or question you have about the VK_NV_coverage_reduction_mode extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-29
--
-- [__Contributors__]
--
--     -   Kedarnath Thangudu, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- When using a framebuffer with mixed samples, a per-fragment coverage
-- reduction operation is performed which generates color sample coverage
-- from the pixel coverage. This extension defines the following modes to
-- control how this reduction is performed.
--
-- -   Merge: When there are more samples in the pixel coverage than color
--     samples, there is an implementation-dependent association of each
--     pixel coverage sample to a color sample. In the merge mode, the
--     color sample coverage is computed such that only if any associated
--     sample in the pixel coverage is covered, the color sample is
--     covered. This is the default mode.
--
-- -   Truncate: When there are more raster samples (N) than color
--     samples(M), there is one to one association of the first M raster
--     samples to the M color samples; other raster samples are ignored.
--
-- When the number of raster samples is equal to the color samples, there
-- is a one to one mapping between them in either of the above modes.
--
-- The new command
-- 'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV' can be
-- used to query the various raster, color, depth\/stencil sample count and
-- reduction mode combinations that are supported by the implementation.
-- This extension would allow an implementation to support the behavior of
-- both @VK_NV_framebuffer_mixed_samples@ and
-- @VK_AMD_mixed_attachment_samples@ extensions simultaneously.
--
-- == New Commands
--
-- -   'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- == New Structures
--
-- -   'FramebufferMixedSamplesCombinationNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCoverageReductionModeFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo':
--
--     -   'PipelineCoverageReductionStateCreateInfoNV'
--
-- == New Enums
--
-- -   'CoverageReductionModeNV'
--
-- == New Bitmasks
--
-- -   'PipelineCoverageReductionStateCreateFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_COVERAGE_REDUCTION_MODE_EXTENSION_NAME'
--
-- -   'NV_COVERAGE_REDUCTION_MODE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV'
--
-- == Version History
--
-- -   Revision 1, 2019-01-29 (Kedarnath Thangudu)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'CoverageReductionModeNV', 'FramebufferMixedSamplesCombinationNV',
-- 'PhysicalDeviceCoverageReductionModeFeaturesNV',
-- 'PipelineCoverageReductionStateCreateFlagsNV',
-- 'PipelineCoverageReductionStateCreateInfoNV',
-- 'getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_coverage_reduction_mode Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_coverage_reduction_mode  ( FramebufferMixedSamplesCombinationNV
                                                        , PhysicalDeviceCoverageReductionModeFeaturesNV
                                                        , PipelineCoverageReductionStateCreateInfoNV
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FramebufferMixedSamplesCombinationNV

instance ToCStruct FramebufferMixedSamplesCombinationNV
instance Show FramebufferMixedSamplesCombinationNV

instance FromCStruct FramebufferMixedSamplesCombinationNV


data PhysicalDeviceCoverageReductionModeFeaturesNV

instance ToCStruct PhysicalDeviceCoverageReductionModeFeaturesNV
instance Show PhysicalDeviceCoverageReductionModeFeaturesNV

instance FromCStruct PhysicalDeviceCoverageReductionModeFeaturesNV


data PipelineCoverageReductionStateCreateInfoNV

instance ToCStruct PipelineCoverageReductionStateCreateInfoNV
instance Show PipelineCoverageReductionStateCreateInfoNV

instance FromCStruct PipelineCoverageReductionStateCreateInfoNV

