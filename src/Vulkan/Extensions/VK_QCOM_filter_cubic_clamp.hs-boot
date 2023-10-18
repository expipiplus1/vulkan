{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_filter_cubic_clamp - device extension
--
-- == VK_QCOM_filter_cubic_clamp
--
-- [__Name String__]
--     @VK_QCOM_filter_cubic_clamp@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     522
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic VK_EXT_filter_cubic>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Version 1.2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_sampler_filter_minmax VK_EXT_sampler_filter_minmax>
--
-- [__Contact__]
--
--     -   Jeff Leger
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_filter_cubic_clamp] @jackohound%0A*Here describe the issue or question you have about the VK_QCOM_filter_cubic_clamp extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-08-02
--
-- [__Contributors__]
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
-- == Description
--
-- This extension extends cubic filtering by adding the ability to enable
-- an anti-ringing clamp. Cubic filtering samples from a 4x4 region of
-- texels and computes a cubic weighted average of the region. In some
-- cases, the resulting value is outside the range of any of the texels in
-- the 4x4 region. This is sometimes referred to as “filter overshoot” or
-- “filter ringing” and can occur when there is a sharp discontinuity in
-- the 4x4 region being filtered. For some use cases this “ringing” can
-- produces unacceptable artifacts.
--
-- The solution to the ringing problem is to clamp the post-cubic-filtered
-- value to be within the max and min of texel values in the 4x4 region.
-- While such “range clamping” can be performed in shader code, the
-- additional texture fetches and clamping ALU operations can be costly.
--
-- Certain Adreno GPUs are able to perform the range clamp in the texture
-- unit during cubic filtering at significant performance\/power savings
-- versus a shader-based clamping approach. This extension exposes such
-- hardware functionality.
--
-- This extension extends
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode', adding
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
-- which enables the range clamp operation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceCubicClampFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_FILTER_CUBIC_CLAMP_EXTENSION_NAME'
--
-- -   'QCOM_FILTER_CUBIC_CLAMP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode':
--
--     -   'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE_RANGECLAMP_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_CUBIC_CLAMP_FEATURES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2023-08-02 (jleger)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDeviceCubicClampFeaturesQCOM'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_QCOM_filter_cubic_clamp Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_filter_cubic_clamp  (PhysicalDeviceCubicClampFeaturesQCOM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceCubicClampFeaturesQCOM

instance ToCStruct PhysicalDeviceCubicClampFeaturesQCOM
instance Show PhysicalDeviceCubicClampFeaturesQCOM

instance FromCStruct PhysicalDeviceCubicClampFeaturesQCOM

