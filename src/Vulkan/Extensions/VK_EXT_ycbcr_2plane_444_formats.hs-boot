{-# language CPP #-}
-- | = Name
--
-- VK_EXT_ycbcr_2plane_444_formats - device extension
--
-- == VK_EXT_ycbcr_2plane_444_formats
--
-- [__Name String__]
--     @VK_EXT_ycbcr_2plane_444_formats@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     331
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@
--
-- [__Contact__]
--
--     -   Tony Zlatinski
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_ycbcr_2plane_444_formats] @tzlatinski%0A<<Here describe the issue or question you have about the VK_EXT_ycbcr_2plane_444_formats extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Piers Daniell, NVIDIA
--
--     -   Ping Liu, Intel
--
-- == Description
--
-- This extension adds some Y′CBCR formats that are in common use for video
-- encode and decode, but were not part of the
-- @VK_KHR_sampler_ycbcr_conversion@ extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_YCBCR_2PLANE_444_FORMATS_EXTENSION_NAME'
--
-- -   'EXT_YCBCR_2PLANE_444_FORMATS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_G16_B16R16_2PLANE_444_UNORM_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_G8_B8R8_2PLANE_444_UNORM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-03-08 (Piers Daniell)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_ycbcr_2plane_444_formats Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_ycbcr_2plane_444_formats  (PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT
instance Show PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT

instance FromCStruct PhysicalDeviceYcbcr2Plane444FormatsFeaturesEXT

