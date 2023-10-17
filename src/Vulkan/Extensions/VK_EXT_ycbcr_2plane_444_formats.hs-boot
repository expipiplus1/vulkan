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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_sampler_ycbcr_conversion VK_KHR_sampler_ycbcr_conversion>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Tony Zlatinski
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_ycbcr_2plane_444_formats] @tzlatinski%0A*Here describe the issue or question you have about the VK_EXT_ycbcr_2plane_444_formats extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-28
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
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
--     -   'FORMAT_G10X6_B10X6R10X6_2PLANE_444_UNORM_3PACK16_EXT'
--
--     -   'FORMAT_G12X4_B12X4R12X4_2PLANE_444_UNORM_3PACK16_EXT'
--
--     -   'FORMAT_G16_B16R16_2PLANE_444_UNORM_EXT'
--
--     -   'FORMAT_G8_B8R8_2PLANE_444_UNORM_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_2_PLANE_444_FORMATS_FEATURES_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- This extension has been partially promoted. The format enumerants
-- introduced by the extension are included in core Vulkan 1.3, with the
-- EXT suffix omitted. However, runtime support for these formats is
-- optional in core Vulkan 1.3, while if this extension is supported,
-- runtime support is mandatory. The feature structure is not promoted. The
-- original enum names are still available as aliases of the core
-- functionality.
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_ycbcr_2plane_444_formats Vulkan Specification>
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

