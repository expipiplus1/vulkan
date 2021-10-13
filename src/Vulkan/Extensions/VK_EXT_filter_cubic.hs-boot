{-# language CPP #-}
-- | = Name
--
-- VK_EXT_filter_cubic - device extension
--
-- == VK_EXT_filter_cubic
--
-- [__Name String__]
--     @VK_EXT_filter_cubic@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     171
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Bill Licea-Kane
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_filter_cubic] @wwlk%0A<<Here describe the issue or question you have about the VK_EXT_filter_cubic extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-12-13
--
-- [__Contributors__]
--
--     -   Bill Licea-Kane, Qualcomm Technologies, Inc.
--
--     -   Andrew Garrard, Samsung
--
--     -   Daniel Koch, NVIDIA
--
--     -   Donald Scorgie, Imagination Technologies
--
--     -   Graeme Leese, Broadcom
--
--     -   Jan-Herald Fredericksen, ARM
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Tobias Hector, AMD
--
--     -   Tom Olson, ARM
--
--     -   Stuart Smith, Imagination Technologies
--
-- == Description
--
-- @VK_EXT_filter_cubic@ extends @VK_IMG_filter_cubic@.
--
-- It documents cubic filtering of other image view types. It adds new
-- structures that /can/ be added to the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2'
-- and
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2'
-- that /can/ be used to determine which image types and which image view
-- types support cubic filtering.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2':
--
--     -   'FilterCubicImageViewImageFormatPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2':
--
--     -   'PhysicalDeviceImageViewImageFormatInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_FILTER_CUBIC_EXTENSION_NAME'
--
-- -   'EXT_FILTER_CUBIC_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Filter.Filter':
--
--     -   'FILTER_CUBIC_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FormatFeatureFlagBits':
--
--     -   'FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT'
--
-- == Version History
--
-- -   Revision 3, 2019-12-13 (wwlk)
--
--     -   Delete requirement to cubic filter the formats USCALED_PACKED32,
--         SSCALED_PACKED32, UINT_PACK32, and SINT_PACK32 (cut\/paste
--         error)
--
-- -   Revision 2, 2019-06-05 (wwlk)
--
--     -   Clarify 1D optional
--
-- -   Revision 1, 2019-01-24 (wwlk)
--
--     -   Initial version
--
-- = See Also
--
-- 'FilterCubicImageViewImageFormatPropertiesEXT',
-- 'PhysicalDeviceImageViewImageFormatInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_filter_cubic Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_filter_cubic  ( FilterCubicImageViewImageFormatPropertiesEXT
                                              , PhysicalDeviceImageViewImageFormatInfoEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data FilterCubicImageViewImageFormatPropertiesEXT

instance ToCStruct FilterCubicImageViewImageFormatPropertiesEXT
instance Show FilterCubicImageViewImageFormatPropertiesEXT

instance FromCStruct FilterCubicImageViewImageFormatPropertiesEXT


data PhysicalDeviceImageViewImageFormatInfoEXT

instance ToCStruct PhysicalDeviceImageViewImageFormatInfoEXT
instance Show PhysicalDeviceImageViewImageFormatInfoEXT

instance FromCStruct PhysicalDeviceImageViewImageFormatInfoEXT

