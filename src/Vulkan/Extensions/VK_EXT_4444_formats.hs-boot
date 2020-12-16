{-# language CPP #-}
-- | = Name
--
-- VK_EXT_4444_formats - device extension
--
-- == VK_EXT_4444_formats
--
-- [__Name String__]
--     @VK_EXT_4444_formats@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     341
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Joshua Ashton
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_4444_formats:%20&body=@Joshua-Ashton%20 >
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
--     -   Joshua Ashton, Valve
--
--     -   Jason Ekstrand, Intel
--
-- == Description
--
-- This extension defines the
-- 'Vulkan.Core10.Enums.Format.FORMAT_A4R4G4B4_UNORM_PACK16_EXT' and
-- 'Vulkan.Core10.Enums.Format.FORMAT_A4B4G4R4_UNORM_PACK16_EXT' formats
-- which are defined in other current graphics APIs.
--
-- This extension may be useful for building translation layers for those
-- APIs or for porting applications that use these formats without having
-- to resort to swizzles.
--
-- When VK_EXT_custom_border_color is used, these formats are not subject
-- to the same restrictions for border color without format as with
-- VK_FORMAT_B4G4R4A4_UNORM_PACK16.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevice4444FormatsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_4444_FORMATS_EXTENSION_NAME'
--
-- -   'EXT_4444_FORMATS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_A4B4G4R4_UNORM_PACK16_EXT'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_A4R4G4B4_UNORM_PACK16_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_4444_FORMATS_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-07-04 (Joshua Ashton)
--
--     -   Initial draft
--
-- = See Also
--
-- 'PhysicalDevice4444FormatsFeaturesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_4444_formats Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_4444_formats  (PhysicalDevice4444FormatsFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevice4444FormatsFeaturesEXT

instance ToCStruct PhysicalDevice4444FormatsFeaturesEXT
instance Show PhysicalDevice4444FormatsFeaturesEXT

instance FromCStruct PhysicalDevice4444FormatsFeaturesEXT

