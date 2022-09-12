{-# language CPP #-}
-- | = Name
--
-- VK_EXT_rgba10x6_formats - device extension
--
-- == VK_EXT_rgba10x6_formats
--
-- [__Name String__]
--     @VK_EXT_rgba10x6_formats@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     345
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_sampler_ycbcr_conversion@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Jan-Harald Fredriksen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_rgba10x6_formats] @janharaldfredriksen-arm%0A*Here describe the issue or question you have about the VK_EXT_rgba10x6_formats extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-09-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Graeme Leese, Broadcom
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- This extension enables the
-- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16'
-- format to be used without a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- enabled.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRGBA10X6FormatsFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_RGBA10X6_FORMATS_EXTENSION_NAME'
--
-- -   'EXT_RGBA10X6_FORMATS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RGBA10X6_FORMATS_FEATURES_EXT'
--
-- == Issues
--
-- 1) Should we reuse the existing format enumeration or introduce a new
-- one?
--
-- __RESOLVED__: We reuse an existing format enumeration,
-- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16',
-- that was previously exclusively used for YCbCr and therefore had a set
-- of limitations related to that usage. The alternative was to introduce a
-- new format token with exactly the same bit representation as the
-- existing token, but without the limitations.
--
-- 2) Should we only introduce
-- 'Vulkan.Core10.Enums.Format.FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16'
-- or also 1-3 component variations?
--
-- __RESOLVED__: Only the 4-component format is introduced because the 1-
-- and 2- component variations are already not exclusive to YCbCr, and the
-- 3-component variation is not a good match for hardware capabilities.
--
-- == Version History
--
-- -   Revision 1, 2021-09-29 (Jan-Harald Fredriksen)
--
--     -   Initial EXT version
--
-- == See Also
--
-- 'PhysicalDeviceRGBA10X6FormatsFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_rgba10x6_formats Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_rgba10x6_formats  (PhysicalDeviceRGBA10X6FormatsFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRGBA10X6FormatsFeaturesEXT

instance ToCStruct PhysicalDeviceRGBA10X6FormatsFeaturesEXT
instance Show PhysicalDeviceRGBA10X6FormatsFeaturesEXT

instance FromCStruct PhysicalDeviceRGBA10X6FormatsFeaturesEXT

