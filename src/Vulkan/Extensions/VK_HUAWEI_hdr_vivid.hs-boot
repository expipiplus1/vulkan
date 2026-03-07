{-# language CPP #-}
-- | = Name
--
-- VK_HUAWEI_hdr_vivid - device extension
--
-- = VK_HUAWEI_hdr_vivid
--
-- [__Name String__]
--     @VK_HUAWEI_hdr_vivid@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     591
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_hdr_metadata VK_EXT_hdr_metadata>
--
-- [__Contact__]
--
--     -   Zehui Lin
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_HUAWEI_hdr_vivid] @bactlink%0A*Here describe the issue or question you have about the VK_HUAWEI_hdr_vivid extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-10-08
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Juntao Li, Huawei
--
--     -   Pan Gao, Huawei
--
--     -   Xiufeng Zhang, Huawei
--
--     -   Zehui Lin, Huawei
--
-- == Description
--
-- This extension allows applications to assign HDR Vivid (T\/UWA
-- 005.1-2022) metadata to swapchains.
--
-- HDR Vivid is an HDR standard released by UWA (UHD World Association). It
-- defines tone mapping from the metadata to better preserve the creator’s
-- intentions and achieve better consistency across devices with different
-- display capabilities.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Extensions.VK_EXT_hdr_metadata.HdrMetadataEXT':
--
--     -   'HdrVividDynamicMetadataHUAWEI'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceHdrVividFeaturesHUAWEI'
--
-- == New Enum Constants
--
-- -   'HUAWEI_HDR_VIVID_EXTENSION_NAME'
--
-- -   'HUAWEI_HDR_VIVID_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HDR_VIVID_DYNAMIC_METADATA_HUAWEI'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_HDR_VIVID_FEATURES_HUAWEI'
--
-- == Version History
--
-- -   Revision 1, 2024-10-08 (Zehui Lin)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_HUAWEI_hdr_vivid Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_HUAWEI_hdr_vivid  ( HdrVividDynamicMetadataHUAWEI
                                              , PhysicalDeviceHdrVividFeaturesHUAWEI
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data HdrVividDynamicMetadataHUAWEI

instance ToCStruct HdrVividDynamicMetadataHUAWEI
instance Show HdrVividDynamicMetadataHUAWEI

instance FromCStruct HdrVividDynamicMetadataHUAWEI


data PhysicalDeviceHdrVividFeaturesHUAWEI

instance ToCStruct PhysicalDeviceHdrVividFeaturesHUAWEI
instance Show PhysicalDeviceHdrVividFeaturesHUAWEI

instance FromCStruct PhysicalDeviceHdrVividFeaturesHUAWEI

