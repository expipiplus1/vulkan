{-# language CPP #-}
-- | = Name
--
-- VK_MSFT_layered_driver - device extension
--
-- == VK_MSFT_layered_driver
--
-- [__Name String__]
--     @VK_MSFT_layered_driver@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     531
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Jesse Natalie
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_MSFT_layered_driver] @jenatali%0A*Here describe the issue or question you have about the VK_MSFT_layered_driver extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_MSFT_layered_driver.adoc VK_MSFT_layered_driver>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-06-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jesse Natalie, Microsoft
--
-- == Description
--
-- This extension adds new physical device properties to allow applications
-- and the Vulkan ICD loader to understand when a physical device is
-- implemented as a layered driver on top of another underlying API.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLayeredDriverPropertiesMSFT'
--
-- == New Enums
--
-- -   'LayeredDriverUnderlyingApiMSFT'
--
-- == New Enum Constants
--
-- -   'MSFT_LAYERED_DRIVER_EXTENSION_NAME'
--
-- -   'MSFT_LAYERED_DRIVER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LAYERED_DRIVER_PROPERTIES_MSFT'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2023-06-21 (Jesse Natalie)
--
--     -   Initial revision
--
-- == See Also
--
-- 'LayeredDriverUnderlyingApiMSFT',
-- 'PhysicalDeviceLayeredDriverPropertiesMSFT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_MSFT_layered_driver Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MSFT_layered_driver  (PhysicalDeviceLayeredDriverPropertiesMSFT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceLayeredDriverPropertiesMSFT

instance ToCStruct PhysicalDeviceLayeredDriverPropertiesMSFT
instance Show PhysicalDeviceLayeredDriverPropertiesMSFT

instance FromCStruct PhysicalDeviceLayeredDriverPropertiesMSFT

