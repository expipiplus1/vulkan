{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pci_bus_info - device extension
--
-- == VK_EXT_pci_bus_info
--
-- [__Name String__]
--     @VK_EXT_pci_bus_info@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     213
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_pci_bus_info:%20&body=@anteru%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Daniel Rakos, AMD
--
-- == Description
--
-- This extension adds a new query to obtain PCI bus information about a
-- physical device.
--
-- Not all physical devices have PCI bus information, either due to the
-- device not being connected to the system through a PCI interface or due
-- to platform specific restrictions and policies. Thus this extension is
-- only expected to be supported by physical devices which can provide the
-- information.
--
-- As a consequence, applications should always check for the presence of
-- the extension string for each individual physical device for which they
-- intend to issue the new query for and should not have any assumptions
-- about the availability of the extension on any given platform.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePCIBusInfoPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PCI_BUS_INFO_EXTENSION_NAME'
--
-- -   'EXT_PCI_BUS_INFO_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT'
--
-- == Version History
--
-- -   Revision 2, 2018-12-10 (Daniel Rakos)
--
--     -   Changed all members of the new structure to have the uint32_t
--         type
--
-- -   Revision 1, 2018-10-11 (Daniel Rakos)
--
--     -   Initial revision
--
-- = See Also
--
-- 'PhysicalDevicePCIBusInfoPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pci_bus_info Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pci_bus_info  (PhysicalDevicePCIBusInfoPropertiesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDevicePCIBusInfoPropertiesEXT

instance ToCStruct PhysicalDevicePCIBusInfoPropertiesEXT
instance Show PhysicalDevicePCIBusInfoPropertiesEXT

instance FromCStruct PhysicalDevicePCIBusInfoPropertiesEXT

