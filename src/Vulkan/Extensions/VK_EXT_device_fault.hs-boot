{-# language CPP #-}
-- | = Name
--
-- VK_EXT_device_fault - device extension
--
-- == VK_EXT_device_fault
--
-- [__Name String__]
--     @VK_EXT_device_fault@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     342
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_device_fault.adoc VK_EXT_device_fault>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ralph Potter, Samsung
--
--     -   Stuart Smith, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Mark Bellamy, ARM
--
--     -   Andrew Ellem, Google
--
--     -   Alex Walters, IMG
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Baldur Karlsson, Valve
--
-- == Description
--
-- Device loss can be triggered by a variety of issues, including invalid
-- API usage, implementation errors, or hardware failures.
--
-- This extension introduces a new command: 'getDeviceFaultInfoEXT', which
-- may be called subsequent to a
-- 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' error code having been
-- returned by the implementation. This command allows developers to query
-- for additional information on GPU faults which may have caused device
-- loss, and to generate binary crash dumps, which may be loaded into
-- external tools for further diagnosis.
--
-- == New Commands
--
-- -   'getDeviceFaultInfoEXT'
--
-- == New Structures
--
-- -   'DeviceFaultAddressInfoEXT'
--
-- -   'DeviceFaultCountsEXT'
--
-- -   'DeviceFaultInfoEXT'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionOneEXT'
--
-- -   'DeviceFaultVendorInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFaultFeaturesEXT'
--
-- == New Enums
--
-- -   'DeviceFaultAddressTypeEXT'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DEVICE_FAULT_EXTENSION_NAME'
--
-- -   'EXT_DEVICE_FAULT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_COUNTS_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 2, 2023-04-05 (Ralph Potter)
--
--     -   Restored two missing members to the XML definition of
--         VkDeviceFaultVendorBinaryHeaderVersionOneEXT. No functional
--         change to the specification.
--
-- -   Revision 1, 2020-10-19 (Ralph Potter)
--
--     -   Initial revision
--
-- == See Also
--
-- 'DeviceFaultAddressInfoEXT', 'DeviceFaultAddressTypeEXT',
-- 'DeviceFaultCountsEXT', 'DeviceFaultInfoEXT',
-- 'DeviceFaultVendorBinaryHeaderVersionEXT',
-- 'DeviceFaultVendorBinaryHeaderVersionOneEXT',
-- 'DeviceFaultVendorInfoEXT', 'PhysicalDeviceFaultFeaturesEXT',
-- 'getDeviceFaultInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_device_fault Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_device_fault  ( DeviceFaultAddressInfoEXT
                                              , DeviceFaultCountsEXT
                                              , DeviceFaultInfoEXT
                                              , DeviceFaultVendorBinaryHeaderVersionOneEXT
                                              , DeviceFaultVendorInfoEXT
                                              , PhysicalDeviceFaultFeaturesEXT
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceFaultAddressInfoEXT

instance ToCStruct DeviceFaultAddressInfoEXT
instance Show DeviceFaultAddressInfoEXT

instance FromCStruct DeviceFaultAddressInfoEXT


data DeviceFaultCountsEXT

instance ToCStruct DeviceFaultCountsEXT
instance Show DeviceFaultCountsEXT

instance FromCStruct DeviceFaultCountsEXT


data DeviceFaultInfoEXT

instance ToCStruct DeviceFaultInfoEXT
instance Show DeviceFaultInfoEXT

instance FromCStruct DeviceFaultInfoEXT


data DeviceFaultVendorBinaryHeaderVersionOneEXT

instance ToCStruct DeviceFaultVendorBinaryHeaderVersionOneEXT
instance Show DeviceFaultVendorBinaryHeaderVersionOneEXT

instance FromCStruct DeviceFaultVendorBinaryHeaderVersionOneEXT


data DeviceFaultVendorInfoEXT

instance ToCStruct DeviceFaultVendorInfoEXT
instance Show DeviceFaultVendorInfoEXT

instance FromCStruct DeviceFaultVendorInfoEXT


data PhysicalDeviceFaultFeaturesEXT

instance ToCStruct PhysicalDeviceFaultFeaturesEXT
instance Show PhysicalDeviceFaultFeaturesEXT

instance FromCStruct PhysicalDeviceFaultFeaturesEXT

