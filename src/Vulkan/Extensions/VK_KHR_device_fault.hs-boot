{-# language CPP #-}
-- | = Name
--
-- VK_KHR_device_fault - device extension
--
-- = VK_KHR_device_fault
--
-- [__Name String__]
--     @VK_KHR_device_fault@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     574
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Ralph Potter <<data:image/png;base64, GitLab>>r_potter
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_device_fault.adoc VK_KHR_device_fault>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ralph Potter, Samsung
--
--     -   Craig Graham, Samsung
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
--     -   Adrian Ravai, Samsung
--
--     -   Peter Gal, Samsung
--
--     -   Matthew Netsch, QUALCOMM
--
--     -   Tobias Hector, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Vikram Tarikere, IMG
--
--     -   Jon Leech, Khronos
--
--     -   Samuel Pitoiset, Valve
--
-- == Description
--
-- Device loss can be triggered by a variety of issues, including invalid
-- API usage, implementation errors, or hardware failures. This extension
-- introduces two new commands:
--
-- -   'getDeviceFaultReportsKHR'
--
-- -   'getDeviceFaultDebugInfoKHR'
--
-- 'getDeviceFaultReportsKHR' allows developers to query for additional
-- information on GPU faults which may have caused device loss, and to
-- generate binary crash dumps, which may be loaded into external tools for
-- further diagnosis. Additionally this command allows developers to query
-- for additional information on GPU faults which were internally recovered
-- by the implementation.
--
-- 'getDeviceFaultReportsKHR' differs from
-- 'Vulkan.Extensions.VK_EXT_device_fault.getDeviceFaultInfoEXT' in that it
-- can be called at any time, is able to report faults which do not result
-- in 'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' and allows the caller
-- to wait for such an error to occur.
--
-- 'getDeviceFaultDebugInfoKHR' provides a separate interface which /must/
-- only be called when a device loss has occurred to provide extended GPU
-- vendor specific crash post-mortem information.
--
-- == New Commands
--
-- -   'getDeviceFaultDebugInfoKHR'
--
-- -   'getDeviceFaultReportsKHR'
--
-- == New Structures
--
-- -   'DeviceFaultAddressInfoKHR'
--
-- -   'DeviceFaultDebugInfoKHR'
--
-- -   'DeviceFaultInfoKHR'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionOneKHR'
--
-- -   'DeviceFaultVendorInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceFaultFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceFaultPropertiesKHR'
--
-- == New Enums
--
-- -   'DeviceFaultAddressTypeKHR'
--
-- -   'DeviceFaultFlagBitsKHR'
--
-- -   'DeviceFaultVendorBinaryHeaderVersionKHR'
--
-- == New Bitmasks
--
-- -   'DeviceFaultFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEVICE_FAULT_EXTENSION_NAME'
--
-- -   'KHR_DEVICE_FAULT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_DEBUG_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_FAULT_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_FAULT_PROPERTIES_KHR'
--
-- == Version History
--
-- -   Revision 0, 2024-03-01 (Ralph Potter)
--
--     -   Internal Revision
--
-- -   Revision 1, 2025-06-10 (Craig Graham)
--
--     -   Revised API to support async fault queries.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_device_fault Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_device_fault  ( DeviceFaultAddressInfoKHR
                                              , DeviceFaultDebugInfoKHR
                                              , DeviceFaultInfoKHR
                                              , DeviceFaultVendorBinaryHeaderVersionOneKHR
                                              , DeviceFaultVendorInfoKHR
                                              , PhysicalDeviceFaultFeaturesKHR
                                              , PhysicalDeviceFaultPropertiesKHR
                                              , DeviceFaultVendorBinaryHeaderVersionKHR
                                              , DeviceFaultAddressTypeKHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data DeviceFaultAddressInfoKHR

instance ToCStruct DeviceFaultAddressInfoKHR
instance Show DeviceFaultAddressInfoKHR

instance FromCStruct DeviceFaultAddressInfoKHR


type role DeviceFaultDebugInfoKHR nominal
data DeviceFaultDebugInfoKHR (es :: [Type])

instance ( Extendss DeviceFaultDebugInfoKHR es
         , PokeChain es ) => ToCStruct (DeviceFaultDebugInfoKHR es)
instance Show (Chain es) => Show (DeviceFaultDebugInfoKHR es)

instance ( Extendss DeviceFaultDebugInfoKHR es
         , PeekChain es ) => FromCStruct (DeviceFaultDebugInfoKHR es)


data DeviceFaultInfoKHR

instance ToCStruct DeviceFaultInfoKHR
instance Show DeviceFaultInfoKHR

instance FromCStruct DeviceFaultInfoKHR


data DeviceFaultVendorBinaryHeaderVersionOneKHR

instance ToCStruct DeviceFaultVendorBinaryHeaderVersionOneKHR
instance Show DeviceFaultVendorBinaryHeaderVersionOneKHR

instance FromCStruct DeviceFaultVendorBinaryHeaderVersionOneKHR


data DeviceFaultVendorInfoKHR

instance ToCStruct DeviceFaultVendorInfoKHR
instance Show DeviceFaultVendorInfoKHR

instance FromCStruct DeviceFaultVendorInfoKHR


data PhysicalDeviceFaultFeaturesKHR

instance ToCStruct PhysicalDeviceFaultFeaturesKHR
instance Show PhysicalDeviceFaultFeaturesKHR

instance FromCStruct PhysicalDeviceFaultFeaturesKHR


data PhysicalDeviceFaultPropertiesKHR

instance ToCStruct PhysicalDeviceFaultPropertiesKHR
instance Show PhysicalDeviceFaultPropertiesKHR

instance FromCStruct PhysicalDeviceFaultPropertiesKHR


data DeviceFaultVendorBinaryHeaderVersionKHR


data DeviceFaultAddressTypeKHR

