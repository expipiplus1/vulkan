{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_diagnostics_config - device extension
--
-- = Registered Extension Number
--
-- 301
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-12-15
--
-- [__Contributors__]
--
--     -   Kedarnath Thangudu, NVIDIA
--
--     -   Thomas Klein, NVIDIA
--
-- == Description
--
-- Applications using Nvidia Nsight™ Aftermath SDK for Vulkan to integrate
-- device crash dumps into their error reporting mechanisms, /may/ use this
-- extension to configure options related to device crash dump creation.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceDiagnosticsConfigCreateInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDiagnosticsConfigFeaturesNV'
--
-- == New Enums
--
-- -   'DeviceDiagnosticsConfigFlagBitsNV'
--
-- == New Bitmasks
--
-- -   'DeviceDiagnosticsConfigFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_DIAGNOSTICS_CONFIG_EXTENSION_NAME'
--
-- -   'NV_DEVICE_DIAGNOSTICS_CONFIG_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV'
--
-- == Version History
--
-- -   Revision 1, 2019-11-21 (Kedarnath Thangudu)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DeviceDiagnosticsConfigCreateInfoNV',
-- 'DeviceDiagnosticsConfigFlagBitsNV', 'DeviceDiagnosticsConfigFlagsNV',
-- 'PhysicalDeviceDiagnosticsConfigFeaturesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_diagnostics_config Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_diagnostics_config  ( DeviceDiagnosticsConfigCreateInfoNV
                                                          , PhysicalDeviceDiagnosticsConfigFeaturesNV
                                                          ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DeviceDiagnosticsConfigCreateInfoNV

instance ToCStruct DeviceDiagnosticsConfigCreateInfoNV
instance Show DeviceDiagnosticsConfigCreateInfoNV

instance FromCStruct DeviceDiagnosticsConfigCreateInfoNV


data PhysicalDeviceDiagnosticsConfigFeaturesNV

instance ToCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV
instance Show PhysicalDeviceDiagnosticsConfigFeaturesNV

instance FromCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV

