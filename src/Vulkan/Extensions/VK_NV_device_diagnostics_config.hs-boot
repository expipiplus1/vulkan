{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_diagnostics_config - device extension
--
-- == VK_NV_device_diagnostics_config
--
-- [__Name String__]
--     @VK_NV_device_diagnostics_config@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     301
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Kedarnath Thangudu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_device_diagnostics_config] @kthangudu%0A*Here describe the issue or question you have about the VK_NV_device_diagnostics_config extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-04-06
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
-- Version 2 of this extension adds
-- 'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV' which
-- when set enables enhanced reporting of shader execution errors.
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
-- -   Revision 2, 2022-04-06 (Kedarnath Thangudu)
--
--     -   Added a config bit
--         'DEVICE_DIAGNOSTICS_CONFIG_ENABLE_SHADER_ERROR_REPORTING_BIT_NV'
--
-- == See Also
--
-- 'DeviceDiagnosticsConfigCreateInfoNV',
-- 'DeviceDiagnosticsConfigFlagBitsNV', 'DeviceDiagnosticsConfigFlagsNV',
-- 'PhysicalDeviceDiagnosticsConfigFeaturesNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_device_diagnostics_config Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_diagnostics_config  ( DeviceDiagnosticsConfigCreateInfoNV
                                                          , PhysicalDeviceDiagnosticsConfigFeaturesNV
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceDiagnosticsConfigCreateInfoNV

instance ToCStruct DeviceDiagnosticsConfigCreateInfoNV
instance Show DeviceDiagnosticsConfigCreateInfoNV

instance FromCStruct DeviceDiagnosticsConfigCreateInfoNV


data PhysicalDeviceDiagnosticsConfigFeaturesNV

instance ToCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV
instance Show PhysicalDeviceDiagnosticsConfigFeaturesNV

instance FromCStruct PhysicalDeviceDiagnosticsConfigFeaturesNV

