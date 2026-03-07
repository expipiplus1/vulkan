{-# language CPP #-}
-- | = Name
--
-- VK_NV_present_metering - device extension
--
-- = VK_NV_present_metering
--
-- [__Name String__]
--     @VK_NV_present_metering@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     614
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
--     -   __This is a /provisional/ extension and /must/ be used with
--         caution. See the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#boilerplate-provisional-header description>
--         of provisional header files for enablement and stability
--         details.__
--
-- [__Contact__]
--
--     -   Charles Hansen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_present_metering] @chansen%0A*Here describe the issue or question you have about the VK_NV_present_metering extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-08
--
-- [__Provisional__]
--     *This extension is /provisional/ and /should/ not be used in
--     production applications. The functionality defined by this extension
--     /may/ change in ways that break backwards compatibility between
--     revisions, and before the final release of the non-provisional
--     version of this extension.
--
-- [__Contributors__]
--
--     -   Charles Hansen, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
-- == Description
--
-- This extension is used to evenly meter presents.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentMeteringFeaturesNV'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'SetPresentConfigNV'
--
-- == New Enum Constants
--
-- -   'NV_PRESENT_METERING_EXTENSION_NAME'
--
-- -   'NV_PRESENT_METERING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_METERING_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SET_PRESENT_CONFIG_NV'
--
-- == Version History
--
-- -   Revision 1, 2025-01-08 (Charles Hansen)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_present_metering Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_present_metering  ( PhysicalDevicePresentMeteringFeaturesNV
                                                 , SetPresentConfigNV
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePresentMeteringFeaturesNV

instance ToCStruct PhysicalDevicePresentMeteringFeaturesNV
instance Show PhysicalDevicePresentMeteringFeaturesNV

instance FromCStruct PhysicalDevicePresentMeteringFeaturesNV


data SetPresentConfigNV

instance ToCStruct SetPresentConfigNV
instance Show SetPresentConfigNV

instance FromCStruct SetPresentConfigNV

