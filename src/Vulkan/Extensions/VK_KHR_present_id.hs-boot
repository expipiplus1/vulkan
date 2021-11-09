{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_id - device extension
--
-- == VK_KHR_present_id
--
-- [__Name String__]
--     @VK_KHR_present_id@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     295
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
-- [__Contact__]
--
--     -   Keith Packard
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_present_id] @keithp%0A<<Here describe the issue or question you have about the VK_KHR_present_id extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Keith Packard, Valve
--
--     -   Ian Elliott, Google
--
--     -   Alon Or-bach, Samsung
--
-- == Description
--
-- This device extension allows an application that uses the
-- @VK_KHR_swapchain@ extension to provide an identifier for present
-- operations on a swapchain. An application /can/ use this to reference
-- specific present operations in other extensions.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentIdFeaturesKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentIdKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_ID_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_ID_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_ID_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-05-15 (Keith Packard)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDevicePresentIdFeaturesKHR', 'PresentIdKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_id  ( PhysicalDevicePresentIdFeaturesKHR
                                            , PresentIdKHR
                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePresentIdFeaturesKHR

instance ToCStruct PhysicalDevicePresentIdFeaturesKHR
instance Show PhysicalDevicePresentIdFeaturesKHR

instance FromCStruct PhysicalDevicePresentIdFeaturesKHR


data PresentIdKHR

instance ToCStruct PresentIdKHR
instance Show PresentIdKHR

instance FromCStruct PresentIdKHR

