{-# language CPP #-}
-- | = Name
--
-- VK_KHR_depth_clamp_zero_one - device extension
--
-- = VK_KHR_depth_clamp_zero_one
--
-- [__Name String__]
--     @VK_KHR_depth_clamp_zero_one@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     605
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
--     -   Graeme Leese
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_depth_clamp_zero_one] @gnl21%0A*Here describe the issue or question you have about the VK_KHR_depth_clamp_zero_one extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-09-10
--
-- [__Contributors__]
--
--     -   Graeme Leese, Broadcom
--
-- == Description
--
-- This extension is based on the @VK_EXT_depth_clamp_zero_one@ extension.
-- This extension gives defined behavior to fragment depth values which end
-- up outside the conventional [0, 1] range. It can be used to ensure
-- portability in edge cases of features like depthBias. The particular
-- behavior is chosen to match OpenGL to aid porting or emulation.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDepthClampZeroOneFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEPTH_CLAMP_ZERO_ONE_EXTENSION_NAME'
--
-- -   'KHR_DEPTH_CLAMP_ZERO_ONE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLAMP_ZERO_ONE_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-09-10 (Graeme Leese)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_depth_clamp_zero_one Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_depth_clamp_zero_one  (PhysicalDeviceDepthClampZeroOneFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDepthClampZeroOneFeaturesKHR

instance ToCStruct PhysicalDeviceDepthClampZeroOneFeaturesKHR
instance Show PhysicalDeviceDepthClampZeroOneFeaturesKHR

instance FromCStruct PhysicalDeviceDepthClampZeroOneFeaturesKHR

