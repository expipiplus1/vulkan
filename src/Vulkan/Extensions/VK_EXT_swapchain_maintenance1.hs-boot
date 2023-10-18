{-# language CPP #-}
-- | = Name
--
-- VK_EXT_swapchain_maintenance1 - device extension
--
-- == VK_EXT_swapchain_maintenance1
--
-- [__Name String__]
--     @VK_EXT_swapchain_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     276
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_swapchain_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_swapchain_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_swapchain_maintenance1.adoc VK_EXT_swapchain_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-10-28
--
-- [__Contributors__]
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Shahbaz Youssefi, Google
--
--     -   Chris Forbes, Google
--
--     -   Ian Elliott, Google
--
--     -   Yiwei Zhang, Google
--
--     -   Charlie Lao, Google
--
--     -   Lina Versace, Google
--
--     -   Ralph Potter, Samsung
--
--     -   Igor Nazarov, Samsung
--
--     -   Hyunchang Kim, Samsung
--
--     -   Suenghwan Lee, Samsung
--
--     -   Munseong Kang, Samsung
--
--     -   Joonyong Park, Samsung
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Lisa Wu, Arm
--
--     -   Daniel Stone, Collabora
--
--     -   Pan Gao, Huawei
--
-- == Description
--
-- @VK_EXT_swapchain_maintenance1@ adds a collection of window system
-- integration features that were intentionally left out or overlooked in
-- the original @VK_KHR_swapchain@ extension.
--
-- The new features are as follows:
--
-- -   Specify a fence that will be signaled when the resources associated
--     with a present operation /can/ be safely destroyed.
--
-- -   Allow changing the present mode a swapchain is using at per-present
--     granularity.
--
-- -   Allow applications to define the behavior when presenting a
--     swapchain image to a surface with different dimensions than the
--     image. Using this feature /may/ allow implementations to avoid
--     returning 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' in this
--     situation.
--
-- -   Allow applications to defer swapchain memory allocation for improved
--     startup time and memory footprint.
--
-- -   Allow applications to release previously acquired images without
--     presenting them.
--
-- == New Commands
--
-- -   'releaseSwapchainImagesEXT'
--
-- == New Structures
--
-- -   'ReleaseSwapchainImagesInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSwapchainMaintenance1FeaturesEXT'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'SwapchainPresentFenceInfoEXT'
--
--     -   'SwapchainPresentModeInfoEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainPresentModesCreateInfoEXT'
--
--     -   'SwapchainPresentScalingCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT'
--
-- == Version History
--
-- -   Revision 0, 2019-05-28
--
--     -   Initial revisions
--
-- -   Revision 1, 2022-08-21 (Shahbaz Youssefi)
--
--     -   Add functionality and complete spec
--
-- == See Also
--
-- 'PhysicalDeviceSwapchainMaintenance1FeaturesEXT',
-- 'ReleaseSwapchainImagesInfoEXT', 'SwapchainPresentFenceInfoEXT',
-- 'SwapchainPresentModeInfoEXT', 'SwapchainPresentModesCreateInfoEXT',
-- 'SwapchainPresentScalingCreateInfoEXT', 'releaseSwapchainImagesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_swapchain_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_swapchain_maintenance1  ( PhysicalDeviceSwapchainMaintenance1FeaturesEXT
                                                        , ReleaseSwapchainImagesInfoEXT
                                                        , SwapchainPresentFenceInfoEXT
                                                        , SwapchainPresentModeInfoEXT
                                                        , SwapchainPresentModesCreateInfoEXT
                                                        , SwapchainPresentScalingCreateInfoEXT
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceSwapchainMaintenance1FeaturesEXT

instance ToCStruct PhysicalDeviceSwapchainMaintenance1FeaturesEXT
instance Show PhysicalDeviceSwapchainMaintenance1FeaturesEXT

instance FromCStruct PhysicalDeviceSwapchainMaintenance1FeaturesEXT


data ReleaseSwapchainImagesInfoEXT

instance ToCStruct ReleaseSwapchainImagesInfoEXT
instance Show ReleaseSwapchainImagesInfoEXT

instance FromCStruct ReleaseSwapchainImagesInfoEXT


data SwapchainPresentFenceInfoEXT

instance ToCStruct SwapchainPresentFenceInfoEXT
instance Show SwapchainPresentFenceInfoEXT

instance FromCStruct SwapchainPresentFenceInfoEXT


data SwapchainPresentModeInfoEXT

instance ToCStruct SwapchainPresentModeInfoEXT
instance Show SwapchainPresentModeInfoEXT

instance FromCStruct SwapchainPresentModeInfoEXT


data SwapchainPresentModesCreateInfoEXT

instance ToCStruct SwapchainPresentModesCreateInfoEXT
instance Show SwapchainPresentModesCreateInfoEXT

instance FromCStruct SwapchainPresentModesCreateInfoEXT


data SwapchainPresentScalingCreateInfoEXT

instance ToCStruct SwapchainPresentScalingCreateInfoEXT
instance Show SwapchainPresentScalingCreateInfoEXT

instance FromCStruct SwapchainPresentScalingCreateInfoEXT

