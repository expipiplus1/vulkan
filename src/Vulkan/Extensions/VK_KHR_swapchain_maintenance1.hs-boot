{-# language CPP #-}
-- | = Name
--
-- VK_KHR_swapchain_maintenance1 - device extension
--
-- = VK_KHR_swapchain_maintenance1
--
-- [__Name String__]
--     @VK_KHR_swapchain_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     488
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_maintenance1 VK_KHR_surface_maintenance1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_swapchain_maintenance1] @syoussefi%0A*Here describe the issue or question you have about the VK_KHR_swapchain_maintenance1 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_swapchain_maintenance1.adoc VK_KHR_swapchain_maintenance1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-03-31
--
-- [__Contributors__]
--
--     -   James Jones, NVIDIA
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
-- This extension is based off the @VK_EXT_swapchain_maintenance1@
-- extension.
--
-- @VK_KHR_swapchain_maintenance1@ adds a collection of window system
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
-- -   'releaseSwapchainImagesKHR'
--
-- == New Structures
--
-- -   'ReleaseSwapchainImagesInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSwapchainMaintenance1FeaturesKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'SwapchainPresentFenceInfoKHR'
--
--     -   'SwapchainPresentModeInfoKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainPresentModesCreateInfoKHR'
--
--     -   'SwapchainPresentScalingCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_KHR'
--
-- == Version History
--
-- -   Revision 1, 2025-03-31 (Shahbaz Youssefi)
--
--     -   Based on VK_EXT_swapchain_maintenance1
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_swapchain_maintenance1 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_swapchain_maintenance1  ( PhysicalDeviceSwapchainMaintenance1FeaturesKHR
                                                        , ReleaseSwapchainImagesInfoKHR
                                                        , SwapchainPresentFenceInfoKHR
                                                        , SwapchainPresentModeInfoKHR
                                                        , SwapchainPresentModesCreateInfoKHR
                                                        , SwapchainPresentScalingCreateInfoKHR
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceSwapchainMaintenance1FeaturesKHR

instance ToCStruct PhysicalDeviceSwapchainMaintenance1FeaturesKHR
instance Show PhysicalDeviceSwapchainMaintenance1FeaturesKHR

instance FromCStruct PhysicalDeviceSwapchainMaintenance1FeaturesKHR


data ReleaseSwapchainImagesInfoKHR

instance ToCStruct ReleaseSwapchainImagesInfoKHR
instance Show ReleaseSwapchainImagesInfoKHR

instance FromCStruct ReleaseSwapchainImagesInfoKHR


data SwapchainPresentFenceInfoKHR

instance ToCStruct SwapchainPresentFenceInfoKHR
instance Show SwapchainPresentFenceInfoKHR

instance FromCStruct SwapchainPresentFenceInfoKHR


data SwapchainPresentModeInfoKHR

instance ToCStruct SwapchainPresentModeInfoKHR
instance Show SwapchainPresentModeInfoKHR

instance FromCStruct SwapchainPresentModeInfoKHR


data SwapchainPresentModesCreateInfoKHR

instance ToCStruct SwapchainPresentModesCreateInfoKHR
instance Show SwapchainPresentModesCreateInfoKHR

instance FromCStruct SwapchainPresentModesCreateInfoKHR


data SwapchainPresentScalingCreateInfoKHR

instance ToCStruct SwapchainPresentScalingCreateInfoKHR
instance Show SwapchainPresentScalingCreateInfoKHR

instance FromCStruct SwapchainPresentScalingCreateInfoKHR

