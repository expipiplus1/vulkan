{-# language CPP #-}
-- | = Name
--
-- VK_EXT_swapchain_maintenance1 - device extension
--
-- = VK_EXT_swapchain_maintenance1
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
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_surface_maintenance1 VK_EXT_surface_maintenance1>
--     and
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--          or
--         
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain_maintenance1 VK_KHR_swapchain_maintenance1>
--         extension
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
--     2022-12-16
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to @VK_KHR_swapchain_maintenance1@
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
-- == Promotion to @VK_KHR_swapchain_maintenance1@
--
-- All functionality in this extension is included in
-- @VK_KHR_swapchain_maintenance1@, with the suffix changed to KHR. The
-- original type, enum, and command names are still available as aliases of
-- the KHR functionality.
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
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT'
--
-- == Version History
--
-- -   Revision 0, 2019-05-28 (James Jones)
--
--     -   Initial revisions
--
-- -   Revision 1, 2022-08-21 (Shahbaz Youssefi)
--
--     -   Add functionality and complete spec
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_swapchain_maintenance1 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_swapchain_maintenance1  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT
                                                        , pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT
                                                        , pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT
                                                        , pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT
                                                        , pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT
                                                        , pattern STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT
                                                        , pattern SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT
                                                        , releaseSwapchainImagesEXT
                                                        , PhysicalDeviceSwapchainMaintenance1FeaturesEXT
                                                        , SwapchainPresentFenceInfoEXT
                                                        , SwapchainPresentModesCreateInfoEXT
                                                        , SwapchainPresentModeInfoEXT
                                                        , SwapchainPresentScalingCreateInfoEXT
                                                        , ReleaseSwapchainImagesInfoEXT
                                                        , EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION
                                                        , pattern EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION
                                                        , EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME
                                                        , pattern EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME
                                                        , SwapchainKHR(..)
                                                        , PhysicalDeviceSwapchainMaintenance1FeaturesKHR(..)
                                                        , SwapchainPresentFenceInfoKHR(..)
                                                        , SwapchainPresentModesCreateInfoKHR(..)
                                                        , SwapchainPresentModeInfoKHR(..)
                                                        , SwapchainPresentScalingCreateInfoKHR(..)
                                                        , ReleaseSwapchainImagesInfoKHR(..)
                                                        , releaseSwapchainImagesKHR
                                                        , PresentModeKHR(..)
                                                        , SwapchainCreateFlagBitsKHR(..)
                                                        , SwapchainCreateFlagsKHR
                                                        , PresentScalingFlagBitsKHR(..)
                                                        , PresentScalingFlagsKHR
                                                        , PresentGravityFlagBitsKHR(..)
                                                        , PresentGravityFlagsKHR
                                                        ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (releaseSwapchainImagesKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (PhysicalDeviceSwapchainMaintenance1FeaturesKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (ReleaseSwapchainImagesInfoKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentFenceInfoKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentModeInfoKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentModesCreateInfoKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentScalingCreateInfoKHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (releaseSwapchainImagesKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (PhysicalDeviceSwapchainMaintenance1FeaturesKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentGravityFlagsKHR)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_surface_maintenance1 (PresentScalingFlagsKHR)
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (ReleaseSwapchainImagesInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentFenceInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentModeInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentModesCreateInfoKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain_maintenance1 (SwapchainPresentScalingCreateInfoKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_SWAPCHAIN_MAINTENANCE_1_FEATURES_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT"
pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_EXT = STRUCTURE_TYPE_SWAPCHAIN_PRESENT_FENCE_INFO_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_EXT = STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODES_CREATE_INFO_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT"
pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_EXT = STRUCTURE_TYPE_SWAPCHAIN_PRESENT_MODE_INFO_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_EXT = STRUCTURE_TYPE_SWAPCHAIN_PRESENT_SCALING_CREATE_INFO_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT"
pattern STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_EXT = STRUCTURE_TYPE_RELEASE_SWAPCHAIN_IMAGES_INFO_KHR


-- No documentation found for TopLevel "VK_SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT"
pattern SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_EXT = SWAPCHAIN_CREATE_DEFERRED_MEMORY_ALLOCATION_BIT_KHR


-- No documentation found for TopLevel "vkReleaseSwapchainImagesEXT"
releaseSwapchainImagesEXT = releaseSwapchainImagesKHR


-- No documentation found for TopLevel "VkPhysicalDeviceSwapchainMaintenance1FeaturesEXT"
type PhysicalDeviceSwapchainMaintenance1FeaturesEXT = PhysicalDeviceSwapchainMaintenance1FeaturesKHR


-- No documentation found for TopLevel "VkSwapchainPresentFenceInfoEXT"
type SwapchainPresentFenceInfoEXT = SwapchainPresentFenceInfoKHR


-- No documentation found for TopLevel "VkSwapchainPresentModesCreateInfoEXT"
type SwapchainPresentModesCreateInfoEXT = SwapchainPresentModesCreateInfoKHR


-- No documentation found for TopLevel "VkSwapchainPresentModeInfoEXT"
type SwapchainPresentModeInfoEXT = SwapchainPresentModeInfoKHR


-- No documentation found for TopLevel "VkSwapchainPresentScalingCreateInfoEXT"
type SwapchainPresentScalingCreateInfoEXT = SwapchainPresentScalingCreateInfoKHR


-- No documentation found for TopLevel "VkReleaseSwapchainImagesInfoEXT"
type ReleaseSwapchainImagesInfoEXT = ReleaseSwapchainImagesInfoKHR


type EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION"
pattern EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SWAPCHAIN_MAINTENANCE_1_SPEC_VERSION = 1


type EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_swapchain_maintenance1"

-- No documentation found for TopLevel "VK_EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME"
pattern EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SWAPCHAIN_MAINTENANCE_1_EXTENSION_NAME = "VK_EXT_swapchain_maintenance1"

