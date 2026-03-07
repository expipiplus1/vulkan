{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_id2 - device extension
--
-- = VK_KHR_present_id2
--
-- [__Name String__]
--     @VK_KHR_present_id2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     480
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Daniel Stone
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_present_id2.adoc VK_KHR_present_id2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Stone, Collabora
--
--     -   Derek Foreman, Collabora
--
--     -   /contributors to
--         \`<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id VK_KHR_present_id>\`/
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
--     -   'PhysicalDevicePresentId2FeaturesKHR'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentId2KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesPresentId2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_ID_2_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_ID_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_ID_2_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_ID_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_ID_2_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_ID_2_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-05-10 (Daniel Stone)
--
--     -   Repurposed VK_KHR_present_id to be driven by surface
--         capabilities
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_present_id2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_id2  ( PhysicalDevicePresentId2FeaturesKHR
                                             , PresentId2KHR
                                             , SurfaceCapabilitiesPresentId2KHR
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePresentId2FeaturesKHR

instance ToCStruct PhysicalDevicePresentId2FeaturesKHR
instance Show PhysicalDevicePresentId2FeaturesKHR

instance FromCStruct PhysicalDevicePresentId2FeaturesKHR


data PresentId2KHR

instance ToCStruct PresentId2KHR
instance Show PresentId2KHR

instance FromCStruct PresentId2KHR


data SurfaceCapabilitiesPresentId2KHR

instance ToCStruct SurfaceCapabilitiesPresentId2KHR
instance Show SurfaceCapabilitiesPresentId2KHR

instance FromCStruct SurfaceCapabilitiesPresentId2KHR

