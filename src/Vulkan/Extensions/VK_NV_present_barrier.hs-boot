{-# language CPP #-}
-- | = Name
--
-- VK_NV_present_barrier - device extension
--
-- == VK_NV_present_barrier
--
-- [__Name String__]
--     @VK_NV_present_barrier@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     293
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--
-- [__Contact__]
--
--     -   Liya Li
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_present_barrier] @liyli%0A*Here describe the issue or question you have about the VK_NV_present_barrier extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-05-16
--
-- [__Contributors__]
--
--     -   Liya Li, Nvidia
--
--     -   Martin Schwarzer, Nvidia
--
--     -   Andy Wolf, Nvidia
--
--     -   Ian Williams, Nvidia
--
--     -   Ben Morris, Nvidia
--
--     -   James Jones, Nvidia
--
--     -   Jeff Juliano, Nvidia
--
-- == Description
--
-- This extension adds support for synchronizing corresponding presentation
-- requests across multiple swapchains using the /present barrier/.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentBarrierFeaturesNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesPresentBarrierNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainPresentBarrierCreateInfoNV'
--
-- == New Enum Constants
--
-- -   'NV_PRESENT_BARRIER_EXTENSION_NAME'
--
-- -   'NV_PRESENT_BARRIER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_BARRIER_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_BARRIER_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_PRESENT_BARRIER_CREATE_INFO_NV'
--
-- == Issues
--
-- 1) Is there a query interface to check if a swapchain is using the
-- present barrier?
--
-- __RESOLVED__. There is no such query interface. When creating a
-- swapchain, an application can specify to use the /present barrier/, and
-- if the swapchain is created successfully, this swapchain will be using
-- the present barrier.
--
-- 2) Do we need an extra interface to set up the present barrier across
-- distributed systems?
--
-- __RESOLVED__. If the required hardware is presented in the system, and
-- all settings for the physical synchronization with other systems are set
-- up, an implementation manages the configuration automatically when
-- creating a swapchain, without any extra calls from the application.
--
-- == Version History
--
-- -   Revision 1, 2022-07-20
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDevicePresentBarrierFeaturesNV',
-- 'SurfaceCapabilitiesPresentBarrierNV',
-- 'SwapchainPresentBarrierCreateInfoNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_present_barrier Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_present_barrier  ( PhysicalDevicePresentBarrierFeaturesNV
                                                , SurfaceCapabilitiesPresentBarrierNV
                                                , SwapchainPresentBarrierCreateInfoNV
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePresentBarrierFeaturesNV

instance ToCStruct PhysicalDevicePresentBarrierFeaturesNV
instance Show PhysicalDevicePresentBarrierFeaturesNV

instance FromCStruct PhysicalDevicePresentBarrierFeaturesNV


data SurfaceCapabilitiesPresentBarrierNV

instance ToCStruct SurfaceCapabilitiesPresentBarrierNV
instance Show SurfaceCapabilitiesPresentBarrierNV

instance FromCStruct SurfaceCapabilitiesPresentBarrierNV


data SwapchainPresentBarrierCreateInfoNV

instance ToCStruct SwapchainPresentBarrierCreateInfoNV
instance Show SwapchainPresentBarrierCreateInfoNV

instance FromCStruct SwapchainPresentBarrierCreateInfoNV

