{-# language CPP #-}
-- | = Name
--
-- VK_KHR_surface_protected_capabilities - instance extension
--
-- = Registered Extension Number
--
-- 240
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.1
--
-- -   Requires @VK_KHR_get_surface_capabilities2@
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-12-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Sandeep Shinde, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- This extension extends
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR',
-- providing applications a way to query whether swapchains /can/ be
-- created with the
-- 'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PROTECTED_BIT_KHR'
-- flag set.
--
-- Vulkan 1.1 added (optional) support for protect memory and protected
-- resources including buffers
-- ('Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_PROTECTED_BIT'),
-- images
-- ('Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_PROTECTED_BIT'),
-- and swapchains
-- ('Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PROTECTED_BIT_KHR').
-- However, on implementations which support multiple windowing systems,
-- not all window systems /may/ be able to provide a protected display
-- path.
--
-- This extension provides a way to query if a protected swapchain created
-- for a surface (and thus a specific windowing system) /can/ be displayed
-- on screen. It extends the existing
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- structure with a new 'SurfaceProtectedCapabilitiesKHR' structure from
-- which the application /can/ obtain information about support for
-- protected swapchain creation through
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceProtectedCapabilitiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SURFACE_PROTECTED_CAPABILITIES_EXTENSION_NAME'
--
-- -   'KHR_SURFACE_PROTECTED_CAPABILITIES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2018-12-18 (Sandeep Shinde, Daniel Koch)
--
--     -   Internal revisions.
--
-- = See Also
--
-- 'SurfaceProtectedCapabilitiesKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface_protected_capabilities Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_surface_protected_capabilities  (SurfaceProtectedCapabilitiesKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data SurfaceProtectedCapabilitiesKHR

instance ToCStruct SurfaceProtectedCapabilitiesKHR
instance Show SurfaceProtectedCapabilitiesKHR

instance FromCStruct SurfaceProtectedCapabilitiesKHR

