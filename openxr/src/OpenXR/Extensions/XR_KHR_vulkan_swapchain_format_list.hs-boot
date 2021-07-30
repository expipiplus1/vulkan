{-# language CPP #-}
-- | = Name
--
-- XR_KHR_vulkan_swapchain_format_list - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_swapchain_format_list  XR_KHR_vulkan_swapchain_format_list>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 15
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @XR_KHR_vulkan_enable@
--
-- = See Also
--
-- 'VulkanSwapchainFormatListCreateInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_swapchain_format_list OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_vulkan_swapchain_format_list  (VulkanSwapchainFormatListCreateInfoKHR) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data VulkanSwapchainFormatListCreateInfoKHR

instance ToCStruct VulkanSwapchainFormatListCreateInfoKHR
instance Show VulkanSwapchainFormatListCreateInfoKHR

instance FromCStruct VulkanSwapchainFormatListCreateInfoKHR

