{-# language CPP #-}
-- | = Name
--
-- XR_KHR_vulkan_enable2 - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable2  XR_KHR_vulkan_enable2>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 91
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingVulkan2KHR', 'GraphicsRequirementsVulkan2KHR',
-- 'SwapchainImageVulkan2KHR', 'VulkanDeviceCreateInfoKHR',
-- 'VulkanGraphicsDeviceGetInfoKHR', 'VulkanInstanceCreateInfoKHR',
-- 'createVulkanDeviceKHR', 'createVulkanInstanceKHR',
-- 'getVulkanGraphicsDevice2KHR', 'getVulkanGraphicsRequirements2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable2 OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_vulkan_enable2  ( VulkanDeviceCreateInfoKHR
                                                , VulkanGraphicsDeviceGetInfoKHR
                                                , VulkanInstanceCreateInfoKHR
                                                ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data VulkanDeviceCreateInfoKHR

instance ToCStruct VulkanDeviceCreateInfoKHR
instance Show VulkanDeviceCreateInfoKHR

instance FromCStruct VulkanDeviceCreateInfoKHR


data VulkanGraphicsDeviceGetInfoKHR

instance ToCStruct VulkanGraphicsDeviceGetInfoKHR
instance Show VulkanGraphicsDeviceGetInfoKHR

instance FromCStruct VulkanGraphicsDeviceGetInfoKHR


data VulkanInstanceCreateInfoKHR

instance ToCStruct VulkanInstanceCreateInfoKHR
instance Show VulkanInstanceCreateInfoKHR

instance FromCStruct VulkanInstanceCreateInfoKHR

