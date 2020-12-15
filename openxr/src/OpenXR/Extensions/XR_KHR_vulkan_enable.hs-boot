{-# language CPP #-}
-- | = Name
--
-- XR_KHR_vulkan_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable  XR_KHR_vulkan_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 26
--
-- = Revision
--
-- 7
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingVulkanKHR', 'GraphicsRequirementsVulkanKHR',
-- 'SwapchainImageVulkanKHR', 'getVulkanDeviceExtensionsKHR',
-- 'getVulkanGraphicsDeviceKHR', 'getVulkanGraphicsRequirementsKHR',
-- 'getVulkanInstanceExtensionsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_vulkan_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_vulkan_enable  ( GraphicsBindingVulkanKHR
                                               , GraphicsRequirementsVulkanKHR
                                               , SwapchainImageVulkanKHR
                                               ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data GraphicsBindingVulkanKHR

instance ToCStruct GraphicsBindingVulkanKHR
instance Show GraphicsBindingVulkanKHR

instance FromCStruct GraphicsBindingVulkanKHR


data GraphicsRequirementsVulkanKHR

instance ToCStruct GraphicsRequirementsVulkanKHR
instance Show GraphicsRequirementsVulkanKHR

instance FromCStruct GraphicsRequirementsVulkanKHR


data SwapchainImageVulkanKHR

instance ToCStruct SwapchainImageVulkanKHR
instance Show SwapchainImageVulkanKHR

instance FromCStruct SwapchainImageVulkanKHR

