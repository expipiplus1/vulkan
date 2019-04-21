{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  , VkDisplayModeCreateFlagsKHR
  , VkDisplayModeCreateInfoKHR
  , VkDisplayModeKHR
  , VkDisplayModeParametersKHR
  , VkDisplayModePropertiesKHR
  , VkDisplayPlaneAlphaFlagBitsKHR
  , VkDisplayPlaneAlphaFlagsKHR
  , VkDisplayPlaneCapabilitiesKHR
  , VkDisplayPlanePropertiesKHR
  , VkDisplayPropertiesKHR
  , VkDisplaySurfaceCreateFlagsKHR
  , VkDisplaySurfaceCreateInfoKHR
  , FN_vkCreateDisplayModeKHR
  , PFN_vkCreateDisplayModeKHR
  , FN_vkCreateDisplayPlaneSurfaceKHR
  , PFN_vkCreateDisplayPlaneSurfaceKHR
  , FN_vkGetDisplayModePropertiesKHR
  , PFN_vkGetDisplayModePropertiesKHR
  , FN_vkGetDisplayPlaneCapabilitiesKHR
  , PFN_vkGetDisplayPlaneCapabilitiesKHR
  , FN_vkGetDisplayPlaneSupportedDisplaysKHR
  , PFN_vkGetDisplayPlaneSupportedDisplaysKHR
  , FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , FN_vkGetPhysicalDeviceDisplayPropertiesKHR
  , PFN_vkGetPhysicalDeviceDisplayPropertiesKHR
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  , VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


-- | Dummy data to tag the 'Ptr' with
data VkDisplayKHR_T
-- | VkDisplayKHR - Opaque handle to a display object
--
-- = See Also
--
-- No cross-references are available
type VkDisplayKHR = Ptr VkDisplayKHR_T

data VkDisplayModeCreateFlagsKHR

data VkDisplayModeCreateInfoKHR

-- | Dummy data to tag the 'Ptr' with
data VkDisplayModeKHR_T
-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- No cross-references are available
type VkDisplayModeKHR = Ptr VkDisplayModeKHR_T

data VkDisplayModeParametersKHR

data VkDisplayModePropertiesKHR

data VkDisplayPlaneAlphaFlagBitsKHR

-- | VkDisplayPlaneAlphaFlagsKHR - Bitmask of VkDisplayPlaneAlphaFlagBitsKHR
--
-- = Description
--
-- 'VkDisplayPlaneAlphaFlagsKHR' is a bitmask type for setting a mask of
-- zero or more 'VkDisplayPlaneAlphaFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type VkDisplayPlaneAlphaFlagsKHR = VkDisplayPlaneAlphaFlagBitsKHR

data VkDisplayPlaneCapabilitiesKHR

data VkDisplayPlanePropertiesKHR

data VkDisplayPropertiesKHR

data VkDisplaySurfaceCreateFlagsKHR

data VkDisplaySurfaceCreateInfoKHR

type FN_vkCreateDisplayModeKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult
type PFN_vkCreateDisplayModeKHR = FunPtr FN_vkCreateDisplayModeKHR

type FN_vkCreateDisplayPlaneSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateDisplayPlaneSurfaceKHR = FunPtr FN_vkCreateDisplayPlaneSurfaceKHR

type FN_vkGetDisplayModePropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult
type PFN_vkGetDisplayModePropertiesKHR = FunPtr FN_vkGetDisplayModePropertiesKHR

type FN_vkGetDisplayPlaneCapabilitiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult
type PFN_vkGetDisplayPlaneCapabilitiesKHR = FunPtr FN_vkGetDisplayPlaneCapabilitiesKHR

type FN_vkGetDisplayPlaneSupportedDisplaysKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult
type PFN_vkGetDisplayPlaneSupportedDisplaysKHR = FunPtr FN_vkGetDisplayPlaneSupportedDisplaysKHR

type FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR = FunPtr FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR

type FN_vkGetPhysicalDeviceDisplayPropertiesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPropertiesKHR = FunPtr FN_vkGetPhysicalDeviceDisplayPropertiesKHR
