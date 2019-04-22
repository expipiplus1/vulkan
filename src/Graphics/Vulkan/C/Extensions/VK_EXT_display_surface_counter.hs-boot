{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT
  , VkSurfaceCounterFlagBitsEXT
  , VkSurfaceCounterFlagsEXT
  , FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT
  , PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT
  ) where

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
  ( VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data VkSurfaceCapabilities2EXT

data VkSurfaceCounterFlagBitsEXT

-- | VkSurfaceCounterFlagsEXT - Bitmask of VkSurfaceCounterFlagBitsEXT
--
-- = Description
--
-- 'VkSurfaceCounterFlagsEXT' is a bitmask type for setting a mask of zero
-- or more 'VkSurfaceCounterFlagBitsEXT'.
--
-- = See Also
--
-- 'VkSurfaceCapabilities2EXT', 'VkSurfaceCounterFlagBitsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.VkSwapchainCounterCreateInfoEXT'
type VkSurfaceCounterFlagsEXT = VkSurfaceCounterFlagBitsEXT

type FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfaceCapabilities2EXT = FunPtr FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT
