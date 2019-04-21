{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR
  , VkXcbSurfaceCreateInfoKHR
  , Xcb_connection_t
  , Xcb_visualid_t
  , Xcb_window_t
  , FN_vkCreateXcbSurfaceKHR
  , PFN_vkCreateXcbSurfaceKHR
  , FN_vkGetPhysicalDeviceXcbPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR
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
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  , VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data VkXcbSurfaceCreateFlagsKHR

data VkXcbSurfaceCreateInfoKHR

-- | Opaque data
data Xcb_connection_t

-- No documentation found for TopLevel "Xcb_visualid_t"
type Xcb_visualid_t = Word32
  

-- No documentation found for TopLevel "Xcb_window_t"
type Xcb_window_t = Word32
  

type FN_vkCreateXcbSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateXcbSurfaceKHR = FunPtr FN_vkCreateXcbSurfaceKHR

type FN_vkGetPhysicalDeviceXcbPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32
type PFN_vkGetPhysicalDeviceXcbPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceXcbPresentationSupportKHR
