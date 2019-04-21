{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( HINSTANCE
  , HWND
  , VkWin32SurfaceCreateFlagsKHR
  , VkWin32SurfaceCreateInfoKHR
  , FN_vkCreateWin32SurfaceKHR
  , PFN_vkCreateWin32SurfaceKHR
  , FN_vkGetPhysicalDeviceWin32PresentationSupportKHR
  , PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR
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


-- No documentation found for TopLevel "HINSTANCE"
type HINSTANCE = Ptr ()
  

-- No documentation found for TopLevel "HWND"
type HWND = Ptr ()
  

data VkWin32SurfaceCreateFlagsKHR

data VkWin32SurfaceCreateInfoKHR

type FN_vkCreateWin32SurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateWin32SurfaceKHR = FunPtr FN_vkCreateWin32SurfaceKHR

type FN_vkGetPhysicalDeviceWin32PresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32
type PFN_vkGetPhysicalDeviceWin32PresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceWin32PresentationSupportKHR
