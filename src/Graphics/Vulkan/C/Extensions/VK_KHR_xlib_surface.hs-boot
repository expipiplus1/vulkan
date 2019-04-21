{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display
  , VisualID
  , VkXlibSurfaceCreateFlagsKHR
  , VkXlibSurfaceCreateInfoKHR
  , Window
  , FN_vkCreateXlibSurfaceKHR
  , PFN_vkCreateXlibSurfaceKHR
  , FN_vkGetPhysicalDeviceXlibPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR
  ) where

import Data.Word
  ( Word32
  , Word64
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


data Display

-- No documentation found for TopLevel "VisualID"
type VisualID = Word64
  

data VkXlibSurfaceCreateFlagsKHR

data VkXlibSurfaceCreateInfoKHR

-- No documentation found for TopLevel "Window"
type Window = Word64
  

type FN_vkCreateXlibSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateXlibSurfaceKHR = FunPtr FN_vkCreateXlibSurfaceKHR

type FN_vkGetPhysicalDeviceXlibPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32
type PFN_vkGetPhysicalDeviceXlibPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceXlibPresentationSupportKHR
