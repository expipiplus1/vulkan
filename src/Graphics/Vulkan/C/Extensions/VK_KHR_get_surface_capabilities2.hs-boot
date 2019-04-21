{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR
  , VkSurfaceCapabilities2KHR
  , VkSurfaceFormat2KHR
  , FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR
  , PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR
  , FN_vkGetPhysicalDeviceSurfaceFormats2KHR
  , PFN_vkGetPhysicalDeviceSurfaceFormats2KHR
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
  ( VkPhysicalDevice
  )


data VkPhysicalDeviceSurfaceInfo2KHR

data VkSurfaceCapabilities2KHR

data VkSurfaceFormat2KHR

type FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfaceCapabilities2KHR = FunPtr FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR

type FN_vkGetPhysicalDeviceSurfaceFormats2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormat2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfaceFormats2KHR = FunPtr FN_vkGetPhysicalDeviceSurfaceFormats2KHR
