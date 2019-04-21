{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR
  , VkDisplayPlaneCapabilities2KHR
  , VkDisplayPlaneInfo2KHR
  , VkDisplayPlaneProperties2KHR
  , VkDisplayProperties2KHR
  , FN_vkGetDisplayModeProperties2KHR
  , PFN_vkGetDisplayModeProperties2KHR
  , FN_vkGetDisplayPlaneCapabilities2KHR
  , PFN_vkGetDisplayPlaneCapabilities2KHR
  , FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , FN_vkGetPhysicalDeviceDisplayProperties2KHR
  , PFN_vkGetPhysicalDeviceDisplayProperties2KHR
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
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )


data VkDisplayModeProperties2KHR

data VkDisplayPlaneCapabilities2KHR

data VkDisplayPlaneInfo2KHR

data VkDisplayPlaneProperties2KHR

data VkDisplayProperties2KHR

type FN_vkGetDisplayModeProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult
type PFN_vkGetDisplayModeProperties2KHR = FunPtr FN_vkGetDisplayModeProperties2KHR

type FN_vkGetDisplayPlaneCapabilities2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult
type PFN_vkGetDisplayPlaneCapabilities2KHR = FunPtr FN_vkGetDisplayPlaneCapabilities2KHR

type FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR = FunPtr FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR

type FN_vkGetPhysicalDeviceDisplayProperties2KHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceDisplayProperties2KHR = FunPtr FN_vkGetPhysicalDeviceDisplayProperties2KHR
