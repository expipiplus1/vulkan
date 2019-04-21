{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2
  , VkImageFormatProperties2
  , VkPhysicalDeviceFeatures2
  , VkPhysicalDeviceImageFormatInfo2
  , VkPhysicalDeviceMemoryProperties2
  , VkPhysicalDeviceProperties2
  , VkPhysicalDeviceSparseImageFormatInfo2
  , VkQueueFamilyProperties2
  , VkSparseImageFormatProperties2
  , FN_vkGetPhysicalDeviceFeatures2
  , PFN_vkGetPhysicalDeviceFeatures2
  , FN_vkGetPhysicalDeviceFormatProperties2
  , PFN_vkGetPhysicalDeviceFormatProperties2
  , FN_vkGetPhysicalDeviceImageFormatProperties2
  , PFN_vkGetPhysicalDeviceImageFormatProperties2
  , FN_vkGetPhysicalDeviceMemoryProperties2
  , PFN_vkGetPhysicalDeviceMemoryProperties2
  , FN_vkGetPhysicalDeviceProperties2
  , PFN_vkGetPhysicalDeviceProperties2
  , FN_vkGetPhysicalDeviceQueueFamilyProperties2
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties2
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties2
  , PFN_vkGetPhysicalDeviceSparseImageFormatProperties2
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
  ( VkFormat
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkPhysicalDevice
  )


data VkFormatProperties2

data VkImageFormatProperties2

data VkPhysicalDeviceFeatures2

data VkPhysicalDeviceImageFormatInfo2

data VkPhysicalDeviceMemoryProperties2

data VkPhysicalDeviceProperties2

data VkPhysicalDeviceSparseImageFormatInfo2

data VkQueueFamilyProperties2

data VkSparseImageFormatProperties2

type FN_vkGetPhysicalDeviceFeatures2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures2 = FunPtr FN_vkGetPhysicalDeviceFeatures2

type FN_vkGetPhysicalDeviceFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceFormatProperties2

type FN_vkGetPhysicalDeviceImageFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties2

type FN_vkGetPhysicalDeviceMemoryProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties2 = FunPtr FN_vkGetPhysicalDeviceMemoryProperties2

type FN_vkGetPhysicalDeviceProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceProperties2 = FunPtr FN_vkGetPhysicalDeviceProperties2

type FN_vkGetPhysicalDeviceQueueFamilyProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties2 = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties2

type FN_vkGetPhysicalDeviceSparseImageFormatProperties2 = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()
type PFN_vkGetPhysicalDeviceSparseImageFormatProperties2 = FunPtr FN_vkGetPhysicalDeviceSparseImageFormatProperties2
