{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2
  ( FormatProperties2KHR
  , ImageFormatProperties2KHR
  , PhysicalDeviceFeatures2KHR
  , PhysicalDeviceImageFormatInfo2KHR
  , PhysicalDeviceMemoryProperties2KHR
  , PhysicalDeviceProperties2KHR
  , PhysicalDeviceSparseImageFormatInfo2KHR
  , QueueFamilyProperties2KHR
  , SparseImageFormatProperties2KHR
  , getPhysicalDeviceFeatures2KHR
  , getPhysicalDeviceFormatProperties2KHR
  , getPhysicalDeviceImageFormatProperties2KHR
  , getPhysicalDeviceMemoryProperties2KHR
  , getPhysicalDeviceProperties2KHR
  , getPhysicalDeviceQueueFamilyProperties2KHR
  , getPhysicalDeviceSparseImageFormatProperties2KHR
  , pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
  , pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  ) where

import Data.Vector
  ( Vector
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.Core10.Core
  ( Format
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( PhysicalDevice(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( FormatProperties2(..)
  , ImageFormatProperties2(..)
  , PhysicalDeviceFeatures2(..)
  , PhysicalDeviceImageFormatInfo2(..)
  , PhysicalDeviceMemoryProperties2(..)
  , PhysicalDeviceProperties2(..)
  , PhysicalDeviceSparseImageFormatInfo2(..)
  , QueueFamilyProperties2(..)
  , SparseImageFormatProperties2(..)
  , getPhysicalDeviceFeatures2
  , getPhysicalDeviceFormatProperties2
  , getPhysicalDeviceImageFormatProperties2
  , getPhysicalDeviceMemoryProperties2
  , getPhysicalDeviceProperties2
  , getPhysicalDeviceQueueFamilyProperties2
  , getPhysicalDeviceSparseImageFormatProperties2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2
  ( pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
  )


type FormatProperties2KHR = FormatProperties2
-- TODO: Pattern constructor alias)
type ImageFormatProperties2KHR = ImageFormatProperties2
-- TODO: Pattern constructor alias)
type PhysicalDeviceFeatures2KHR = PhysicalDeviceFeatures2
-- TODO: Pattern constructor alias)
type PhysicalDeviceImageFormatInfo2KHR = PhysicalDeviceImageFormatInfo2
-- TODO: Pattern constructor alias)
type PhysicalDeviceMemoryProperties2KHR = PhysicalDeviceMemoryProperties2
-- TODO: Pattern constructor alias)
type PhysicalDeviceProperties2KHR = PhysicalDeviceProperties2
-- TODO: Pattern constructor alias)
type PhysicalDeviceSparseImageFormatInfo2KHR = PhysicalDeviceSparseImageFormatInfo2
-- TODO: Pattern constructor alias)
type QueueFamilyProperties2KHR = QueueFamilyProperties2
-- TODO: Pattern constructor alias)
type SparseImageFormatProperties2KHR = SparseImageFormatProperties2
-- TODO: Pattern constructor alias)
getPhysicalDeviceFeatures2KHR :: PhysicalDevice ->  IO (PhysicalDeviceFeatures2)
getPhysicalDeviceFeatures2KHR = getPhysicalDeviceFeatures2
getPhysicalDeviceFormatProperties2KHR :: PhysicalDevice ->  Format ->  IO (FormatProperties2)
getPhysicalDeviceFormatProperties2KHR = getPhysicalDeviceFormatProperties2
getPhysicalDeviceImageFormatProperties2KHR :: PhysicalDevice ->  PhysicalDeviceImageFormatInfo2 ->  IO (ImageFormatProperties2)
getPhysicalDeviceImageFormatProperties2KHR = getPhysicalDeviceImageFormatProperties2
getPhysicalDeviceMemoryProperties2KHR :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties2)
getPhysicalDeviceMemoryProperties2KHR = getPhysicalDeviceMemoryProperties2
getPhysicalDeviceProperties2KHR :: PhysicalDevice ->  IO (PhysicalDeviceProperties2)
getPhysicalDeviceProperties2KHR = getPhysicalDeviceProperties2
getPhysicalDeviceQueueFamilyProperties2KHR :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties2)
getPhysicalDeviceQueueFamilyProperties2KHR = getPhysicalDeviceQueueFamilyProperties2
getPhysicalDeviceSparseImageFormatProperties2KHR :: PhysicalDevice ->  PhysicalDeviceSparseImageFormatInfo2 ->  Word32 ->  IO (Vector SparseImageFormatProperties2)
getPhysicalDeviceSparseImageFormatProperties2KHR = getPhysicalDeviceSparseImageFormatProperties2
