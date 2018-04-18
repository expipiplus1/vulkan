{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2
  ( pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
  , pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
  , vkGetPhysicalDeviceFeatures2KHR
  , vkGetPhysicalDeviceProperties2KHR
  , vkGetPhysicalDeviceFormatProperties2KHR
  , vkGetPhysicalDeviceImageFormatProperties2KHR
  , vkGetPhysicalDeviceQueueFamilyProperties2KHR
  , vkGetPhysicalDeviceMemoryProperties2KHR
  , vkGetPhysicalDeviceSparseImageFormatProperties2KHR
  , VkPhysicalDeviceFeatures2KHR
  , pattern VkPhysicalDeviceFeatures2KHR
  , VkPhysicalDeviceProperties2KHR
  , pattern VkPhysicalDeviceProperties2KHR
  , VkFormatProperties2KHR
  , pattern VkFormatProperties2KHR
  , VkImageFormatProperties2KHR
  , pattern VkImageFormatProperties2KHR
  , VkPhysicalDeviceImageFormatInfo2KHR
  , pattern VkPhysicalDeviceImageFormatInfo2KHR
  , VkQueueFamilyProperties2KHR
  , pattern VkQueueFamilyProperties2KHR
  , VkPhysicalDeviceMemoryProperties2KHR
  , pattern VkPhysicalDeviceMemoryProperties2KHR
  , VkSparseImageFormatProperties2KHR
  , pattern VkSparseImageFormatProperties2KHR
  , VkPhysicalDeviceSparseImageFormatInfo2KHR
  , pattern VkPhysicalDeviceSparseImageFormatInfo2KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkResult(..)
  , VkFormat(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkSampleCountFlagBits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkQueueFamilyProperties(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageFormatProperties(..)
  , VkFormatProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkSparseImageFormatProperties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , VkSparseImageFormatProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , vkGetPhysicalDeviceSparseImageFormatProperties2
  , VkPhysicalDeviceMemoryProperties2(..)
  , vkGetPhysicalDeviceMemoryProperties2
  , VkQueueFamilyProperties2(..)
  , vkGetPhysicalDeviceQueueFamilyProperties2
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , vkGetPhysicalDeviceImageFormatProperties2
  , VkFormatProperties2(..)
  , vkGetPhysicalDeviceFormatProperties2
  , VkPhysicalDeviceProperties2(..)
  , vkGetPhysicalDeviceProperties2
  , VkPhysicalDeviceFeatures2(..)
  , vkGetPhysicalDeviceFeatures2
  )


pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 1
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_physical_device_properties2"
vkGetPhysicalDeviceFeatures2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()
vkGetPhysicalDeviceFeatures2KHR = vkGetPhysicalDeviceFeatures2
vkGetPhysicalDeviceProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()
vkGetPhysicalDeviceProperties2KHR = vkGetPhysicalDeviceProperties2
vkGetPhysicalDeviceFormatProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()
vkGetPhysicalDeviceFormatProperties2KHR = vkGetPhysicalDeviceFormatProperties2
vkGetPhysicalDeviceImageFormatProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties2KHR = vkGetPhysicalDeviceImageFormatProperties2
vkGetPhysicalDeviceQueueFamilyProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties2KHR = vkGetPhysicalDeviceQueueFamilyProperties2
vkGetPhysicalDeviceMemoryProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()
vkGetPhysicalDeviceMemoryProperties2KHR = vkGetPhysicalDeviceMemoryProperties2
vkGetPhysicalDeviceSparseImageFormatProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()
vkGetPhysicalDeviceSparseImageFormatProperties2KHR = vkGetPhysicalDeviceSparseImageFormatProperties2
type VkPhysicalDeviceFeatures2KHR = VkPhysicalDeviceFeatures2


pattern VkPhysicalDeviceFeatures2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("features" ::: VkPhysicalDeviceFeatures) -> VkPhysicalDeviceFeatures2KHR
pattern VkPhysicalDeviceFeatures2KHR vkSType vkNext vkFeatures = VkPhysicalDeviceFeatures2 vkSType vkNext vkFeatures
type VkPhysicalDeviceProperties2KHR = VkPhysicalDeviceProperties2


pattern VkPhysicalDeviceProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("properties" ::: VkPhysicalDeviceProperties) -> VkPhysicalDeviceProperties2KHR
pattern VkPhysicalDeviceProperties2KHR vkSType vkNext vkProperties = VkPhysicalDeviceProperties2 vkSType vkNext vkProperties
type VkFormatProperties2KHR = VkFormatProperties2


pattern VkFormatProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("formatProperties" ::: VkFormatProperties) -> VkFormatProperties2KHR
pattern VkFormatProperties2KHR vkSType vkNext vkFormatProperties = VkFormatProperties2 vkSType vkNext vkFormatProperties
type VkImageFormatProperties2KHR = VkImageFormatProperties2


pattern VkImageFormatProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("imageFormatProperties" ::: VkImageFormatProperties) -> VkImageFormatProperties2KHR
pattern VkImageFormatProperties2KHR vkSType vkNext vkImageFormatProperties = VkImageFormatProperties2 vkSType vkNext vkImageFormatProperties
type VkPhysicalDeviceImageFormatInfo2KHR = VkPhysicalDeviceImageFormatInfo2


pattern VkPhysicalDeviceImageFormatInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> VkPhysicalDeviceImageFormatInfo2KHR
pattern VkPhysicalDeviceImageFormatInfo2KHR vkSType vkNext vkFormat vkType vkTiling vkUsage vkFlags = VkPhysicalDeviceImageFormatInfo2 vkSType vkNext vkFormat vkType vkTiling vkUsage vkFlags
type VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2


pattern VkQueueFamilyProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("queueFamilyProperties" ::: VkQueueFamilyProperties) -> VkQueueFamilyProperties2KHR
pattern VkQueueFamilyProperties2KHR vkSType vkNext vkQueueFamilyProperties = VkQueueFamilyProperties2 vkSType vkNext vkQueueFamilyProperties
type VkPhysicalDeviceMemoryProperties2KHR = VkPhysicalDeviceMemoryProperties2


pattern VkPhysicalDeviceMemoryProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("memoryProperties" ::: VkPhysicalDeviceMemoryProperties) -> VkPhysicalDeviceMemoryProperties2KHR
pattern VkPhysicalDeviceMemoryProperties2KHR vkSType vkNext vkMemoryProperties = VkPhysicalDeviceMemoryProperties2 vkSType vkNext vkMemoryProperties
type VkSparseImageFormatProperties2KHR = VkSparseImageFormatProperties2


pattern VkSparseImageFormatProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("properties" ::: VkSparseImageFormatProperties) -> VkSparseImageFormatProperties2KHR
pattern VkSparseImageFormatProperties2KHR vkSType vkNext vkProperties = VkSparseImageFormatProperties2 vkSType vkNext vkProperties
type VkPhysicalDeviceSparseImageFormatInfo2KHR = VkPhysicalDeviceSparseImageFormatInfo2


pattern VkPhysicalDeviceSparseImageFormatInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> VkPhysicalDeviceSparseImageFormatInfo2KHR
pattern VkPhysicalDeviceSparseImageFormatInfo2KHR vkSType vkNext vkFormat vkType vkSamples vkUsage vkTiling = VkPhysicalDeviceSparseImageFormatInfo2 vkSType vkNext vkFormat vkType vkSamples vkUsage vkTiling
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
