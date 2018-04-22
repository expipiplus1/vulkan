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
  ( VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkQueueFamilyProperties(..)
  , VkSampleCountFlagBits(..)
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkPhysicalDevice
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkSparseImageFormatProperties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkSparseImageFormatProperties2(..)
  , vkGetPhysicalDeviceFeatures2
  , vkGetPhysicalDeviceFormatProperties2
  , vkGetPhysicalDeviceImageFormatProperties2
  , vkGetPhysicalDeviceMemoryProperties2
  , vkGetPhysicalDeviceProperties2
  , vkGetPhysicalDeviceQueueFamilyProperties2
  , vkGetPhysicalDeviceSparseImageFormatProperties2
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  )


-- No documentation found for TopLevel "VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION"
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME"
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_physical_device_properties2"
-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures2KHR"
vkGetPhysicalDeviceFeatures2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()
vkGetPhysicalDeviceFeatures2KHR = vkGetPhysicalDeviceFeatures2
-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties2KHR"
vkGetPhysicalDeviceProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()
vkGetPhysicalDeviceProperties2KHR = vkGetPhysicalDeviceProperties2
-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties2KHR"
vkGetPhysicalDeviceFormatProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()
vkGetPhysicalDeviceFormatProperties2KHR = vkGetPhysicalDeviceFormatProperties2
-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties2KHR"
vkGetPhysicalDeviceImageFormatProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties2KHR = vkGetPhysicalDeviceImageFormatProperties2
-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
vkGetPhysicalDeviceQueueFamilyProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties2KHR = vkGetPhysicalDeviceQueueFamilyProperties2
-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties2KHR"
vkGetPhysicalDeviceMemoryProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()
vkGetPhysicalDeviceMemoryProperties2KHR = vkGetPhysicalDeviceMemoryProperties2
-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
vkGetPhysicalDeviceSparseImageFormatProperties2KHR :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()
vkGetPhysicalDeviceSparseImageFormatProperties2KHR = vkGetPhysicalDeviceSparseImageFormatProperties2
-- No documentation found for TopLevel "VkPhysicalDeviceFeatures2KHR"
type VkPhysicalDeviceFeatures2KHR = VkPhysicalDeviceFeatures2


-- No documentation found for TopLevel "VkPhysicalDeviceFeatures2KHR"
pattern VkPhysicalDeviceFeatures2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("features" ::: VkPhysicalDeviceFeatures) -> VkPhysicalDeviceFeatures2KHR
pattern VkPhysicalDeviceFeatures2KHR vkSType vkPNext vkFeatures = VkPhysicalDeviceFeatures2 vkSType vkPNext vkFeatures
-- No documentation found for TopLevel "VkPhysicalDeviceProperties2KHR"
type VkPhysicalDeviceProperties2KHR = VkPhysicalDeviceProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceProperties2KHR"
pattern VkPhysicalDeviceProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("properties" ::: VkPhysicalDeviceProperties) -> VkPhysicalDeviceProperties2KHR
pattern VkPhysicalDeviceProperties2KHR vkSType vkPNext vkProperties = VkPhysicalDeviceProperties2 vkSType vkPNext vkProperties
-- No documentation found for TopLevel "VkFormatProperties2KHR"
type VkFormatProperties2KHR = VkFormatProperties2


-- No documentation found for TopLevel "VkFormatProperties2KHR"
pattern VkFormatProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("formatProperties" ::: VkFormatProperties) -> VkFormatProperties2KHR
pattern VkFormatProperties2KHR vkSType vkPNext vkFormatProperties = VkFormatProperties2 vkSType vkPNext vkFormatProperties
-- No documentation found for TopLevel "VkImageFormatProperties2KHR"
type VkImageFormatProperties2KHR = VkImageFormatProperties2


-- No documentation found for TopLevel "VkImageFormatProperties2KHR"
pattern VkImageFormatProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("imageFormatProperties" ::: VkImageFormatProperties) -> VkImageFormatProperties2KHR
pattern VkImageFormatProperties2KHR vkSType vkPNext vkImageFormatProperties = VkImageFormatProperties2 vkSType vkPNext vkImageFormatProperties
-- No documentation found for TopLevel "VkPhysicalDeviceImageFormatInfo2KHR"
type VkPhysicalDeviceImageFormatInfo2KHR = VkPhysicalDeviceImageFormatInfo2


-- No documentation found for TopLevel "VkPhysicalDeviceImageFormatInfo2KHR"
pattern VkPhysicalDeviceImageFormatInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> VkPhysicalDeviceImageFormatInfo2KHR
pattern VkPhysicalDeviceImageFormatInfo2KHR vkSType vkPNext vkFormat vkType vkTiling vkUsage vkFlags = VkPhysicalDeviceImageFormatInfo2 vkSType vkPNext vkFormat vkType vkTiling vkUsage vkFlags
-- No documentation found for TopLevel "VkQueueFamilyProperties2KHR"
type VkQueueFamilyProperties2KHR = VkQueueFamilyProperties2


-- No documentation found for TopLevel "VkQueueFamilyProperties2KHR"
pattern VkQueueFamilyProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("queueFamilyProperties" ::: VkQueueFamilyProperties) -> VkQueueFamilyProperties2KHR
pattern VkQueueFamilyProperties2KHR vkSType vkPNext vkQueueFamilyProperties = VkQueueFamilyProperties2 vkSType vkPNext vkQueueFamilyProperties
-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties2KHR"
type VkPhysicalDeviceMemoryProperties2KHR = VkPhysicalDeviceMemoryProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties2KHR"
pattern VkPhysicalDeviceMemoryProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("memoryProperties" ::: VkPhysicalDeviceMemoryProperties) -> VkPhysicalDeviceMemoryProperties2KHR
pattern VkPhysicalDeviceMemoryProperties2KHR vkSType vkPNext vkMemoryProperties = VkPhysicalDeviceMemoryProperties2 vkSType vkPNext vkMemoryProperties
-- No documentation found for TopLevel "VkSparseImageFormatProperties2KHR"
type VkSparseImageFormatProperties2KHR = VkSparseImageFormatProperties2


-- No documentation found for TopLevel "VkSparseImageFormatProperties2KHR"
pattern VkSparseImageFormatProperties2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("properties" ::: VkSparseImageFormatProperties) -> VkSparseImageFormatProperties2KHR
pattern VkSparseImageFormatProperties2KHR vkSType vkPNext vkProperties = VkSparseImageFormatProperties2 vkSType vkPNext vkProperties
-- No documentation found for TopLevel "VkPhysicalDeviceSparseImageFormatInfo2KHR"
type VkPhysicalDeviceSparseImageFormatInfo2KHR = VkPhysicalDeviceSparseImageFormatInfo2


-- No documentation found for TopLevel "VkPhysicalDeviceSparseImageFormatInfo2KHR"
pattern VkPhysicalDeviceSparseImageFormatInfo2KHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> VkPhysicalDeviceSparseImageFormatInfo2KHR
pattern VkPhysicalDeviceSparseImageFormatInfo2KHR vkSType vkPNext vkFormat vkType vkSamples vkUsage vkTiling = VkPhysicalDeviceSparseImageFormatInfo2 vkSType vkPNext vkFormat vkType vkSamples vkUsage vkTiling
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR"
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR = VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
