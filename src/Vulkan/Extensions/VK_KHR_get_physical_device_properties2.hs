{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_get_physical_device_properties2  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR
                                                                 , pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR
                                                                 , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR
                                                                 , getPhysicalDeviceFeatures2KHR
                                                                 , getPhysicalDeviceProperties2KHR
                                                                 , getPhysicalDeviceFormatProperties2KHR
                                                                 , getPhysicalDeviceImageFormatProperties2KHR
                                                                 , getPhysicalDeviceQueueFamilyProperties2KHR
                                                                 , getPhysicalDeviceMemoryProperties2KHR
                                                                 , getPhysicalDeviceSparseImageFormatProperties2KHR
                                                                 , PhysicalDeviceFeatures2KHR
                                                                 , PhysicalDeviceProperties2KHR
                                                                 , FormatProperties2KHR
                                                                 , ImageFormatProperties2KHR
                                                                 , PhysicalDeviceImageFormatInfo2KHR
                                                                 , QueueFamilyProperties2KHR
                                                                 , PhysicalDeviceMemoryProperties2KHR
                                                                 , SparseImageFormatProperties2KHR
                                                                 , PhysicalDeviceSparseImageFormatInfo2KHR
                                                                 , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
                                                                 , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION
                                                                 , KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                                                                 , pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME
                                                                 ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceFeatures2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceImageFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceMemoryProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceQueueFamilyProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (getPhysicalDeviceSparseImageFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (FormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (ImageFormatProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceFeatures2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceImageFormatInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceMemoryProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (PhysicalDeviceSparseImageFormatInfo2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (QueueFamilyProperties2)
import Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2 (SparseImageFormatProperties2)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2_KHR = STRUCTURE_TYPE_FORMAT_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2_KHR = STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2_KHR = STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2_KHR = STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2


-- No documentation found for TopLevel "vkGetPhysicalDeviceFeatures2KHR"
getPhysicalDeviceFeatures2KHR = getPhysicalDeviceFeatures2


-- No documentation found for TopLevel "vkGetPhysicalDeviceProperties2KHR"
getPhysicalDeviceProperties2KHR = getPhysicalDeviceProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceFormatProperties2KHR"
getPhysicalDeviceFormatProperties2KHR = getPhysicalDeviceFormatProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceImageFormatProperties2KHR"
getPhysicalDeviceImageFormatProperties2KHR = getPhysicalDeviceImageFormatProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceQueueFamilyProperties2KHR"
getPhysicalDeviceQueueFamilyProperties2KHR = getPhysicalDeviceQueueFamilyProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceMemoryProperties2KHR"
getPhysicalDeviceMemoryProperties2KHR = getPhysicalDeviceMemoryProperties2


-- No documentation found for TopLevel "vkGetPhysicalDeviceSparseImageFormatProperties2KHR"
getPhysicalDeviceSparseImageFormatProperties2KHR = getPhysicalDeviceSparseImageFormatProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceFeatures2KHR"
type PhysicalDeviceFeatures2KHR = PhysicalDeviceFeatures2


-- No documentation found for TopLevel "VkPhysicalDeviceProperties2KHR"
type PhysicalDeviceProperties2KHR = PhysicalDeviceProperties2


-- No documentation found for TopLevel "VkFormatProperties2KHR"
type FormatProperties2KHR = FormatProperties2


-- No documentation found for TopLevel "VkImageFormatProperties2KHR"
type ImageFormatProperties2KHR = ImageFormatProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceImageFormatInfo2KHR"
type PhysicalDeviceImageFormatInfo2KHR = PhysicalDeviceImageFormatInfo2


-- No documentation found for TopLevel "VkQueueFamilyProperties2KHR"
type QueueFamilyProperties2KHR = QueueFamilyProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceMemoryProperties2KHR"
type PhysicalDeviceMemoryProperties2KHR = PhysicalDeviceMemoryProperties2


-- No documentation found for TopLevel "VkSparseImageFormatProperties2KHR"
type SparseImageFormatProperties2KHR = SparseImageFormatProperties2


-- No documentation found for TopLevel "VkPhysicalDeviceSparseImageFormatInfo2KHR"
type PhysicalDeviceSparseImageFormatInfo2KHR = PhysicalDeviceSparseImageFormatInfo2


type KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION"
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_SPEC_VERSION = 2


type KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_physical_device_properties2"

-- No documentation found for TopLevel "VK_KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME"
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_GET_PHYSICAL_DEVICE_PROPERTIES_2_EXTENSION_NAME = "VK_KHR_get_physical_device_properties2"

