{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_vulkan_memory_model  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
                                                     , PhysicalDeviceVulkanMemoryModelFeaturesKHR
                                                     , KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
                                                     , pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
                                                     , KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                                                     , pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model (PhysicalDeviceVulkanMemoryModelFeatures)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES


-- No documentation found for TopLevel "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR"
type PhysicalDeviceVulkanMemoryModelFeaturesKHR = PhysicalDeviceVulkanMemoryModelFeatures


type KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION"
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3


type KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = "VK_KHR_vulkan_memory_model"

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME"
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = "VK_KHR_vulkan_memory_model"

