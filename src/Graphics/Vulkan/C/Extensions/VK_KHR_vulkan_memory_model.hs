{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( VkPhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- No documentation found for TopLevel "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR"
data VkPhysicalDeviceVulkanMemoryModelFeaturesKHR = VkPhysicalDeviceVulkanMemoryModelFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModel"
  vkVulkanMemoryModel :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelDeviceScope"
  vkVulkanMemoryModelDeviceScope :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelAvailabilityVisibilityChains"
  vkVulkanMemoryModelAvailabilityVisibilityChains :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceVulkanMemoryModelFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceVulkanMemoryModelFeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                          <*> peek (ptr `plusPtr` 8)
                                                          <*> peek (ptr `plusPtr` 16)
                                                          <*> peek (ptr `plusPtr` 20)
                                                          <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkVulkanMemoryModel (poked :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
                *> poke (ptr `plusPtr` 20) (vkVulkanMemoryModelDeviceScope (poked :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
                *> poke (ptr `plusPtr` 24) (vkVulkanMemoryModelAvailabilityVisibilityChains (poked :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME"
pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = "VK_KHR_vulkan_memory_model"
-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION"
pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: Integral a => a
pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR = VkStructureType 1000211000
