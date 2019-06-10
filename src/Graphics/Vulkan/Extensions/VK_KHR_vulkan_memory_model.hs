{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_vulkan_memory_model
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  PhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , 
#endif
  pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  )



#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkPhysicalDeviceVulkanMemoryModelFeaturesKHR"
data PhysicalDeviceVulkanMemoryModelFeaturesKHR = PhysicalDeviceVulkanMemoryModelFeaturesKHR
  { -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModel"
  vulkanMemoryModel :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelDeviceScope"
  vulkanMemoryModelDeviceScope :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelAvailabilityVisibilityChains"
  vulkanMemoryModelAvailabilityVisibilityChains :: Bool
  }
  deriving (Show, Eq)

instance Zero PhysicalDeviceVulkanMemoryModelFeaturesKHR where
  zero = PhysicalDeviceVulkanMemoryModelFeaturesKHR Nothing
                                                    False
                                                    False
                                                    False

#endif

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME"
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION"
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: Integral a => a
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
