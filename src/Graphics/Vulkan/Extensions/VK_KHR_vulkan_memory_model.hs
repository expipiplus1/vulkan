{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_vulkan_memory_model
  ( withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
  , fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
  , PhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( VkPhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
  )


-- No documentation found for TopLevel "PhysicalDeviceVulkanMemoryModelFeaturesKHR"
data PhysicalDeviceVulkanMemoryModelFeaturesKHR = PhysicalDeviceVulkanMemoryModelFeaturesKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModel"
  vkVulkanMemoryModel :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelDeviceScope"
  vkVulkanMemoryModelDeviceScope :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelAvailabilityVisibilityChains"
  vkVulkanMemoryModelAvailabilityVisibilityChains :: Bool
  }
  deriving (Show, Eq)
withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR :: PhysicalDeviceVulkanMemoryModelFeaturesKHR -> (VkPhysicalDeviceVulkanMemoryModelFeaturesKHR -> IO a) -> IO a
withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: PhysicalDeviceVulkanMemoryModelFeaturesKHR)) (\pPNext -> cont (VkPhysicalDeviceVulkanMemoryModelFeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR pPNext (boolToBool32 (vkVulkanMemoryModel (from :: PhysicalDeviceVulkanMemoryModelFeaturesKHR))) (boolToBool32 (vkVulkanMemoryModelDeviceScope (from :: PhysicalDeviceVulkanMemoryModelFeaturesKHR))) (boolToBool32 (vkVulkanMemoryModelAvailabilityVisibilityChains (from :: PhysicalDeviceVulkanMemoryModelFeaturesKHR)))))
fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR -> IO PhysicalDeviceVulkanMemoryModelFeaturesKHR
fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR c = PhysicalDeviceVulkanMemoryModelFeaturesKHR <$> -- Univalued Member elided
                                                                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkVulkanMemoryModel (c :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkVulkanMemoryModelDeviceScope (c :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR)))
                                                                                                     <*> pure (bool32ToBool (vkVulkanMemoryModelAvailabilityVisibilityChains (c :: VkPhysicalDeviceVulkanMemoryModelFeaturesKHR)))
instance Zero PhysicalDeviceVulkanMemoryModelFeaturesKHR where
  zero = PhysicalDeviceVulkanMemoryModelFeaturesKHR Nothing
                                                    False
                                                    False
                                                    False
