{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_vulkan_memory_model
  ( withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
  , fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
  , PhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  ) where

import Data.String
  ( IsString
  )
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
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME
  , pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  )



-- | VkPhysicalDeviceVulkanMemoryModelFeaturesKHR - Structure describing
-- features supported by VK_KHR_vulkan_memory_model
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data PhysicalDeviceVulkanMemoryModelFeaturesKHR = PhysicalDeviceVulkanMemoryModelFeaturesKHR
  { -- Univalued member elided
  -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModel"
  vulkanMemoryModel :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelDeviceScope"
  vulkanMemoryModelDeviceScope :: Bool
  , -- No documentation found for Nested "PhysicalDeviceVulkanMemoryModelFeaturesKHR" "vulkanMemoryModelAvailabilityVisibilityChains"
  vulkanMemoryModelAvailabilityVisibilityChains :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceVulkanMemoryModelFeaturesKHR' and
-- marshal a 'PhysicalDeviceVulkanMemoryModelFeaturesKHR' into it. The 'VkPhysicalDeviceVulkanMemoryModelFeaturesKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR :: PhysicalDeviceVulkanMemoryModelFeaturesKHR -> (VkPhysicalDeviceVulkanMemoryModelFeaturesKHR -> IO a) -> IO a
withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: PhysicalDeviceVulkanMemoryModelFeaturesKHR)) (\pPNext -> cont (VkPhysicalDeviceVulkanMemoryModelFeaturesKHR VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR pPNext (boolToBool32 (vulkanMemoryModel (marshalled :: PhysicalDeviceVulkanMemoryModelFeaturesKHR))) (boolToBool32 (vulkanMemoryModelDeviceScope (marshalled :: PhysicalDeviceVulkanMemoryModelFeaturesKHR))) (boolToBool32 (vulkanMemoryModelAvailabilityVisibilityChains (marshalled :: PhysicalDeviceVulkanMemoryModelFeaturesKHR)))))

-- | A function to read a 'VkPhysicalDeviceVulkanMemoryModelFeaturesKHR' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceVulkanMemoryModelFeaturesKHR'.
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


-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME"
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION"
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: Integral a => a
pattern KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION
