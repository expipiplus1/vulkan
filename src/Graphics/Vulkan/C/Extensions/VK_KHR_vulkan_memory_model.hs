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
  , Zero(..)
  )


-- | VkPhysicalDeviceVulkanMemoryModelFeaturesKHR - Structure describing
-- features supported by VK_KHR_vulkan_memory_model
--
-- = Description
--
-- Unresolved directive in VkPhysicalDeviceVulkanMemoryModelFeaturesKHR.txt
-- -
-- include::..\/validity\/structs\/VkPhysicalDeviceVulkanMemoryModelFeaturesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDeviceVulkanMemoryModelFeaturesKHR = VkPhysicalDeviceVulkanMemoryModelFeaturesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @vulkanMemoryModel@ indicates whether the Vulkan Memory Model is
  -- supported, as defined in
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-model Vulkan Memory Model>.
  -- This also indicates whether shader modules /can/ declare the
  -- @VulkanMemoryModelKHR@ capability.
  vkVulkanMemoryModel :: VkBool32
  , -- | @vulkanMemoryModelDeviceScope@ indicates whether the Vulkan Memory Model
  -- can use @Device@ scope synchronization. This also indicates whether
  -- shader modules /can/ declare the @VulkanMemoryModelDeviceScopeKHR@
  -- capability.
  vkVulkanMemoryModelDeviceScope :: VkBool32
  , -- | @vulkanMemoryModelAvailabilityVisibilityChains@ indicates whether the
  -- Vulkan Memory Model can use
  -- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-model-availability-visibility availability and visibility chains>
  -- with more than one element.
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

instance Zero VkPhysicalDeviceVulkanMemoryModelFeaturesKHR where
  zero = VkPhysicalDeviceVulkanMemoryModelFeaturesKHR zero
                                                      zero
                                                      zero
                                                      zero
                                                      zero
-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME"
pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_VULKAN_MEMORY_MODEL_EXTENSION_NAME = "VK_KHR_vulkan_memory_model"
-- No documentation found for TopLevel "VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION"
pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION :: Integral a => a
pattern VK_KHR_VULKAN_MEMORY_MODEL_SPEC_VERSION = 3
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR = VkStructureType 1000211000
