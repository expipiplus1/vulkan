{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  , VkPhysicalDevice16BitStorageFeatures(..)
  ) where

import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES = VkStructureType 1000083000
-- | VkPhysicalDevice16BitStorageFeatures - Structure describing features
-- supported by VK_KHR_16bit_storage
data VkPhysicalDevice16BitStorageFeatures = VkPhysicalDevice16BitStorageFeatures
  { -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "vkStorageBuffer16BitAccess"
  vkStorageBuffer16BitAccess :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "vkUniformAndStorageBuffer16BitAccess"
  vkUniformAndStorageBuffer16BitAccess :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "vkStoragePushConstant16"
  vkStoragePushConstant16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "vkStorageInputOutput16"
  vkStorageInputOutput16 :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDevice16BitStorageFeatures where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDevice16BitStorageFeatures <$> peek (ptr `plusPtr` 0)
                                                  <*> peek (ptr `plusPtr` 8)
                                                  <*> peek (ptr `plusPtr` 16)
                                                  <*> peek (ptr `plusPtr` 20)
                                                  <*> peek (ptr `plusPtr` 24)
                                                  <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 16) (vkStorageBuffer16BitAccess (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 20) (vkUniformAndStorageBuffer16BitAccess (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 24) (vkStoragePushConstant16 (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 28) (vkStorageInputOutput16 (poked :: VkPhysicalDevice16BitStorageFeatures))
