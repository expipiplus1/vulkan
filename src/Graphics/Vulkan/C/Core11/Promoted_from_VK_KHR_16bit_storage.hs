{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage
  ( VkPhysicalDevice16BitStorageFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  ) where

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


-- No documentation found for TopLevel "VkPhysicalDevice16BitStorageFeatures"
data VkPhysicalDevice16BitStorageFeatures = VkPhysicalDevice16BitStorageFeatures
  { -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "storageBuffer16BitAccess"
  vkStorageBuffer16BitAccess :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "uniformAndStorageBuffer16BitAccess"
  vkUniformAndStorageBuffer16BitAccess :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "storagePushConstant16"
  vkStoragePushConstant16 :: VkBool32
  , -- No documentation found for Nested "VkPhysicalDevice16BitStorageFeatures" "storageInputOutput16"
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

instance Zero VkPhysicalDevice16BitStorageFeatures where
  zero = VkPhysicalDevice16BitStorageFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
                                              zero
                                              zero
                                              zero
                                              zero
                                              zero

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES = VkStructureType 1000083000
