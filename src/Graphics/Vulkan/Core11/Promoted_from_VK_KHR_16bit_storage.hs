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


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES = VkStructureType 1000083000
-- | TODO: Struct comments
data VkPhysicalDevice16BitStorageFeatures = VkPhysicalDevice16BitStorageFeatures
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkStorageBuffer16BitAccess :: VkBool32
  , vkUniformAndStorageBuffer16BitAccess :: VkBool32
  , vkStoragePushConstant16 :: VkBool32
  , vkStorageInputOutput16 :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 16) (vkStorageBuffer16BitAccess (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 20) (vkUniformAndStorageBuffer16BitAccess (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 24) (vkStoragePushConstant16 (poked :: VkPhysicalDevice16BitStorageFeatures))
                *> poke (ptr `plusPtr` 28) (vkStorageInputOutput16 (poked :: VkPhysicalDevice16BitStorageFeatures))
