{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  , VkPhysicalDevice16BitStorageFeatures(..)
  ) where

import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
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
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkPhysicalDevice16BitStorageFeatures = VkPhysicalDevice16BitStorageFeatures
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @storageBuffer16BitAccess@ specifies whether objects in the
  -- @StorageBuffer@ storage class with the @Block@ decoration /can/ have
  -- 16-bit integer and 16-bit floating-point members. If this feature is not
  -- enabled, 16-bit integer or 16-bit floating-point members /must/ not be
  -- used in such objects. This also specifies whether shader modules /can/
  -- declare the @StorageBuffer16BitAccess@ capability.
  vkStorageBuffer16BitAccess :: VkBool32
  , -- | @uniformAndStorageBuffer16BitAccess@ specifies whether objects in the
  -- @Uniform@ storage class with the @Block@ decoration and in the
  -- @StorageBuffer@ storage class with the same decoration /can/ have 16-bit
  -- integer and 16-bit floating-point members. If this feature is not
  -- enabled, 16-bit integer or 16-bit floating-point members /must/ not be
  -- used in such objects. This also specifies whether shader modules /can/
  -- declare the @UniformAndStorageBuffer16BitAccess@ capability.
  vkUniformAndStorageBuffer16BitAccess :: VkBool32
  , -- | @storagePushConstant16@ specifies whether objects in the @PushConstant@
  -- storage class /can/ have 16-bit integer and 16-bit floating-point
  -- members. If this feature is not enabled, 16-bit integer or
  -- floating-point members /must/ not be used in such objects. This also
  -- specifies whether shader modules /can/ declare the
  -- @StoragePushConstant16@ capability.
  vkStoragePushConstant16 :: VkBool32
  , -- | @storageInputOutput16@ specifies whether objects in the @Input@ and
  -- @Output@ storage classes /can/ have 16-bit integer and 16-bit
  -- floating-point members. If this feature is not enabled, 16-bit integer
  -- or 16-bit floating-point members /must/ not be used in such objects.
  -- This also specifies whether shader modules /can/ declare the
  -- @StorageInputOutput16@ capability.
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
