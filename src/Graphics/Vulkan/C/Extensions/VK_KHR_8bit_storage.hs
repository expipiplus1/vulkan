{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( VkPhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME
  , pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
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


-- | VkPhysicalDevice8BitStorageFeaturesKHR - Structure describing features
-- supported by VK_KHR_8bit_storage
--
-- = Description
--
-- Unresolved directive in VkPhysicalDevice8BitStorageFeaturesKHR.txt -
-- include::..\/validity\/structs\/VkPhysicalDevice8BitStorageFeaturesKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkPhysicalDevice8BitStorageFeaturesKHR = VkPhysicalDevice8BitStorageFeaturesKHR
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @storageBuffer8BitAccess@ indicates whether objects in the
  -- @StorageBuffer@ or @PhysicalStorageBufferEXT@ storage class with the
  -- @Block@ decoration /can/ have 8-bit integer members. If this feature is
  -- not enabled, 8-bit integer members /must/ not be used in such objects.
  -- This also indicates whether shader modules /can/ declare the
  -- @StorageBuffer8BitAccess@ capability.
  vkStorageBuffer8BitAccess :: VkBool32
  , -- | @uniformAndStorageBuffer8BitAccess@ indicates whether objects in the
  -- @Uniform@ storage class with the @Block@ decoration and in the
  -- @StorageBuffer@ or @PhysicalStorageBufferEXT@ storage class with the
  -- same decoration /can/ have 8-bit integer members. If this feature is not
  -- enabled, 8-bit integer members /must/ not be used in such objects. This
  -- also indicates whether shader modules /can/ declare the
  -- @UniformAndStorageBuffer8BitAccess@ capability.
  vkUniformAndStorageBuffer8BitAccess :: VkBool32
  , -- | @storagePushConstant8@ indicates whether objects in the @PushConstant@
  -- storage class /can/ have 8-bit integer members. If this feature is not
  -- enabled, 8-bit integer members /must/ not be used in such objects. This
  -- also indicates whether shader modules /can/ declare the
  -- @StoragePushConstant8@ capability.
  vkStoragePushConstant8 :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDevice8BitStorageFeaturesKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkPhysicalDevice8BitStorageFeaturesKHR <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
                                                    <*> peek (ptr `plusPtr` 20)
                                                    <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDevice8BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDevice8BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 16) (vkStorageBuffer8BitAccess (poked :: VkPhysicalDevice8BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 20) (vkUniformAndStorageBuffer8BitAccess (poked :: VkPhysicalDevice8BitStorageFeaturesKHR))
                *> poke (ptr `plusPtr` 24) (vkStoragePushConstant8 (poked :: VkPhysicalDevice8BitStorageFeaturesKHR))

instance Zero VkPhysicalDevice8BitStorageFeaturesKHR where
  zero = VkPhysicalDevice8BitStorageFeaturesKHR zero
                                                zero
                                                zero
                                                zero
                                                zero
-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_EXTENSION_NAME"
pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_8BIT_STORAGE_EXTENSION_NAME = "VK_KHR_8bit_storage"
-- No documentation found for TopLevel "VK_KHR_8BIT_STORAGE_SPEC_VERSION"
pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_8BIT_STORAGE_SPEC_VERSION = 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR = VkStructureType 1000177000
