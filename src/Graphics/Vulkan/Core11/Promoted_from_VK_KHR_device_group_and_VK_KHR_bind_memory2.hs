{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkRect2D(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO = VkStructureType 1000060013
-- | Nothing
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO = VkStructureType 1000060014
-- | Just "Allows using VkBindImageMemoryDeviceGroupInfo::pSplitInstanceBindRegions when binding memory to the image"
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = VkImageCreateFlagBits 0x00000040
-- | TODO: Struct comments
data VkBindBufferMemoryDeviceGroupInfo = VkBindBufferMemoryDeviceGroupInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDeviceIndexCount :: Word32
  , vkDeviceIndices :: Ptr Word32
  }
  deriving (Eq, Show)

instance Storable VkBindBufferMemoryDeviceGroupInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkBindBufferMemoryDeviceGroupInfo <$> peek (ptr `plusPtr` 0)
                                               <*> peek (ptr `plusPtr` 8)
                                               <*> peek (ptr `plusPtr` 16)
                                               <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindBufferMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBindBufferMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceIndexCount (poked :: VkBindBufferMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 24) (vkDeviceIndices (poked :: VkBindBufferMemoryDeviceGroupInfo))
-- | TODO: Struct comments
data VkBindImageMemoryDeviceGroupInfo = VkBindImageMemoryDeviceGroupInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkDeviceIndexCount :: Word32
  , vkDeviceIndices :: Ptr Word32
  , vkSplitInstanceBindRegionCount :: Word32
  , vkSplitInstanceBindRegions :: Ptr VkRect2D
  }
  deriving (Eq, Show)

instance Storable VkBindImageMemoryDeviceGroupInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkBindImageMemoryDeviceGroupInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
                                              <*> peek (ptr `plusPtr` 24)
                                              <*> peek (ptr `plusPtr` 32)
                                              <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceIndexCount (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 24) (vkDeviceIndices (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 32) (vkSplitInstanceBindRegionCount (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 40) (vkSplitInstanceBindRegions (poked :: VkBindImageMemoryDeviceGroupInfo))
