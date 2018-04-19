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


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO = VkStructureType 1000060013
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO = VkStructureType 1000060014
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = VkImageCreateFlagBits 0x00000040
-- | VkBindBufferMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
data VkBindBufferMemoryDeviceGroupInfo = VkBindBufferMemoryDeviceGroupInfo
  { -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "vkDeviceIndexCount"
  vkDeviceIndexCount :: Word32
  , -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "vkPDeviceIndices"
  vkPDeviceIndices :: Ptr Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindBufferMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceIndexCount (poked :: VkBindBufferMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 24) (vkPDeviceIndices (poked :: VkBindBufferMemoryDeviceGroupInfo))
-- | VkBindImageMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
data VkBindImageMemoryDeviceGroupInfo = VkBindImageMemoryDeviceGroupInfo
  { -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "vkDeviceIndexCount"
  vkDeviceIndexCount :: Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "vkPDeviceIndices"
  vkPDeviceIndices :: Ptr Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "vkSplitInstanceBindRegionCount"
  vkSplitInstanceBindRegionCount :: Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "vkPSplitInstanceBindRegions"
  vkPSplitInstanceBindRegions :: Ptr VkRect2D
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 16) (vkDeviceIndexCount (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 24) (vkPDeviceIndices (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 32) (vkSplitInstanceBindRegionCount (poked :: VkBindImageMemoryDeviceGroupInfo))
                *> poke (ptr `plusPtr` 40) (vkPSplitInstanceBindRegions (poked :: VkBindImageMemoryDeviceGroupInfo))
