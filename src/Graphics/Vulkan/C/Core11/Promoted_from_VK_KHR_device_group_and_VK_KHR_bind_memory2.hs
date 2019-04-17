{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  ) where

import Data.Word
  ( Word32
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
  ( VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkRect2D(..)
  )


-- No documentation found for TopLevel "VkBindBufferMemoryDeviceGroupInfo"
data VkBindBufferMemoryDeviceGroupInfo = VkBindBufferMemoryDeviceGroupInfo
  { -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "deviceIndexCount"
  vkDeviceIndexCount :: Word32
  , -- No documentation found for Nested "VkBindBufferMemoryDeviceGroupInfo" "pDeviceIndices"
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

instance Zero VkBindBufferMemoryDeviceGroupInfo where
  zero = VkBindBufferMemoryDeviceGroupInfo zero
                                           zero
                                           zero
                                           zero
-- No documentation found for TopLevel "VkBindImageMemoryDeviceGroupInfo"
data VkBindImageMemoryDeviceGroupInfo = VkBindImageMemoryDeviceGroupInfo
  { -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "deviceIndexCount"
  vkDeviceIndexCount :: Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "pDeviceIndices"
  vkPDeviceIndices :: Ptr Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "splitInstanceBindRegionCount"
  vkSplitInstanceBindRegionCount :: Word32
  , -- No documentation found for Nested "VkBindImageMemoryDeviceGroupInfo" "pSplitInstanceBindRegions"
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

instance Zero VkBindImageMemoryDeviceGroupInfo where
  zero = VkBindImageMemoryDeviceGroupInfo zero
                                          zero
                                          zero
                                          zero
                                          zero
                                          zero
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = VkImageCreateFlagBits 0x00000040
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO = VkStructureType 1000060013
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO = VkStructureType 1000060014
