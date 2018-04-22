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
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
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
--
-- = Members
--
-- If the @pNext@ list of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo'
-- includes a @VkBindBufferMemoryDeviceGroupInfo@ structure, then that
-- structure determines how memory is bound to buffers across multiple
-- devices in a device group.
--
-- = Description
--
-- The @VkBindBufferMemoryDeviceGroupInfo@ structure is defined as:
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to an extension-specific structure.
--
-- -   @deviceIndexCount@ is the number of elements in @pDeviceIndices@.
--
-- -   @pDeviceIndices@ is a pointer to an array of device indices.
--
-- If @deviceIndexCount@ is greater than zero, then on device index i the
-- buffer is attached to the instance of @memory@ on the physical device
-- with device index pDeviceIndices[i].
--
-- If @deviceIndexCount@ is zero and @memory@ comes from a memory heap with
-- the @VK_MEMORY_HEAP_MULTI_INSTANCE_BIT@ bit set, then it is as if
-- @pDeviceIndices@ contains consecutive indices from zero to the number of
-- physical devices in the logical device, minus one. In other words, by
-- default each physical device attaches to its own instance of @memory@.
--
-- If @deviceIndexCount@ is zero and @memory@ comes from a memory heap
-- without the @VK_MEMORY_HEAP_MULTI_INSTANCE_BIT@ bit set, then it is as
-- if @pDeviceIndices@ contains an array of zeros. In other words, by
-- default each physical device attaches to instance zero.
--
-- == Valid Usage
--
-- -   @deviceIndexCount@ /must/ either be zero or equal to the number of
--     physical devices in the logical device
--
-- -   All elements of @pDeviceIndices@ /must/ be valid device indices
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO@
--
-- -   If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
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
-- | VkBindImageMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
--
-- = Members
--
-- If the @pNext@ list of
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo'
-- includes a @VkBindImageMemoryDeviceGroupInfo@ structure, then that
-- structure determines how memory is bound to images across multiple
-- devices in a device group.
--
-- = Description
--
-- The @VkBindImageMemoryDeviceGroupInfo@ structure is defined as:
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to an extension-specific structure.
--
-- -   @deviceIndexCount@ is the number of elements in @pDeviceIndices@.
--
-- -   @pDeviceIndices@ is a pointer to an array of device indices.
--
-- -   @splitInstanceBindRegionCount@ is the number of elements in
--     @pSplitInstanceBindRegions@.
--
-- -   @pSplitInstanceBindRegions@ is a pointer to an array of rectangles
--     describing which regions of the image are attached to each instance
--     of memory.
--
-- If @deviceIndexCount@ is greater than zero, then on device index i
-- @image@ is attached to the instance of the memory on the physical device
-- with device index pDeviceIndices[i].
--
-- Let N be the number of physical devices in the logical device. If
-- @splitInstanceBindRegionCount@ is greater than zero, then
-- @pSplitInstanceBindRegions@ is an array of N2 rectangles, where the
-- image region specified by the rectangle at element i*N+j in resource
-- instance i is bound to the memory instance j. The blocks of the memory
-- that are bound to each sparse image block region use an offset in
-- memory, relative to @memoryOffset@, computed as if the whole image were
-- being bound to a contiguous range of memory. In other words,
-- horizontally adjacent image blocks use consecutive blocks of memory,
-- vertically adjacent image blocks are separated by the number of bytes
-- per block multiplied by the width in blocks of @image@, and the block at
-- (0,0) corresponds to memory starting at @memoryOffset@.
--
-- If @splitInstanceBindRegionCount@ and @deviceIndexCount@ are zero and
-- the memory comes from a memory heap with the
-- @VK_MEMORY_HEAP_MULTI_INSTANCE_BIT@ bit set, then it is as if
-- @pDeviceIndices@ contains consecutive indices from zero to the number of
-- physical devices in the logical device, minus one. In other words, by
-- default each physical device attaches to its own instance of the memory.
--
-- If @splitInstanceBindRegionCount@ and @deviceIndexCount@ are zero and
-- the memory comes from a memory heap without the
-- @VK_MEMORY_HEAP_MULTI_INSTANCE_BIT@ bit set, then it is as if
-- @pDeviceIndices@ contains an array of zeros. In other words, by default
-- each physical device attaches to instance zero.
--
-- == Valid Usage
--
-- -   At least one of @deviceIndexCount@ and
--     @splitInstanceBindRegionCount@ /must/ be zero.
--
-- -   @deviceIndexCount@ /must/ either be zero or equal to the number of
--     physical devices in the logical device
--
-- -   All elements of @pDeviceIndices@ /must/ be valid device indices.
--
-- -   @splitInstanceBindRegionCount@ /must/ either be zero or equal to the
--     number of physical devices in the logical device squared
--
-- -   Elements of @pSplitInstanceBindRegions@ that correspond to the same
--     instance of an image /must/ not overlap.
--
-- -   The @offset.x@ member of any element of @pSplitInstanceBindRegions@
--     /must/ be a multiple of the sparse image block width
--     (@VkSparseImageFormatProperties@::@imageGranularity.width@) of all
--     non-metadata aspects of the image
--
-- -   The @offset.y@ member of any element of @pSplitInstanceBindRegions@
--     /must/ be a multiple of the sparse image block height
--     (@VkSparseImageFormatProperties@::@imageGranularity.height@) of all
--     non-metadata aspects of the image
--
-- -   The @extent.width@ member of any element of
--     @pSplitInstanceBindRegions@ /must/ either be a multiple of the
--     sparse image block width of all non-metadata aspects of the image,
--     or else @extent.width@ + @offset.x@ /must/ equal the width of the
--     image subresource
--
-- -   The @extent.height@ member of any element of
--     @pSplitInstanceBindRegions@ /must/ either be a multiple of the
--     sparse image block height of all non-metadata aspects of the image,
--     or else @extent.height@
--     @offset.y@ /must/ equal the width of the image subresource
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO@
--
-- -   If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   If @splitInstanceBindRegionCount@ is not @0@,
--     @pSplitInstanceBindRegions@ /must/ be a valid pointer to an array of
--     @splitInstanceBindRegionCount@ @VkRect2D@ structures
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
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
