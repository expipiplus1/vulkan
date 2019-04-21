{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( withCStructBindBufferMemoryDeviceGroupInfo
  , fromCStructBindBufferMemoryDeviceGroupInfo
  , BindBufferMemoryDeviceGroupInfo(..)
  , withCStructBindImageMemoryDeviceGroupInfo
  , fromCStructBindImageMemoryDeviceGroupInfo
  , BindImageMemoryDeviceGroupInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  ) where

import Control.Monad
  ( (<=<)
  )
import Data.Function
  ( (&)
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , fromCStructRect2D
  , withCStructRect2D
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  )



-- | VkBindBufferMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
--
-- = Members
--
-- If the @pNext@ list of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo'
-- includes a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo'
-- structure, then that structure determines how memory is bound to buffers
-- across multiple devices in a device group.
--
-- = Description
--
-- The
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo'
-- structure is defined as:
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
-- the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains consecutive indices
-- from zero to the number of physical devices in the logical device, minus
-- one. In other words, by default each physical device attaches to its own
-- instance of @memory@.
--
-- If @deviceIndexCount@ is zero and @memory@ comes from a memory heap
-- without the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains an array of zeros.
-- In other words, by default each physical device attaches to instance
-- zero.
--
-- == Valid Usage
--
-- -   @deviceIndexCount@ /must/ either be zero or equal to the number of
--     physical devices in the logical device
--
-- -   All elements of @pDeviceIndices@ /must/ be valid device indices
--
-- Unresolved directive in VkBindBufferMemoryDeviceGroupInfo.txt -
-- include::{generated}\/validity\/structs\/VkBindBufferMemoryDeviceGroupInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data BindBufferMemoryDeviceGroupInfo = BindBufferMemoryDeviceGroupInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindBufferMemoryDeviceGroupInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "BindBufferMemoryDeviceGroupInfo" "pDeviceIndices"
  deviceIndices :: Vector Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindBufferMemoryDeviceGroupInfo' and
-- marshal a 'BindBufferMemoryDeviceGroupInfo' into it. The 'VkBindBufferMemoryDeviceGroupInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindBufferMemoryDeviceGroupInfo :: BindBufferMemoryDeviceGroupInfo -> (VkBindBufferMemoryDeviceGroupInfo -> IO a) -> IO a
withCStructBindBufferMemoryDeviceGroupInfo marshalled cont = withVec (&) (deviceIndices (marshalled :: BindBufferMemoryDeviceGroupInfo)) (\pPDeviceIndices -> maybeWith withSomeVkStruct (next (marshalled :: BindBufferMemoryDeviceGroupInfo)) (\pPNext -> cont (VkBindBufferMemoryDeviceGroupInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO pPNext (fromIntegral (Data.Vector.length (deviceIndices (marshalled :: BindBufferMemoryDeviceGroupInfo)))) pPDeviceIndices)))

-- | A function to read a 'VkBindBufferMemoryDeviceGroupInfo' and all additional
-- structures in the pointer chain into a 'BindBufferMemoryDeviceGroupInfo'.
fromCStructBindBufferMemoryDeviceGroupInfo :: VkBindBufferMemoryDeviceGroupInfo -> IO BindBufferMemoryDeviceGroupInfo
fromCStructBindBufferMemoryDeviceGroupInfo c = BindBufferMemoryDeviceGroupInfo <$> -- Univalued Member elided
                                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindBufferMemoryDeviceGroupInfo)))
                                                                               -- Length valued member elided
                                                                               <*> (Data.Vector.generateM (fromIntegral (vkDeviceIndexCount (c :: VkBindBufferMemoryDeviceGroupInfo))) (peekElemOff (vkPDeviceIndices (c :: VkBindBufferMemoryDeviceGroupInfo))))

instance Zero BindBufferMemoryDeviceGroupInfo where
  zero = BindBufferMemoryDeviceGroupInfo Nothing
                                         Data.Vector.empty



-- | VkBindImageMemoryDeviceGroupInfo - Structure specifying device within a
-- group to bind to
--
-- = Members
--
-- If the @pNext@ list of
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo'
-- includes a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'
-- structure, then that structure determines how memory is bound to images
-- across multiple devices in a device group.
--
-- = Description
--
-- The
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'
-- structure is defined as:
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
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains consecutive indices
-- from zero to the number of physical devices in the logical device, minus
-- one. In other words, by default each physical device attaches to its own
-- instance of the memory.
--
-- If @splitInstanceBindRegionCount@ and @deviceIndexCount@ are zero and
-- the memory comes from a memory heap without the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VK_MEMORY_HEAP_MULTI_INSTANCE_BIT'
-- bit set, then it is as if @pDeviceIndices@ contains an array of zeros.
-- In other words, by default each physical device attaches to instance
-- zero.
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
--     ('Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'::@imageGranularity.width@)
--     of all non-metadata aspects of the image
--
-- -   The @offset.y@ member of any element of @pSplitInstanceBindRegions@
--     /must/ be a multiple of the sparse image block height
--     ('Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'::@imageGranularity.height@)
--     of all non-metadata aspects of the image
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
-- Unresolved directive in VkBindImageMemoryDeviceGroupInfo.txt -
-- include::{generated}\/validity\/structs\/VkBindImageMemoryDeviceGroupInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data BindImageMemoryDeviceGroupInfo = BindImageMemoryDeviceGroupInfo
  { -- Univalued member elided
  -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pDeviceIndices"
  deviceIndices :: Vector Word32
  -- Length valued member elided
  , -- No documentation found for Nested "BindImageMemoryDeviceGroupInfo" "pSplitInstanceBindRegions"
  splitInstanceBindRegions :: Vector Rect2D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBindImageMemoryDeviceGroupInfo' and
-- marshal a 'BindImageMemoryDeviceGroupInfo' into it. The 'VkBindImageMemoryDeviceGroupInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBindImageMemoryDeviceGroupInfo :: BindImageMemoryDeviceGroupInfo -> (VkBindImageMemoryDeviceGroupInfo -> IO a) -> IO a
withCStructBindImageMemoryDeviceGroupInfo marshalled cont = withVec withCStructRect2D (splitInstanceBindRegions (marshalled :: BindImageMemoryDeviceGroupInfo)) (\pPSplitInstanceBindRegions -> withVec (&) (deviceIndices (marshalled :: BindImageMemoryDeviceGroupInfo)) (\pPDeviceIndices -> maybeWith withSomeVkStruct (next (marshalled :: BindImageMemoryDeviceGroupInfo)) (\pPNext -> cont (VkBindImageMemoryDeviceGroupInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO pPNext (fromIntegral (Data.Vector.length (deviceIndices (marshalled :: BindImageMemoryDeviceGroupInfo)))) pPDeviceIndices (fromIntegral (Data.Vector.length (splitInstanceBindRegions (marshalled :: BindImageMemoryDeviceGroupInfo)))) pPSplitInstanceBindRegions))))

-- | A function to read a 'VkBindImageMemoryDeviceGroupInfo' and all additional
-- structures in the pointer chain into a 'BindImageMemoryDeviceGroupInfo'.
fromCStructBindImageMemoryDeviceGroupInfo :: VkBindImageMemoryDeviceGroupInfo -> IO BindImageMemoryDeviceGroupInfo
fromCStructBindImageMemoryDeviceGroupInfo c = BindImageMemoryDeviceGroupInfo <$> -- Univalued Member elided
                                                                             maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBindImageMemoryDeviceGroupInfo)))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkDeviceIndexCount (c :: VkBindImageMemoryDeviceGroupInfo))) (peekElemOff (vkPDeviceIndices (c :: VkBindImageMemoryDeviceGroupInfo))))
                                                                             -- Length valued member elided
                                                                             <*> (Data.Vector.generateM (fromIntegral (vkSplitInstanceBindRegionCount (c :: VkBindImageMemoryDeviceGroupInfo))) (((fromCStructRect2D <=<) . peekElemOff) (vkPSplitInstanceBindRegions (c :: VkBindImageMemoryDeviceGroupInfo))))

instance Zero BindImageMemoryDeviceGroupInfo where
  zero = BindImageMemoryDeviceGroupInfo Nothing
                                        Data.Vector.empty
                                        Data.Vector.empty

