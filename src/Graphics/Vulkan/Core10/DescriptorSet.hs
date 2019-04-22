{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.DescriptorSet
  ( withCStructCopyDescriptorSet
  , fromCStructCopyDescriptorSet
  , CopyDescriptorSet(..)
  , withCStructDescriptorBufferInfo
  , fromCStructDescriptorBufferInfo
  , DescriptorBufferInfo(..)
  , withCStructDescriptorImageInfo
  , fromCStructDescriptorImageInfo
  , DescriptorImageInfo(..)
  , DescriptorPool
  , DescriptorPoolCreateFlagBits
  , pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , DescriptorPoolCreateFlags
  , withCStructDescriptorPoolCreateInfo
  , fromCStructDescriptorPoolCreateInfo
  , DescriptorPoolCreateInfo(..)
  , DescriptorPoolResetFlags
  , withCStructDescriptorPoolSize
  , fromCStructDescriptorPoolSize
  , DescriptorPoolSize(..)
  , DescriptorSet
  , withCStructDescriptorSetAllocateInfo
  , fromCStructDescriptorSetAllocateInfo
  , DescriptorSetAllocateInfo(..)
  , withCStructDescriptorSetLayoutBinding
  , fromCStructDescriptorSetLayoutBinding
  , DescriptorSetLayoutBinding(..)
  , DescriptorSetLayoutCreateFlagBits
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  , pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  , DescriptorSetLayoutCreateFlags
  , withCStructDescriptorSetLayoutCreateInfo
  , fromCStructDescriptorSetLayoutCreateInfo
  , DescriptorSetLayoutCreateInfo(..)
  , DescriptorType
  , pattern DESCRIPTOR_TYPE_SAMPLER
  , pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  , pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  , pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  , withCStructWriteDescriptorSet
  , fromCStructWriteDescriptorSet
  , WriteDescriptorSet(..)
  , allocateDescriptorSets
  , createDescriptorPool
  , createDescriptorSetLayout
  , destroyDescriptorPool
  , destroyDescriptorSetLayout
  , freeDescriptorSets
  , resetDescriptorPool
  , updateDescriptorSets
  , withDescriptorPool
  , withDescriptorSetLayout
  , withDescriptorSets
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Function
  ( (&)
  )
import Data.List
  ( minimum
  )
import Data.Maybe
  ( maybe
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
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorBufferInfo(..)
  , VkDescriptorImageInfo(..)
  , VkDescriptorPoolCreateFlagBits(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorPoolSize(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorType(..)
  , VkWriteDescriptorSet(..)
  , VkDescriptorPool
  , VkDescriptorSet
  , vkAllocateDescriptorSets
  , vkCreateDescriptorPool
  , vkCreateDescriptorSetLayout
  , vkDestroyDescriptorPool
  , vkDestroyDescriptorSetLayout
  , vkFreeDescriptorSets
  , vkResetDescriptorPool
  , vkUpdateDescriptorSets
  , pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( pattern VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT
  , pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( pattern VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( pattern VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
  )
import Graphics.Vulkan.Core10.BufferView
  ( BufferView
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageView
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( DescriptorSetLayout
  , ShaderStageFlags
  )
import Graphics.Vulkan.Core10.Sampler
  ( Sampler
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkCopyDescriptorSet - Structure specifying a copy descriptor set
-- operation
--
-- == Valid Usage
--
-- -   @srcBinding@ /must/ be a valid binding within @srcSet@
--
-- -   The sum of @srcArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @srcBinding@, and all applicable consecutive
--     bindings, as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   @dstBinding@ /must/ be a valid binding within @dstSet@
--
-- -   The sum of @dstArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @dstBinding@, and all applicable consecutive
--     bindings, as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   The type of @dstBinding@ within @dstSet@ /must/ be equal to the type
--     of @srcBinding@ within @srcSet@
--
-- -   If @srcSet@ is equal to @dstSet@, then the source and destination
--     ranges of descriptors /must/ not overlap, where the ranges /may/
--     include array elements from consecutive bindings as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @srcSet@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handle
--
-- -   @dstSet@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handle
--
-- -   Both of @dstSet@, and @srcSet@ /must/ have been created, allocated,
--     or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets'
data CopyDescriptorSet = CopyDescriptorSet
  { -- Univalued member elided
  -- No documentation found for Nested "CopyDescriptorSet" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "CopyDescriptorSet" "srcSet"
  srcSet :: DescriptorSet
  , -- No documentation found for Nested "CopyDescriptorSet" "srcBinding"
  srcBinding :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "srcArrayElement"
  srcArrayElement :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "dstSet"
  dstSet :: DescriptorSet
  , -- No documentation found for Nested "CopyDescriptorSet" "dstBinding"
  dstBinding :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "dstArrayElement"
  dstArrayElement :: Word32
  , -- No documentation found for Nested "CopyDescriptorSet" "descriptorCount"
  descriptorCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkCopyDescriptorSet' and
-- marshal a 'CopyDescriptorSet' into it. The 'VkCopyDescriptorSet' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructCopyDescriptorSet :: CopyDescriptorSet -> (VkCopyDescriptorSet -> IO a) -> IO a
withCStructCopyDescriptorSet marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: CopyDescriptorSet)) (\pPNext -> cont (VkCopyDescriptorSet VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET pPNext (srcSet (marshalled :: CopyDescriptorSet)) (srcBinding (marshalled :: CopyDescriptorSet)) (srcArrayElement (marshalled :: CopyDescriptorSet)) (dstSet (marshalled :: CopyDescriptorSet)) (dstBinding (marshalled :: CopyDescriptorSet)) (dstArrayElement (marshalled :: CopyDescriptorSet)) (descriptorCount (marshalled :: CopyDescriptorSet))))

-- | A function to read a 'VkCopyDescriptorSet' and all additional
-- structures in the pointer chain into a 'CopyDescriptorSet'.
fromCStructCopyDescriptorSet :: VkCopyDescriptorSet -> IO CopyDescriptorSet
fromCStructCopyDescriptorSet c = CopyDescriptorSet <$> -- Univalued Member elided
                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkCopyDescriptorSet)))
                                                   <*> pure (vkSrcSet (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkSrcBinding (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkSrcArrayElement (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDstSet (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDstBinding (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDstArrayElement (c :: VkCopyDescriptorSet))
                                                   <*> pure (vkDescriptorCount (c :: VkCopyDescriptorSet))

instance Zero CopyDescriptorSet where
  zero = CopyDescriptorSet Nothing
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero



-- | VkDescriptorBufferInfo - Structure specifying descriptor buffer info
--
-- = Description
--
-- __Note__
--
-- When setting @range@ to
-- 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', the effective range
-- /must/ not be larger than the maximum range for the descriptor type
-- (<https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxUniformBufferRange maxUniformBufferRange>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxStorageBufferRange maxStorageBufferRange>).
-- This means that 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE' is
-- not typically useful in the common case where uniform buffer descriptors
-- are suballocated from a buffer that is much larger than
-- @maxUniformBufferRange@.
--
-- For
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
-- and
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
-- descriptor types, @offset@ is the base offset from which the dynamic
-- offset is applied and @range@ is the static size used for all dynamic
-- offsets.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @range@ /must/
--     be greater than @0@
--
-- -   If @range@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @range@ /must/
--     be less than or equal to the size of @buffer@ minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
data DescriptorBufferInfo = DescriptorBufferInfo
  { -- No documentation found for Nested "DescriptorBufferInfo" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "DescriptorBufferInfo" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "DescriptorBufferInfo" "range"
  range :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorBufferInfo' and
-- marshal a 'DescriptorBufferInfo' into it. The 'VkDescriptorBufferInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorBufferInfo :: DescriptorBufferInfo -> (VkDescriptorBufferInfo -> IO a) -> IO a
withCStructDescriptorBufferInfo marshalled cont = cont (VkDescriptorBufferInfo (buffer (marshalled :: DescriptorBufferInfo)) (offset (marshalled :: DescriptorBufferInfo)) (range (marshalled :: DescriptorBufferInfo)))

-- | A function to read a 'VkDescriptorBufferInfo' and all additional
-- structures in the pointer chain into a 'DescriptorBufferInfo'.
fromCStructDescriptorBufferInfo :: VkDescriptorBufferInfo -> IO DescriptorBufferInfo
fromCStructDescriptorBufferInfo c = DescriptorBufferInfo <$> pure (vkBuffer (c :: VkDescriptorBufferInfo))
                                                         <*> pure (vkOffset (c :: VkDescriptorBufferInfo))
                                                         <*> pure (vkRange (c :: VkDescriptorBufferInfo))

instance Zero DescriptorBufferInfo where
  zero = DescriptorBufferInfo zero
                              zero
                              zero



-- | VkDescriptorImageInfo - Structure specifying descriptor image info
--
-- = Description
--
-- Members of
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo' that are
-- not used in an update (as described above) are ignored.
--
-- == Valid Usage
--
-- -   If @imageView@ is created from a depth\/stencil image, the
--     @aspectMask@ used to create the @imageView@ /must/ include either
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
--     but not both.
--
-- -   @imageLayout@ /must/ match the actual
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' of each subresource
--     accessible from @imageView@ at the time this descriptor is accessed
--     as defined by the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-layouts-matching-rule image layout matching rules>
--
-- == Valid Usage (Implicit)
--
-- -   Both of @imageView@, and @sampler@ that are valid handles /must/
--     have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
data DescriptorImageInfo = DescriptorImageInfo
  { -- No documentation found for Nested "DescriptorImageInfo" "sampler"
  sampler :: Sampler
  , -- No documentation found for Nested "DescriptorImageInfo" "imageView"
  imageView :: ImageView
  , -- No documentation found for Nested "DescriptorImageInfo" "imageLayout"
  imageLayout :: ImageLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorImageInfo' and
-- marshal a 'DescriptorImageInfo' into it. The 'VkDescriptorImageInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorImageInfo :: DescriptorImageInfo -> (VkDescriptorImageInfo -> IO a) -> IO a
withCStructDescriptorImageInfo marshalled cont = cont (VkDescriptorImageInfo (sampler (marshalled :: DescriptorImageInfo)) (imageView (marshalled :: DescriptorImageInfo)) (imageLayout (marshalled :: DescriptorImageInfo)))

-- | A function to read a 'VkDescriptorImageInfo' and all additional
-- structures in the pointer chain into a 'DescriptorImageInfo'.
fromCStructDescriptorImageInfo :: VkDescriptorImageInfo -> IO DescriptorImageInfo
fromCStructDescriptorImageInfo c = DescriptorImageInfo <$> pure (vkSampler (c :: VkDescriptorImageInfo))
                                                       <*> pure (vkImageView (c :: VkDescriptorImageInfo))
                                                       <*> pure (vkImageLayout (c :: VkDescriptorImageInfo))

instance Zero DescriptorImageInfo where
  zero = DescriptorImageInfo zero
                             zero
                             zero


-- | VkDescriptorPool - Opaque handle to a descriptor pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool'
type DescriptorPool = VkDescriptorPool

-- | VkDescriptorPoolCreateFlagBits - Bitmask specifying certain supported
-- operations on a descriptor pool
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlags'
type DescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits


{-# complete DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT, DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: DescriptorPoolCreateFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT'
-- specifies that descriptor sets /can/ return their individual allocations
-- to the pool, i.e. all of
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets', and
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool' are
-- allowed. Otherwise, descriptor sets allocated from the pool /must/ not
-- be individually freed back to the pool, i.e. only
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets' and
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool' are
-- allowed.
pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT :: (a ~ DescriptorPoolCreateFlagBits) => a
pattern DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT


-- No documentation found for Nested "DescriptorPoolCreateFlagBits" "DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT"
pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT :: (a ~ DescriptorPoolCreateFlagBits) => a
pattern DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT = VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT

-- | VkDescriptorPoolCreateFlags - Bitmask of VkDescriptorPoolCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlags' is
-- a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo'
type DescriptorPoolCreateFlags = DescriptorPoolCreateFlagBits


-- | VkDescriptorPoolCreateInfo - Structure specifying parameters of a newly
-- created descriptor pool
--
-- = Description
--
-- If multiple
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolSize' structures
-- appear in the @pPoolSizes@ array then the pool will be created with
-- enough storage for the total number of descriptors of each type.
--
-- Fragmentation of a descriptor pool is possible and /may/ lead to
-- descriptor set allocation failures. A failure due to fragmentation is
-- defined as failing a descriptor set allocation despite the sum of all
-- outstanding descriptor set allocations from the pool plus the requested
-- allocation requiring no more than the total number of descriptors
-- requested at pool creation. Implementations provide certain guarantees
-- of when fragmentation /must/ not cause allocation failure, as described
-- below.
--
-- If a descriptor pool has not had any descriptor sets freed since it was
-- created or most recently reset then fragmentation /must/ not cause an
-- allocation failure (note that this is always the case for a pool created
-- without the
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT'
-- bit set). Additionally, if all sets allocated from the pool since it was
-- created or most recently reset use the same number of descriptors (of
-- each type) and the requested allocation also uses that same number of
-- descriptors (of each type), then fragmentation /must/ not cause an
-- allocation failure.
--
-- If an allocation failure occurs due to fragmentation, an application
-- /can/ create an additional descriptor pool to perform further descriptor
-- set allocations.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorPool'
data DescriptorPoolCreateInfo = DescriptorPoolCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorPoolCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "flags"
  flags :: DescriptorPoolCreateFlags
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "maxSets"
  maxSets :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorPoolCreateInfo" "pPoolSizes"
  poolSizes :: Vector DescriptorPoolSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorPoolCreateInfo' and
-- marshal a 'DescriptorPoolCreateInfo' into it. The 'VkDescriptorPoolCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorPoolCreateInfo :: DescriptorPoolCreateInfo -> (VkDescriptorPoolCreateInfo -> IO a) -> IO a
withCStructDescriptorPoolCreateInfo marshalled cont = withVec withCStructDescriptorPoolSize (poolSizes (marshalled :: DescriptorPoolCreateInfo)) (\pPPoolSizes -> maybeWith withSomeVkStruct (next (marshalled :: DescriptorPoolCreateInfo)) (\pPNext -> cont (VkDescriptorPoolCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO pPNext (flags (marshalled :: DescriptorPoolCreateInfo)) (maxSets (marshalled :: DescriptorPoolCreateInfo)) (fromIntegral (Data.Vector.length (poolSizes (marshalled :: DescriptorPoolCreateInfo)))) pPPoolSizes)))

-- | A function to read a 'VkDescriptorPoolCreateInfo' and all additional
-- structures in the pointer chain into a 'DescriptorPoolCreateInfo'.
fromCStructDescriptorPoolCreateInfo :: VkDescriptorPoolCreateInfo -> IO DescriptorPoolCreateInfo
fromCStructDescriptorPoolCreateInfo c = DescriptorPoolCreateInfo <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorPoolCreateInfo)))
                                                                 <*> pure (vkFlags (c :: VkDescriptorPoolCreateInfo))
                                                                 <*> pure (vkMaxSets (c :: VkDescriptorPoolCreateInfo))
                                                                 -- Length valued member elided
                                                                 <*> (Data.Vector.generateM (fromIntegral (vkPoolSizeCount (c :: VkDescriptorPoolCreateInfo))) (((fromCStructDescriptorPoolSize <=<) . peekElemOff) (vkPPoolSizes (c :: VkDescriptorPoolCreateInfo))))

instance Zero DescriptorPoolCreateInfo where
  zero = DescriptorPoolCreateInfo Nothing
                                  zero
                                  zero
                                  Data.Vector.empty


-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolResetFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool'
type DescriptorPoolResetFlags = VkDescriptorPoolResetFlags


-- No complete pragma for DescriptorPoolResetFlags as it has no patterns


-- | VkDescriptorPoolSize - Structure specifying descriptor pool size
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType'
data DescriptorPoolSize = DescriptorPoolSize
  { -- No documentation found for Nested "DescriptorPoolSize" "type"
  type' :: DescriptorType
  , -- No documentation found for Nested "DescriptorPoolSize" "descriptorCount"
  descriptorCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorPoolSize' and
-- marshal a 'DescriptorPoolSize' into it. The 'VkDescriptorPoolSize' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorPoolSize :: DescriptorPoolSize -> (VkDescriptorPoolSize -> IO a) -> IO a
withCStructDescriptorPoolSize marshalled cont = cont (VkDescriptorPoolSize (type' (marshalled :: DescriptorPoolSize)) (descriptorCount (marshalled :: DescriptorPoolSize)))

-- | A function to read a 'VkDescriptorPoolSize' and all additional
-- structures in the pointer chain into a 'DescriptorPoolSize'.
fromCStructDescriptorPoolSize :: VkDescriptorPoolSize -> IO DescriptorPoolSize
fromCStructDescriptorPoolSize c = DescriptorPoolSize <$> pure (vkType (c :: VkDescriptorPoolSize))
                                                     <*> pure (vkDescriptorCount (c :: VkDescriptorPoolSize))

instance Zero DescriptorPoolSize where
  zero = DescriptorPoolSize zero
                            zero


-- | VkDescriptorSet - Opaque handle to a descriptor set object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR'
type DescriptorSet = VkDescriptorSet


-- | VkDescriptorSetAllocateInfo - Structure specifying the allocation
-- parameters for descriptor sets
--
-- == Valid Usage
--
-- -   @descriptorSetCount@ /must/ not be greater than the number of sets
--     that are currently available for allocation in @descriptorPool@
--
-- -   @descriptorPool@ /must/ have enough free descriptor capacity
--     remaining to allocate the descriptor sets of the specified layouts
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetVariableDescriptorCountAllocateInfoEXT'
--
-- -   @descriptorPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool' handle
--
-- -   @pSetLayouts@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout'
--     handles
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   Both of @descriptorPool@, and the elements of @pSetLayouts@ /must/
--     have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets'
data DescriptorSetAllocateInfo = DescriptorSetAllocateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorSetAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetAllocateInfo" "descriptorPool"
  descriptorPool :: DescriptorPool
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorSetAllocateInfo" "pSetLayouts"
  setLayouts :: Vector DescriptorSetLayout
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetAllocateInfo' and
-- marshal a 'DescriptorSetAllocateInfo' into it. The 'VkDescriptorSetAllocateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetAllocateInfo :: DescriptorSetAllocateInfo -> (VkDescriptorSetAllocateInfo -> IO a) -> IO a
withCStructDescriptorSetAllocateInfo marshalled cont = withVec (&) (setLayouts (marshalled :: DescriptorSetAllocateInfo)) (\pPSetLayouts -> maybeWith withSomeVkStruct (next (marshalled :: DescriptorSetAllocateInfo)) (\pPNext -> cont (VkDescriptorSetAllocateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO pPNext (descriptorPool (marshalled :: DescriptorSetAllocateInfo)) (fromIntegral (Data.Vector.length (setLayouts (marshalled :: DescriptorSetAllocateInfo)))) pPSetLayouts)))

-- | A function to read a 'VkDescriptorSetAllocateInfo' and all additional
-- structures in the pointer chain into a 'DescriptorSetAllocateInfo'.
fromCStructDescriptorSetAllocateInfo :: VkDescriptorSetAllocateInfo -> IO DescriptorSetAllocateInfo
fromCStructDescriptorSetAllocateInfo c = DescriptorSetAllocateInfo <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetAllocateInfo)))
                                                                   <*> pure (vkDescriptorPool (c :: VkDescriptorSetAllocateInfo))
                                                                   -- Length valued member elided
                                                                   <*> (Data.Vector.generateM (fromIntegral (vkDescriptorSetCount (c :: VkDescriptorSetAllocateInfo))) (peekElemOff (vkPSetLayouts (c :: VkDescriptorSetAllocateInfo))))

instance Zero DescriptorSetAllocateInfo where
  zero = DescriptorSetAllocateInfo Nothing
                                   zero
                                   Data.Vector.empty



-- | VkDescriptorSetLayoutBinding - Structure specifying a descriptor set
-- layout binding
--
-- = Description
--
-- -   @pImmutableSamplers@ affects initialization of samplers. If
--     @descriptorType@ specifies a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     type descriptor, then @pImmutableSamplers@ /can/ be used to
--     initialize a set of /immutable samplers/. Immutable samplers are
--     permanently bound into the set layout; later binding a sampler into
--     an immutable sampler slot in a descriptor set is not allowed. If
--     @pImmutableSamplers@ is not @NULL@, then it is considered to be a
--     pointer to an array of sampler handles that will be consumed by the
--     set layout and used for the corresponding binding. If
--     @pImmutableSamplers@ is @NULL@, then the sampler slots are dynamic
--     and sampler handles /must/ be bound into descriptor sets using this
--     layout. If @descriptorType@ is not one of these descriptor types,
--     then @pImmutableSamplers@ is ignored.
--
-- The above layout definition allows the descriptor bindings to be
-- specified sparsely such that not all binding numbers between 0 and the
-- maximum binding number need to be specified in the @pBindings@ array.
-- Bindings that are not specified have a @descriptorCount@ and
-- @stageFlags@ of zero, and the value of @descriptorType@ is undefined.
-- However, all binding numbers between 0 and the maximum binding number in
-- the
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'::@pBindings@
-- array /may/ consume memory in the descriptor set layout even if not all
-- descriptor bindings are used, though it /should/ not consume additional
-- memory from the descriptor pool.
--
-- __Note__
--
-- The maximum binding number specified /should/ be as compact as possible
-- to avoid wasted memory.
--
-- == Valid Usage
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and @descriptorCount@ is not @0@ and @pImmutableSamplers@ is not
--     @NULL@, @pImmutableSamplers@ /must/ be a valid pointer to an array
--     of @descriptorCount@ valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' handles
--
-- -   If @descriptorCount@ is not @0@, @stageFlags@ /must/ be a valid
--     combination of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits' values
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     and @descriptorCount@ is not @0@, then @stageFlags@ /must/ be @0@ or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_FRAGMENT_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @descriptorType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType' value
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags'
data DescriptorSetLayoutBinding = DescriptorSetLayoutBinding
  { -- No documentation found for Nested "DescriptorSetLayoutBinding" "binding"
  binding :: Word32
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "descriptorType"
  descriptorType :: DescriptorType
  -- Optional length valued member elided
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "stageFlags"
  stageFlags :: ShaderStageFlags
  , -- No documentation found for Nested "DescriptorSetLayoutBinding" "pImmutableSamplers"
  immutableSamplers :: Maybe (Vector Sampler)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetLayoutBinding' and
-- marshal a 'DescriptorSetLayoutBinding' into it. The 'VkDescriptorSetLayoutBinding' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetLayoutBinding :: DescriptorSetLayoutBinding -> (VkDescriptorSetLayoutBinding -> IO a) -> IO a
withCStructDescriptorSetLayoutBinding marshalled cont = maybeWith (withVec (&)) (immutableSamplers (marshalled :: DescriptorSetLayoutBinding)) (\pPImmutableSamplers -> cont (VkDescriptorSetLayoutBinding (binding (marshalled :: DescriptorSetLayoutBinding)) (descriptorType (marshalled :: DescriptorSetLayoutBinding)) (maybe 0 (fromIntegral . Data.Vector.length) (immutableSamplers (marshalled :: DescriptorSetLayoutBinding))) (stageFlags (marshalled :: DescriptorSetLayoutBinding)) pPImmutableSamplers))

-- | A function to read a 'VkDescriptorSetLayoutBinding' and all additional
-- structures in the pointer chain into a 'DescriptorSetLayoutBinding'.
fromCStructDescriptorSetLayoutBinding :: VkDescriptorSetLayoutBinding -> IO DescriptorSetLayoutBinding
fromCStructDescriptorSetLayoutBinding c = DescriptorSetLayoutBinding <$> pure (vkBinding (c :: VkDescriptorSetLayoutBinding))
                                                                     <*> pure (vkDescriptorType (c :: VkDescriptorSetLayoutBinding))
                                                                     -- Optional length valued member elided
                                                                     <*> pure (vkStageFlags (c :: VkDescriptorSetLayoutBinding))
                                                                     <*> maybePeek (\p -> Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkDescriptorSetLayoutBinding))) (peekElemOff p)) (vkPImmutableSamplers (c :: VkDescriptorSetLayoutBinding))

instance Zero DescriptorSetLayoutBinding where
  zero = DescriptorSetLayoutBinding zero
                                    zero
                                    zero
                                    Nothing


-- | VkDescriptorSetLayoutCreateFlagBits - Bitmask specifying descriptor set
-- layout properties
--
-- = Description
--
-- __Note__
--
-- All bits for this type are defined by extensions, and none of those
-- extensions are enabled in this build of the specification.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlags'
type DescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits


{-# complete DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR, DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: DescriptorSetLayoutCreateFlagBits #-}


-- No documentation found for Nested "DescriptorSetLayoutCreateFlagBits" "DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR :: (a ~ DescriptorSetLayoutCreateFlagBits) => a
pattern DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR = VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR


-- No documentation found for Nested "DescriptorSetLayoutCreateFlagBits" "DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT"
pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT :: (a ~ DescriptorSetLayoutCreateFlagBits) => a
pattern DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT = VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT

-- | VkDescriptorSetLayoutCreateFlags - Bitmask of
-- VkDescriptorSetLayoutCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlags'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'
type DescriptorSetLayoutCreateFlags = DescriptorSetLayoutCreateFlagBits


-- | VkDescriptorSetLayoutCreateInfo - Structure specifying parameters of a
-- newly created descriptor set layout
--
-- == Valid Usage
--
-- -   The
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'::@binding@
--     members of the elements of the @pBindings@ array /must/ each have
--     different values.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetLayoutBindingFlagsCreateInfoEXT'
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlagBits'
--     values
--
-- -   If @bindingCount@ is not @0@, @pBindings@ /must/ be a valid pointer
--     to an array of @bindingCount@ valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupportKHR'
data DescriptorSetLayoutCreateInfo = DescriptorSetLayoutCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "flags"
  flags :: DescriptorSetLayoutCreateFlags
  -- Length valued member elided
  , -- No documentation found for Nested "DescriptorSetLayoutCreateInfo" "pBindings"
  bindings :: Vector DescriptorSetLayoutBinding
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDescriptorSetLayoutCreateInfo' and
-- marshal a 'DescriptorSetLayoutCreateInfo' into it. The 'VkDescriptorSetLayoutCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDescriptorSetLayoutCreateInfo :: DescriptorSetLayoutCreateInfo -> (VkDescriptorSetLayoutCreateInfo -> IO a) -> IO a
withCStructDescriptorSetLayoutCreateInfo marshalled cont = withVec withCStructDescriptorSetLayoutBinding (bindings (marshalled :: DescriptorSetLayoutCreateInfo)) (\pPBindings -> maybeWith withSomeVkStruct (next (marshalled :: DescriptorSetLayoutCreateInfo)) (\pPNext -> cont (VkDescriptorSetLayoutCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO pPNext (flags (marshalled :: DescriptorSetLayoutCreateInfo)) (fromIntegral (Data.Vector.length (bindings (marshalled :: DescriptorSetLayoutCreateInfo)))) pPBindings)))

-- | A function to read a 'VkDescriptorSetLayoutCreateInfo' and all additional
-- structures in the pointer chain into a 'DescriptorSetLayoutCreateInfo'.
fromCStructDescriptorSetLayoutCreateInfo :: VkDescriptorSetLayoutCreateInfo -> IO DescriptorSetLayoutCreateInfo
fromCStructDescriptorSetLayoutCreateInfo c = DescriptorSetLayoutCreateInfo <$> -- Univalued Member elided
                                                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkDescriptorSetLayoutCreateInfo)))
                                                                           <*> pure (vkFlags (c :: VkDescriptorSetLayoutCreateInfo))
                                                                           -- Length valued member elided
                                                                           <*> (Data.Vector.generateM (fromIntegral (vkBindingCount (c :: VkDescriptorSetLayoutCreateInfo))) (((fromCStructDescriptorSetLayoutBinding <=<) . peekElemOff) (vkPBindings (c :: VkDescriptorSetLayoutCreateInfo))))

instance Zero DescriptorSetLayoutCreateInfo where
  zero = DescriptorSetLayoutCreateInfo Nothing
                                       zero
                                       Data.Vector.empty


-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampler sampler descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler combined image sampler descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic dynamic uniform buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic dynamic storage buffer descriptor>.
--
-- -   'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
--     specifies an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-inputattachment input attachment descriptor>.
--
-- When a descriptor set is updated via elements of
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet', members
-- of @pImageInfo@, @pBufferInfo@ and @pTexelBufferView@ are only accessed
-- by the implementation when they correspond to descriptor type being
-- defined - otherwise they are ignored. The members accessed are as
-- follows for each descriptor type:
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
--     only the @sampler@ member of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pImageInfo@
--     is accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     only the @imageView@ and @imageLayout@ members of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pImageInfo@
--     are accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     all members of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pImageInfo@
--     are accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     all members of each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pBufferInfo@
--     are accessed.
--
-- -   For
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     each element of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'::@pTexelBufferView@
--     is accessed.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolSize',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateEntry',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle.VkImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
type DescriptorType = VkDescriptorType


{-# complete DESCRIPTOR_TYPE_SAMPLER, DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, DESCRIPTOR_TYPE_SAMPLED_IMAGE, DESCRIPTOR_TYPE_STORAGE_IMAGE, DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER, DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER, DESCRIPTOR_TYPE_UNIFORM_BUFFER, DESCRIPTOR_TYPE_STORAGE_BUFFER, DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC, DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC, DESCRIPTOR_TYPE_INPUT_ATTACHMENT, DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT, DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :: DescriptorType #-}


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_SAMPLER"
pattern DESCRIPTOR_TYPE_SAMPLER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_SAMPLER = VK_DESCRIPTOR_TYPE_SAMPLER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE = VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE = VK_DESCRIPTOR_TYPE_STORAGE_IMAGE


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT = VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT


-- No documentation found for Nested "DescriptorType" "DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV :: (a ~ DescriptorType) => a
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV


-- | VkWriteDescriptorSet - Structure specifying the parameters of a
-- descriptor set write operation
--
-- = Description
--
-- Only one of @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ members
-- is used according to the descriptor type specified in the
-- @descriptorType@ member of the containing
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet' structure,
-- as specified below.
--
-- If the @dstBinding@ has fewer than @descriptorCount@ array elements
-- remaining starting from @dstArrayElement@, then the remainder will be
-- used to update the subsequent binding - @dstBinding@+1 starting at array
-- element zero. If a binding has a @descriptorCount@ of zero, it is
-- skipped. This behavior applies recursively, with the update affecting
-- consecutive bindings as needed to update all @descriptorCount@
-- descriptors.
--
-- == Valid Usage
--
-- -   @dstBinding@ /must/ be less than or equal to the maximum value of
--     @binding@ of all
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
--     structures specified when @dstSet@’s descriptor set layout was
--     created
--
-- -   @dstBinding@ /must/ be a binding with a non-zero @descriptorCount@
--
-- -   All consecutive bindings updated via a single
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
--     structure, except those with a @descriptorCount@ of zero, /must/
--     have identical @descriptorType@ and @stageFlags@.
--
-- -   All consecutive bindings updated via a single
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
--     structure, except those with a @descriptorCount@ of zero, /must/ all
--     either use immutable samplers or /must/ all not use immutable
--     samplers.
--
-- -   @descriptorType@ /must/ match the type of @dstBinding@ within
--     @dstSet@
--
-- -   @dstSet@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handle
--
-- -   The sum of @dstArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @dstBinding@, and all applicable consecutive
--     bindings, as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     @pImageInfo@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo'
--     structures
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     @pTexelBufferView@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid
--     'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' handles
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     @pBufferInfo@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo'
--     structures
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and @dstSet@ was not allocated with a layout that included immutable
--     samplers for @dstBinding@ with @descriptorType@, the @sampler@
--     member of each element of @pImageInfo@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @imageView@ and @imageLayout@ members of each element of
--     @pImageInfo@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' and
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout', respectively
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     for each descriptor that will be accessed via load or store
--     operations the @imageLayout@ member for corresponding elements of
--     @pImageInfo@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     the @offset@ member of each element of @pBufferInfo@ /must/ be a
--     multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     the @offset@ member of each element of @pBufferInfo@ /must/ be a
--     multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     and the @buffer@ member of any element of @pBufferInfo@ is the
--     handle of a non-sparse buffer, then that buffer /must/ be bound
--     completely and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     the @buffer@ member of each element of @pBufferInfo@ /must/ have
--     been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     the @buffer@ member of each element of @pBufferInfo@ /must/ have
--     been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     the @range@ member of each element of @pBufferInfo@, or the
--     effective range if @range@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', /must/ be less
--     than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxUniformBufferRange@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
--     the @range@ member of each element of @pBufferInfo@, or the
--     effective range if @range@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', /must/ be less
--     than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxStorageBufferRange@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     the 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' that each
--     element of @pTexelBufferView@ was created from /must/ have been
--     created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     the 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' that each
--     element of @pTexelBufferView@ was created from /must/ have been
--     created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with the identity swizzle
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
--     set
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     the @imageLayout@ member of each element of @pImageInfo@ /must/ be a
--     member of the list given in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage Sampled Image>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler Combined Image Sampler>,
--     corresponding to its type
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     set
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
--     the @imageView@ member of each element of @pImageInfo@ /must/ have
--     been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkWriteDescriptorSetAccelerationStructureNV'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkWriteDescriptorSetInlineUniformBlockEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @descriptorType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType' value
--
-- -   @descriptorCount@ /must/ be greater than @0@
--
-- -   Both of @dstSet@, and the elements of @pTexelBufferView@ that are
--     valid handles /must/ have been created, allocated, or retrieved from
--     the same 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorImageInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.vkCmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets'
data WriteDescriptorSet = WriteDescriptorSet
  { -- Univalued member elided
  -- No documentation found for Nested "WriteDescriptorSet" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WriteDescriptorSet" "dstSet"
  dstSet :: DescriptorSet
  , -- No documentation found for Nested "WriteDescriptorSet" "dstBinding"
  dstBinding :: Word32
  , -- No documentation found for Nested "WriteDescriptorSet" "dstArrayElement"
  dstArrayElement :: Word32
  -- Length valued member elided
  , -- No documentation found for Nested "WriteDescriptorSet" "descriptorType"
  descriptorType :: DescriptorType
  , -- No documentation found for Nested "WriteDescriptorSet" "pImageInfo"
  imageInfo :: Vector DescriptorImageInfo
  , -- No documentation found for Nested "WriteDescriptorSet" "pBufferInfo"
  bufferInfo :: Vector DescriptorBufferInfo
  , -- No documentation found for Nested "WriteDescriptorSet" "pTexelBufferView"
  texelBufferView :: Vector BufferView
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkWriteDescriptorSet' and
-- marshal a 'WriteDescriptorSet' into it. The 'VkWriteDescriptorSet' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructWriteDescriptorSet :: WriteDescriptorSet -> (VkWriteDescriptorSet -> IO a) -> IO a
withCStructWriteDescriptorSet marshalled cont = withVec (&) (texelBufferView (marshalled :: WriteDescriptorSet)) (\pPTexelBufferView -> withVec withCStructDescriptorBufferInfo (bufferInfo (marshalled :: WriteDescriptorSet)) (\pPBufferInfo -> withVec withCStructDescriptorImageInfo (imageInfo (marshalled :: WriteDescriptorSet)) (\pPImageInfo -> maybeWith withSomeVkStruct (next (marshalled :: WriteDescriptorSet)) (\pPNext -> cont (VkWriteDescriptorSet VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET pPNext (dstSet (marshalled :: WriteDescriptorSet)) (dstBinding (marshalled :: WriteDescriptorSet)) (dstArrayElement (marshalled :: WriteDescriptorSet)) (fromIntegral (minimum ([Data.Vector.length (imageInfo (marshalled :: WriteDescriptorSet)), Data.Vector.length (bufferInfo (marshalled :: WriteDescriptorSet)), Data.Vector.length (texelBufferView (marshalled :: WriteDescriptorSet))]))) (descriptorType (marshalled :: WriteDescriptorSet)) pPImageInfo pPBufferInfo pPTexelBufferView)))))

-- | A function to read a 'VkWriteDescriptorSet' and all additional
-- structures in the pointer chain into a 'WriteDescriptorSet'.
fromCStructWriteDescriptorSet :: VkWriteDescriptorSet -> IO WriteDescriptorSet
fromCStructWriteDescriptorSet c = WriteDescriptorSet <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWriteDescriptorSet)))
                                                     <*> pure (vkDstSet (c :: VkWriteDescriptorSet))
                                                     <*> pure (vkDstBinding (c :: VkWriteDescriptorSet))
                                                     <*> pure (vkDstArrayElement (c :: VkWriteDescriptorSet))
                                                     -- Length valued member elided
                                                     <*> pure (vkDescriptorType (c :: VkWriteDescriptorSet))
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkWriteDescriptorSet))) (((fromCStructDescriptorImageInfo <=<) . peekElemOff) (vkPImageInfo (c :: VkWriteDescriptorSet))))
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkWriteDescriptorSet))) (((fromCStructDescriptorBufferInfo <=<) . peekElemOff) (vkPBufferInfo (c :: VkWriteDescriptorSet))))
                                                     <*> (Data.Vector.generateM (fromIntegral (vkDescriptorCount (c :: VkWriteDescriptorSet))) (peekElemOff (vkPTexelBufferView (c :: VkWriteDescriptorSet))))

instance Zero WriteDescriptorSet where
  zero = WriteDescriptorSet Nothing
                            zero
                            zero
                            zero
                            zero
                            Data.Vector.empty
                            Data.Vector.empty
                            Data.Vector.empty



-- | vkAllocateDescriptorSets - Allocate one or more descriptor sets
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'
--     structure describing parameters of the allocation.
--
-- -   @pDescriptorSets@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handles in
--     which the resulting descriptor set objects are returned.
--
-- = Description
--
-- The allocated descriptor sets are returned in @pDescriptorSets@.
--
-- When a descriptor set is allocated, the initial state is largely
-- uninitialized and all descriptors are undefined. Descriptors also become
-- undefined if the underlying resource is destroyed. Descriptor sets
-- containing undefined descriptors /can/ still be bound and used, subject
-- to the following conditions:
--
-- -   Descriptors that are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-staticuse statically used>
--     /must/ have been populated before the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-binding consumed>.
--
-- -   Entries that are not used by a pipeline /can/ have undefined
--     descriptors.
--
-- If an allocation fails due to fragmentation, an indeterminate error is
-- returned with an unspecified error code. Any returned error other than
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FRAGMENTED_POOL' does not imply
-- its usual meaning: applications /should/ assume that the allocation
-- failed due to fragmentation, and create a new descriptor pool.
--
-- __Note__
--
-- Applications /should/ check for a negative return value when allocating
-- new descriptor sets, assume that any error effectively means
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FRAGMENTED_POOL', and try to
-- create a new descriptor pool. If
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FRAGMENTED_POOL' is the actual
-- return value, it adds certainty to that decision.
--
-- The reason for this is that
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FRAGMENTED_POOL' was only added
-- in a later version of the 1.0 specification, and so drivers /may/ return
-- other errors if they were written against earlier versions. To ensure
-- full compatibility with earlier patch versions, these other errors are
-- allowed.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pAllocateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo'
--     structure
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @pAllocateInfo@::descriptorSetCount
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handles
--
-- == Host Synchronization
--
-- -   Host access to @pAllocateInfo@::descriptorPool /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FRAGMENTED_POOL'
--
--     -   'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_ERROR_OUT_OF_POOL_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
allocateDescriptorSets :: Device ->  DescriptorSetAllocateInfo ->  IO (Vector DescriptorSet)
allocateDescriptorSets = \(Device device' commandTable) -> \allocateInfo' -> allocaArray (Data.Vector.length (setLayouts (allocateInfo' :: DescriptorSetAllocateInfo))) (\pDescriptorSets' -> (\marshalled -> withCStructDescriptorSetAllocateInfo marshalled . flip with) allocateInfo' (\pAllocateInfo' -> vkAllocateDescriptorSets commandTable device' pAllocateInfo' pDescriptorSets' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((Data.Vector.generateM (Data.Vector.length (setLayouts (allocateInfo' :: DescriptorSetAllocateInfo))) (peekElemOff pDescriptorSets'))))))


-- | vkCreateDescriptorPool - Creates a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo'
--     structure specifying the state of the descriptor pool object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDescriptorPool@ points to a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool' handle in
--     which the resulting descriptor pool object is returned.
--
-- = Description
--
-- @pAllocator@ controls host memory allocation as described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
-- chapter.
--
-- The created descriptor pool is returned in @pDescriptorPool@.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pDescriptorPool@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_ERROR_FRAGMENTATION_EXT'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createDescriptorPool :: Device ->  DescriptorPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorPool)
createDescriptorPool = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pDescriptorPool' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDescriptorPoolCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDescriptorPool commandTable device' pCreateInfo' pAllocator pDescriptorPool' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pDescriptorPool')))))


-- | vkCreateDescriptorSetLayout - Create a new descriptor set layout
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor set
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'
--     structure specifying the state of the descriptor set layout object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pSetLayout@ points to a
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout'
--     handle in which the resulting descriptor set layout object is
--     returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pSetLayout@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout'
--     handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
createDescriptorSetLayout :: Device ->  DescriptorSetLayoutCreateInfo ->  Maybe AllocationCallbacks ->  IO (DescriptorSetLayout)
createDescriptorSetLayout = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pSetLayout' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructDescriptorSetLayoutCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateDescriptorSetLayout commandTable device' pCreateInfo' pAllocator pSetLayout' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSetLayout')))))


-- | vkDestroyDescriptorPool - Destroy a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the descriptor pool.
--
-- -   @descriptorPool@ is the descriptor pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- = Description
--
-- When a pool is destroyed, all descriptor sets allocated from the pool
-- are implicitly freed and become invalid. Descriptor sets allocated from
-- a given pool do not need to be freed before destroying that descriptor
-- pool.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @descriptorPool@ (via any
--     allocated descriptor sets) /must/ have completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @descriptorPool@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @descriptorPool@ was created, @pAllocator@ /must/
--     be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @descriptorPool@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @descriptorPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @descriptorPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyDescriptorPool :: Device ->  DescriptorPool ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorPool = \(Device device' commandTable) -> \descriptorPool' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyDescriptorPool commandTable device' descriptorPool' pAllocator *> (pure ()))


-- | vkDestroyDescriptorSetLayout - Destroy a descriptor set layout object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the descriptor set
--     layout.
--
-- -   @descriptorSetLayout@ is the descriptor set layout to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @descriptorSetLayout@ was created, a compatible
--     set of callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @descriptorSetLayout@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @descriptorSetLayout@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     @descriptorSetLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout'
--     handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @descriptorSetLayout@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorSetLayout@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
destroyDescriptorSetLayout :: Device ->  DescriptorSetLayout ->  Maybe AllocationCallbacks ->  IO ()
destroyDescriptorSetLayout = \(Device device' commandTable) -> \descriptorSetLayout' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyDescriptorSetLayout commandTable device' descriptorSetLayout' pAllocator *> (pure ()))


-- | vkFreeDescriptorSets - Free one or more descriptor sets
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @descriptorPool@ is the descriptor pool from which the descriptor
--     sets were allocated.
--
-- -   @descriptorSetCount@ is the number of elements in the
--     @pDescriptorSets@ array.
--
-- -   @pDescriptorSets@ is an array of handles to
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' objects.
--
-- = Description
--
-- After a successful call to
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets', all
-- descriptor sets in @pDescriptorSets@ are invalid.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to any element of
--     @pDescriptorSets@ /must/ have completed execution
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handles,
--     each element of which /must/ either be a valid handle or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   Each valid handle in @pDescriptorSets@ /must/ have been allocated
--     from @descriptorPool@
--
-- -   @descriptorPool@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT'
--     flag
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @descriptorPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool' handle
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   @descriptorPool@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- -   Each element of @pDescriptorSets@ that is a valid handle /must/ have
--     been created, allocated, or retrieved from @descriptorPool@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- -   Host access to each member of @pDescriptorSets@ /must/ be externally
--     synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
freeDescriptorSets :: Device ->  DescriptorPool ->  Vector DescriptorSet ->  IO ()
freeDescriptorSets = \(Device device' commandTable) -> \descriptorPool' -> \descriptorSets' -> withVec (&) descriptorSets' (\pDescriptorSets' -> vkFreeDescriptorSets commandTable device' descriptorPool' (fromIntegral $ Data.Vector.length descriptorSets') pDescriptorSets' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkResetDescriptorPool - Resets a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @descriptorPool@ is the descriptor pool to be reset.
--
-- -   @flags@ is reserved for future use.
--
-- = Description
--
-- Resetting a descriptor pool recycles all of the resources from all of
-- the descriptor sets allocated from the descriptor pool back to the
-- descriptor pool, and the descriptor sets are implicitly freed.
--
-- == Valid Usage
--
-- -   All uses of @descriptorPool@ (via any allocated descriptor sets)
--     /must/ have completed execution
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @descriptorPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool' handle
--
-- -   @flags@ /must/ be @0@
--
-- -   @descriptorPool@ /must/ have been created, allocated, or retrieved
--     from @device@
--
-- == Host Synchronization
--
-- -   Host access to @descriptorPool@ /must/ be externally synchronized
--
-- -   Host access to any
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' objects
--     allocated from @descriptorPool@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolResetFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
resetDescriptorPool :: Device ->  DescriptorPool ->  DescriptorPoolResetFlags ->  IO ()
resetDescriptorPool = \(Device device' commandTable) -> \descriptorPool' -> \flags' -> vkResetDescriptorPool commandTable device' descriptorPool' flags' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


-- | vkUpdateDescriptorSets - Update the contents of a descriptor set object
--
-- = Parameters
--
-- -   @device@ is the logical device that updates the descriptor sets.
--
-- -   @descriptorWriteCount@ is the number of elements in the
--     @pDescriptorWrites@ array.
--
-- -   @pDescriptorWrites@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
--     structures describing the descriptor sets to write to.
--
-- -   @descriptorCopyCount@ is the number of elements in the
--     @pDescriptorCopies@ array.
--
-- -   @pDescriptorCopies@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet'
--     structures describing the descriptor sets to copy between.
--
-- = Description
--
-- The operations described by @pDescriptorWrites@ are performed first,
-- followed by the operations described by @pDescriptorCopies@. Within each
-- array, the operations are performed in the order they appear in the
-- array.
--
-- Each element in the @pDescriptorWrites@ array describes an operation
-- updating the descriptor set using descriptors for resources specified in
-- the structure.
--
-- Each element in the @pDescriptorCopies@ array is a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet' structure
-- describing an operation copying descriptors between sets.
--
-- If the @dstSet@ member of any element of @pDescriptorWrites@ or
-- @pDescriptorCopies@ is bound, accessed, or modified by any command that
-- was recorded to a command buffer which is currently in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>,
-- that command buffer becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   The @dstSet@ member of each element of @pDescriptorWrites@ or
--     @pDescriptorCopies@ /must/ not be used by any command that was
--     recorded to a command buffer which is in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @descriptorWriteCount@ is not @0@, @pDescriptorWrites@ /must/ be
--     a valid pointer to an array of @descriptorWriteCount@ valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
--     structures
--
-- -   If @descriptorCopyCount@ is not @0@, @pDescriptorCopies@ /must/ be a
--     valid pointer to an array of @descriptorCopyCount@ valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet'
--     structures
--
-- == Host Synchronization
--
-- -   Host access to @pDescriptorWrites@[].dstSet /must/ be externally
--     synchronized
--
-- -   Host access to @pDescriptorCopies@[].dstSet /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
updateDescriptorSets :: Device ->  Vector WriteDescriptorSet ->  Vector CopyDescriptorSet ->  IO ()
updateDescriptorSets = \(Device device' commandTable) -> \descriptorWrites' -> \descriptorCopies' -> withVec withCStructCopyDescriptorSet descriptorCopies' (\pDescriptorCopies' -> withVec withCStructWriteDescriptorSet descriptorWrites' (\pDescriptorWrites' -> vkUpdateDescriptorSets commandTable device' (fromIntegral $ Data.Vector.length descriptorWrites') pDescriptorWrites' (fromIntegral $ Data.Vector.length descriptorCopies') pDescriptorCopies' *> (pure ())))

-- | A safe wrapper for 'createDescriptorPool' and 'destroyDescriptorPool' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorPool
  :: Device -> DescriptorPoolCreateInfo -> Maybe (AllocationCallbacks) -> (DescriptorPool -> IO a) -> IO a
withDescriptorPool device descriptorPoolCreateInfo allocationCallbacks = bracket
  (createDescriptorPool device descriptorPoolCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorPool device o allocationCallbacks)

-- | A safe wrapper for 'createDescriptorSetLayout' and 'destroyDescriptorSetLayout' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorSetLayout
  :: Device -> DescriptorSetLayoutCreateInfo -> Maybe (AllocationCallbacks) -> (DescriptorSetLayout -> IO a) -> IO a
withDescriptorSetLayout device descriptorSetLayoutCreateInfo allocationCallbacks = bracket
  (createDescriptorSetLayout device descriptorSetLayoutCreateInfo allocationCallbacks)
  (\o -> destroyDescriptorSetLayout device o allocationCallbacks)

-- | A safe wrapper for 'allocateDescriptorSets' and 'freeDescriptorSets' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withDescriptorSets
  :: Device -> DescriptorSetAllocateInfo -> (Vector (DescriptorSet) -> IO a) -> IO a
withDescriptorSets device descriptorSetAllocateInfo = bracket
  (allocateDescriptorSets device descriptorSetAllocateInfo)
  (\o -> freeDescriptorSets device (descriptorPool (descriptorSetAllocateInfo :: DescriptorSetAllocateInfo))  o)
