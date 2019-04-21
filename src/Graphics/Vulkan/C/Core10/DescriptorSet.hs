{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorBufferInfo(..)
  , VkDescriptorImageInfo(..)
  , VkDescriptorPool
  , VkDescriptorPoolCreateFlagBits(..)
  , pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT
  , VkDescriptorPoolCreateFlags
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorPoolSize(..)
  , VkDescriptorSet
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorSetLayoutCreateFlagBits(..)
  , VkDescriptorSetLayoutCreateFlags
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorType(..)
  , pattern VK_DESCRIPTOR_TYPE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  , pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER
  , pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
  , pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  , VkWriteDescriptorSet(..)
  , FN_vkAllocateDescriptorSets
  , PFN_vkAllocateDescriptorSets
  , vkAllocateDescriptorSets
  , FN_vkCreateDescriptorPool
  , PFN_vkCreateDescriptorPool
  , vkCreateDescriptorPool
  , FN_vkCreateDescriptorSetLayout
  , PFN_vkCreateDescriptorSetLayout
  , vkCreateDescriptorSetLayout
  , FN_vkDestroyDescriptorPool
  , PFN_vkDestroyDescriptorPool
  , vkDestroyDescriptorPool
  , FN_vkDestroyDescriptorSetLayout
  , PFN_vkDestroyDescriptorSetLayout
  , vkDestroyDescriptorSetLayout
  , FN_vkFreeDescriptorSets
  , PFN_vkFreeDescriptorSets
  , vkFreeDescriptorSets
  , FN_vkResetDescriptorPool
  , PFN_vkResetDescriptorPool
  , vkResetDescriptorPool
  , FN_vkUpdateDescriptorSets
  , PFN_vkUpdateDescriptorSets
  , vkUpdateDescriptorSets
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferView
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageView
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSampler
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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
-- -   If the descriptor type of the descriptor set binding specified by
--     @srcBinding@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @srcArrayElement@ /must/ be an integer multiple of @4@
--
-- -   If the descriptor type of the descriptor set binding specified by
--     @dstBinding@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @dstArrayElement@ /must/ be an integer multiple of @4@
--
-- -   If the descriptor type of the descriptor set binding specified by
--     either @srcBinding@ or @dstBinding@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @descriptorCount@ /must/ be an integer multiple of @4@
--
-- -   If @srcSet@’s layout was created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     flag set, then @dstSet@’s layout /must/ also have been created with
--     the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     flag set
--
-- -   If @srcSet@’s layout was created without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     flag set, then @dstSet@’s layout /must/ also have been created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     flag set
--
-- -   If the descriptor pool from which @srcSet@ was allocated was created
--     with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
--     flag set, then the descriptor pool from which @dstSet@ was allocated
--     /must/ also have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
--     flag set
--
-- -   If the descriptor pool from which @srcSet@ was allocated was created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
--     flag set, then the descriptor pool from which @dstSet@ was allocated
--     /must/ also have been created without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
--     flag set
--
-- Unresolved directive in VkCopyDescriptorSet.txt -
-- include::{generated}\/validity\/structs\/VkCopyDescriptorSet.txt[]
--
-- = See Also
--
-- 'VkDescriptorSet', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkUpdateDescriptorSets'
data VkCopyDescriptorSet = VkCopyDescriptorSet
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcSet@, @srcBinding@, and @srcArrayElement@ are the source set,
  -- binding, and array element, respectively. If the descriptor binding
  -- identified by @srcSet@ and @srcBinding@ has a descriptor type of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @srcArrayElement@ specifies the starting byte offset within the
  -- binding to copy from.
  vkSrcSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcBinding"
  vkSrcBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "srcArrayElement"
  vkSrcArrayElement :: Word32
  , -- | @dstSet@, @dstBinding@, and @dstArrayElement@ are the destination set,
  -- binding, and array element, respectively. If the descriptor binding
  -- identified by @dstSet@ and @dstBinding@ has a descriptor type of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @dstArrayElement@ specifies the starting byte offset within the
  -- binding to copy to.
  vkDstSet :: VkDescriptorSet
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstBinding"
  vkDstBinding :: Word32
  , -- No documentation found for Nested "VkCopyDescriptorSet" "dstArrayElement"
  vkDstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to copy from the source
  -- to destination. If @descriptorCount@ is greater than the number of
  -- remaining array elements in the source or destination binding, those
  -- affect consecutive bindings in a manner similar to
  -- 'VkWriteDescriptorSet' above. If the descriptor binding identified by
  -- @srcSet@ and @srcBinding@ has a descriptor type of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @descriptorCount@ specifies the number of bytes to copy and the
  -- remaining array elements in the source or destination binding refer to
  -- the remaining number of bytes in those.
  vkDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkCopyDescriptorSet where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkCopyDescriptorSet <$> peek (ptr `plusPtr` 0)
                                 <*> peek (ptr `plusPtr` 8)
                                 <*> peek (ptr `plusPtr` 16)
                                 <*> peek (ptr `plusPtr` 24)
                                 <*> peek (ptr `plusPtr` 28)
                                 <*> peek (ptr `plusPtr` 32)
                                 <*> peek (ptr `plusPtr` 40)
                                 <*> peek (ptr `plusPtr` 44)
                                 <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkSrcSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkSrcBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkSrcArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDstSet (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkDstBinding (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 44) (vkDstArrayElement (poked :: VkCopyDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkDescriptorCount (poked :: VkCopyDescriptorSet))

instance Zero VkCopyDescriptorSet where
  zero = VkCopyDescriptorSet VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
                             zero
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
-- For 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' and
-- 'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC' descriptor types, @offset@
-- is the base offset from which the dynamic offset is applied and @range@
-- is the static size used for all dynamic offsets.
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
-- Unresolved directive in VkDescriptorBufferInfo.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorBufferInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'VkWriteDescriptorSet'
data VkDescriptorBufferInfo = VkDescriptorBufferInfo
  { -- | @buffer@ is the buffer resource.
  vkBuffer :: VkBuffer
  , -- | @offset@ is the offset in bytes from the start of @buffer@. Access to
  -- buffer memory via this descriptor uses addressing that is relative to
  -- this starting offset.
  vkOffset :: VkDeviceSize
  , -- | @range@ is the size in bytes that is used for this descriptor update, or
  -- 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE' to use the range from
  -- @offset@ to the end of the buffer.
  vkRange :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkDescriptorBufferInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorBufferInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBuffer (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 8) (vkOffset (poked :: VkDescriptorBufferInfo))
                *> poke (ptr `plusPtr` 16) (vkRange (poked :: VkDescriptorBufferInfo))

instance Zero VkDescriptorBufferInfo where
  zero = VkDescriptorBufferInfo zero
                                zero
                                zero

-- | VkDescriptorImageInfo - Structure specifying descriptor image info
--
-- = Description
--
-- Members of 'VkDescriptorImageInfo' that are not used in an update (as
-- described above) are ignored.
--
-- == Valid Usage
--
-- -   @imageView@ /must/ not be 2D or 2D array image view created from a
--     3D image
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
-- -   If @sampler@ is used and the
--     'Graphics.Vulkan.C.Core10.Core.VkFormat' of the image is a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>,
--     the image /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT',
--     and the @aspectMask@ of the @imageView@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_0_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_1_BIT'
--     or (for three-plane formats only)
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_ASPECT_PLANE_2_BIT'
--
-- Unresolved directive in VkDescriptorImageInfo.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorImageInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler', 'VkWriteDescriptorSet'
data VkDescriptorImageInfo = VkDescriptorImageInfo
  { -- | @sampler@ is a sampler handle, and is used in descriptor updates for
  -- types 'VK_DESCRIPTOR_TYPE_SAMPLER' and
  -- 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER' if the binding being updated
  -- does not use immutable samplers.
  vkSampler :: VkSampler
  , -- | @imageView@ is an image view handle, and is used in descriptor updates
  -- for types 'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
  -- 'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
  -- 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', and
  -- 'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'.
  vkImageView :: VkImageView
  , -- | @imageLayout@ is the layout that the image subresources accessible from
  -- @imageView@ will be in at the time this descriptor is accessed.
  -- @imageLayout@ is used in descriptor updates for types
  -- 'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE', 'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
  -- 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', and
  -- 'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'.
  vkImageLayout :: VkImageLayout
  }
  deriving (Eq, Show)

instance Storable VkDescriptorImageInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorImageInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSampler (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 8) (vkImageView (poked :: VkDescriptorImageInfo))
                *> poke (ptr `plusPtr` 16) (vkImageLayout (poked :: VkDescriptorImageInfo))

instance Zero VkDescriptorImageInfo where
  zero = VkDescriptorImageInfo zero
                               zero
                               zero

-- | Dummy data to tag the 'Ptr' with
data VkDescriptorPool_T
-- | VkDescriptorPool - Opaque handle to a descriptor pool object
--
-- = See Also
--
-- 'VkDescriptorSetAllocateInfo', 'vkCreateDescriptorPool',
-- 'vkDestroyDescriptorPool', 'vkFreeDescriptorSets',
-- 'vkResetDescriptorPool'
type VkDescriptorPool = Ptr VkDescriptorPool_T

-- ** VkDescriptorPoolCreateFlagBits

-- | VkDescriptorPoolCreateFlagBits - Bitmask specifying certain supported
-- operations on a descriptor pool
--
-- = See Also
--
-- 'VkDescriptorPoolCreateFlags'
newtype VkDescriptorPoolCreateFlagBits = VkDescriptorPoolCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDescriptorPoolCreateFlagBits where
  showsPrec _ VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = showString "VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorPoolCreateFlagBits 0x00000002) = showString "VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT"
  showsPrec p (VkDescriptorPoolCreateFlagBits x) = showParen (p >= 11) (showString "VkDescriptorPoolCreateFlagBits " . showsPrec 11 x)

instance Read VkDescriptorPoolCreateFlagBits where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT", pure VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT", pure (VkDescriptorPoolCreateFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorPoolCreateFlagBits")
                        v <- step readPrec
                        pure (VkDescriptorPoolCreateFlagBits v)
                        )
                    )

-- | 'VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT' specifies that
-- descriptor sets /can/ return their individual allocations to the pool,
-- i.e. all of 'vkAllocateDescriptorSets', 'vkFreeDescriptorSets', and
-- 'vkResetDescriptorPool' are allowed. Otherwise, descriptor sets
-- allocated from the pool /must/ not be individually freed back to the
-- pool, i.e. only 'vkAllocateDescriptorSets' and 'vkResetDescriptorPool'
-- are allowed.
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT :: VkDescriptorPoolCreateFlagBits
pattern VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT = VkDescriptorPoolCreateFlagBits 0x00000001

-- | VkDescriptorPoolCreateFlags - Bitmask of VkDescriptorPoolCreateFlagBits
--
-- = Description
--
-- 'VkDescriptorPoolCreateFlags' is a bitmask type for setting a mask of
-- zero or more 'VkDescriptorPoolCreateFlagBits'.
--
-- = See Also
--
-- 'VkDescriptorPoolCreateFlagBits', 'VkDescriptorPoolCreateInfo'
type VkDescriptorPoolCreateFlags = VkDescriptorPoolCreateFlagBits

-- | VkDescriptorPoolCreateInfo - Structure specifying parameters of a newly
-- created descriptor pool
--
-- = Description
--
-- If multiple 'VkDescriptorPoolSize' structures appear in the @pPoolSizes@
-- array then the pool will be created with enough storage for the total
-- number of descriptors of each type.
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
-- without the 'VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT' bit
-- set). Additionally, if all sets allocated from the pool since it was
-- created or most recently reset use the same number of descriptors (of
-- each type) and the requested allocation also uses that same number of
-- descriptors (of each type), then fragmentation /must/ not cause an
-- allocation failure.
--
-- If an allocation failure occurs due to fragmentation, an application
-- /can/ create an additional descriptor pool to perform further descriptor
-- set allocations.
--
-- If @flags@ has the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
-- bit set, descriptor pool creation /may/ fail with the error
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_ERROR_FRAGMENTATION_EXT'
-- if the total number of descriptors across all pools (including this one)
-- created with this bit set exceeds
-- @maxUpdateAfterBindDescriptorsInAllPools@, or if fragmentation of the
-- underlying hardware resources occurs.
--
-- == Valid Usage
--
-- Unresolved directive in VkDescriptorPoolCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorPoolCreateInfo.txt[]
--
-- = See Also
--
-- 'VkDescriptorPoolCreateFlags', 'VkDescriptorPoolSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateDescriptorPool'
data VkDescriptorPoolCreateInfo = VkDescriptorPoolCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkDescriptorPoolCreateFlagBits' specifying
  -- certain supported operations on the pool.
  vkFlags :: VkDescriptorPoolCreateFlags
  , -- | @maxSets@ /must/ be greater than @0@
  vkMaxSets :: Word32
  , -- | @poolSizeCount@ is the number of elements in @pPoolSizes@.
  vkPoolSizeCount :: Word32
  , -- | @pPoolSizes@ is a pointer to an array of 'VkDescriptorPoolSize'
  -- structures, each containing a descriptor type and number of descriptors
  -- of that type to be allocated in the pool.
  vkPPoolSizes :: Ptr VkDescriptorPoolSize
  }
  deriving (Eq, Show)

instance Storable VkDescriptorPoolCreateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDescriptorPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 24)
                                        <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkMaxSets (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPoolSizeCount (poked :: VkDescriptorPoolCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkPPoolSizes (poked :: VkDescriptorPoolCreateInfo))

instance Zero VkDescriptorPoolCreateInfo where
  zero = VkDescriptorPoolCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- ** VkDescriptorPoolResetFlags

-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'VkDescriptorPoolResetFlags' is a bitmask type for setting a mask, but
-- is currently reserved for future use.
--
-- = See Also
--
-- 'vkResetDescriptorPool'
newtype VkDescriptorPoolResetFlags = VkDescriptorPoolResetFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDescriptorPoolResetFlags where
  
  showsPrec p (VkDescriptorPoolResetFlags x) = showParen (p >= 11) (showString "VkDescriptorPoolResetFlags " . showsPrec 11 x)

instance Read VkDescriptorPoolResetFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorPoolResetFlags")
                        v <- step readPrec
                        pure (VkDescriptorPoolResetFlags v)
                        )
                    )



-- | VkDescriptorPoolSize - Structure specifying descriptor pool size
--
-- == Valid Usage
--
-- -   @descriptorCount@ /must/ be greater than @0@
--
-- -   If @type@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     then @descriptorCount@ /must/ be a multiple of @4@
--
-- Unresolved directive in VkDescriptorPoolSize.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorPoolSize.txt[]
--
-- = See Also
--
-- 'VkDescriptorPoolCreateInfo', 'VkDescriptorType'
data VkDescriptorPoolSize = VkDescriptorPoolSize
  { -- | @type@ is the type of descriptor.
  vkType :: VkDescriptorType
  , -- | @descriptorCount@ is the number of descriptors of that type to allocate.
  -- If @type@ is
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @descriptorCount@ is the number of bytes to allocate for
  -- descriptors of this type.
  vkDescriptorCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDescriptorPoolSize where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkDescriptorPoolSize <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkType (poked :: VkDescriptorPoolSize))
                *> poke (ptr `plusPtr` 4) (vkDescriptorCount (poked :: VkDescriptorPoolSize))

instance Zero VkDescriptorPoolSize where
  zero = VkDescriptorPoolSize zero
                              zero

-- | Dummy data to tag the 'Ptr' with
data VkDescriptorSet_T
-- | VkDescriptorSet - Opaque handle to a descriptor set object
--
-- = See Also
--
-- 'VkCopyDescriptorSet', 'VkWriteDescriptorSet',
-- 'vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
-- 'vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate'
type VkDescriptorSet = Ptr VkDescriptorSet_T

-- | VkDescriptorSetAllocateInfo - Structure specifying the allocation
-- parameters for descriptor sets
--
-- == Valid Usage
--
-- -   Each element of @pSetLayouts@ /must/ not have been created with
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR'
--     set
--
-- -   If any element of @pSetLayouts@ was created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--     bit set, @descriptorPool@ /must/ have been created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_POOL_CREATE_UPDATE_AFTER_BIND_BIT_EXT'
--     flag set
--
-- Unresolved directive in VkDescriptorSetAllocateInfo.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetAllocateInfo.txt[]
--
-- = See Also
--
-- 'VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkAllocateDescriptorSets'
data VkDescriptorSetAllocateInfo = VkDescriptorSetAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @descriptorPool@ is the pool which the sets will be allocated from.
  vkDescriptorPool :: VkDescriptorPool
  , -- | @descriptorSetCount@ determines the number of descriptor sets to be
  -- allocated from the pool.
  vkDescriptorSetCount :: Word32
  , -- | @pSetLayouts@ is an array of descriptor set layouts, with each member
  -- specifying how the corresponding descriptor set is allocated.
  vkPSetLayouts :: Ptr VkDescriptorSetLayout
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetAllocateInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkDescriptorSetAllocateInfo <$> peek (ptr `plusPtr` 0)
                                         <*> peek (ptr `plusPtr` 8)
                                         <*> peek (ptr `plusPtr` 16)
                                         <*> peek (ptr `plusPtr` 24)
                                         <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkDescriptorPool (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkDescriptorSetCount (poked :: VkDescriptorSetAllocateInfo))
                *> poke (ptr `plusPtr` 32) (vkPSetLayouts (poked :: VkDescriptorSetAllocateInfo))

instance Zero VkDescriptorSetAllocateInfo where
  zero = VkDescriptorSetAllocateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                                     zero
                                     zero
                                     zero
                                     zero

-- | VkDescriptorSetLayoutBinding - Structure specifying a descriptor set
-- layout binding
--
-- = Description
--
-- -   @pImmutableSamplers@ affects initialization of samplers. If
--     @descriptorType@ specifies a 'VK_DESCRIPTOR_TYPE_SAMPLER' or
--     'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER' type descriptor, then
--     @pImmutableSamplers@ /can/ be used to initialize a set of /immutable
--     samplers/. Immutable samplers are permanently bound into the set
--     layout; later binding a sampler into an immutable sampler slot in a
--     descriptor set is not allowed. If @pImmutableSamplers@ is not
--     @NULL@, then it is considered to be a pointer to an array of sampler
--     handles that will be consumed by the set layout and used for the
--     corresponding binding. If @pImmutableSamplers@ is @NULL@, then the
--     sampler slots are dynamic and sampler handles /must/ be bound into
--     descriptor sets using this layout. If @descriptorType@ is not one of
--     these descriptor types, then @pImmutableSamplers@ is ignored.
--
-- The above layout definition allows the descriptor bindings to be
-- specified sparsely such that not all binding numbers between 0 and the
-- maximum binding number need to be specified in the @pBindings@ array.
-- Bindings that are not specified have a @descriptorCount@ and
-- @stageFlags@ of zero, and the value of @descriptorType@ is undefined.
-- However, all binding numbers between 0 and the maximum binding number in
-- the 'VkDescriptorSetLayoutCreateInfo'::@pBindings@ array /may/ consume
-- memory in the descriptor set layout even if not all descriptor bindings
-- are used, though it /should/ not consume additional memory from the
-- descriptor pool.
--
-- __Note__
--
-- The maximum binding number specified /should/ be as compact as possible
-- to avoid wasted memory.
--
-- == Valid Usage
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_SAMPLER' or
--     'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', and @descriptorCount@
--     is not @0@ and @pImmutableSamplers@ is not @NULL@,
--     @pImmutableSamplers@ /must/ be a valid pointer to an array of
--     @descriptorCount@ valid 'Graphics.Vulkan.C.Core10.Sampler.VkSampler'
--     handles
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     then @descriptorCount@ /must/ be a multiple of @4@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     then @descriptorCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkPhysicalDeviceInlineUniformBlockPropertiesEXT'::@maxInlineUniformBlockSize@
--
-- -   If @descriptorCount@ is not @0@, @stageFlags@ /must/ be a valid
--     combination of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits' values
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT' and
--     @descriptorCount@ is not @0@, then @stageFlags@ /must/ be @0@ or
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_FRAGMENT_BIT'
--
-- Unresolved directive in VkDescriptorSetLayoutBinding.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetLayoutBinding.txt[]
--
-- = See Also
--
-- 'VkDescriptorSetLayoutCreateInfo', 'VkDescriptorType',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSampler',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags'
data VkDescriptorSetLayoutBinding = VkDescriptorSetLayoutBinding
  { -- | @binding@ is the binding number of this entry and corresponds to a
  -- resource of the same binding number in the shader stages.
  vkBinding :: Word32
  , -- | @descriptorType@ is a 'VkDescriptorType' specifying which type of
  -- resource descriptors are used for this binding.
  vkDescriptorType :: VkDescriptorType
  , -- | @descriptorCount@ is the number of descriptors contained in the binding,
  -- accessed in a shader as an array , except if @descriptorType@ is
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- in which case @descriptorCount@ is the size in bytes of the inline
  -- uniform block . If @descriptorCount@ is zero this binding entry is
  -- reserved and the resource /must/ not be accessed from any stage via this
  -- binding within any pipeline using the set layout.
  vkDescriptorCount :: Word32
  , -- | @stageFlags@ member is a bitmask of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits' specifying
  -- which pipeline shader stages /can/ access a resource for this binding.
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_ALL' is a shorthand
  -- specifying that all defined shader stages, including any additional
  -- stages defined by extensions, /can/ access the resource.
  --
  -- If a shader stage is not included in @stageFlags@, then a resource
  -- /must/ not be accessed from that stage via this binding within any
  -- pipeline using the set layout. Other than input attachments which are
  -- limited to the fragment shader, there are no limitations on what
  -- combinations of stages /can/ use a descriptor binding, and in particular
  -- a binding /can/ be used by both graphics stages and the compute stage.
  vkStageFlags :: VkShaderStageFlags
  , -- No documentation found for Nested "VkDescriptorSetLayoutBinding" "pImmutableSamplers"
  vkPImmutableSamplers :: Ptr VkSampler
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutBinding where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutBinding <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBinding (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 4) (vkDescriptorType (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 8) (vkDescriptorCount (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 12) (vkStageFlags (poked :: VkDescriptorSetLayoutBinding))
                *> poke (ptr `plusPtr` 16) (vkPImmutableSamplers (poked :: VkDescriptorSetLayoutBinding))

instance Zero VkDescriptorSetLayoutBinding where
  zero = VkDescriptorSetLayoutBinding zero
                                      zero
                                      zero
                                      zero
                                      zero

-- ** VkDescriptorSetLayoutCreateFlagBits

-- | VkDescriptorSetLayoutCreateFlagBits - Bitmask specifying descriptor set
-- layout properties
--
-- = See Also
--
-- 'VkDescriptorSetLayoutCreateFlags'
newtype VkDescriptorSetLayoutCreateFlagBits = VkDescriptorSetLayoutCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkDescriptorSetLayoutCreateFlagBits where
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorSetLayoutCreateFlagBits 0x00000001) = showString "VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR"
  showsPrec _ (VkDescriptorSetLayoutCreateFlagBits 0x00000002) = showString "VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT"
  showsPrec p (VkDescriptorSetLayoutCreateFlagBits x) = showParen (p >= 11) (showString "VkDescriptorSetLayoutCreateFlagBits " . showsPrec 11 x)

instance Read VkDescriptorSetLayoutCreateFlagBits where
  readPrec = parens ( choose [ -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR",        pure (VkDescriptorSetLayoutCreateFlagBits 0x00000001))
                             , ("VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT", pure (VkDescriptorSetLayoutCreateFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorSetLayoutCreateFlagBits")
                        v <- step readPrec
                        pure (VkDescriptorSetLayoutCreateFlagBits v)
                        )
                    )



-- | VkDescriptorSetLayoutCreateFlags - Bitmask of
-- VkDescriptorSetLayoutCreateFlagBits
--
-- = Description
--
-- 'VkDescriptorSetLayoutCreateFlags' is a bitmask type for setting a mask
-- of zero or more 'VkDescriptorSetLayoutCreateFlagBits'.
--
-- = See Also
--
-- 'VkDescriptorSetLayoutCreateFlagBits', 'VkDescriptorSetLayoutCreateInfo'
type VkDescriptorSetLayoutCreateFlags = VkDescriptorSetLayoutCreateFlagBits

-- | VkDescriptorSetLayoutCreateInfo - Structure specifying parameters of a
-- newly created descriptor set layout
--
-- == Valid Usage
--
-- -   The 'VkDescriptorSetLayoutBinding'::@binding@ members of the
--     elements of the @pBindings@ array /must/ each have different values.
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then all elements of @pBindings@ /must/ not have a @descriptorType@
--     of 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then all elements of @pBindings@ /must/ not have a @descriptorType@
--     of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--
-- -   If @flags@ contains
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VK_DESCRIPTOR_SET_LAYOUT_CREATE_PUSH_DESCRIPTOR_BIT_KHR',
--     then the total number of elements of all bindings /must/ be less
--     than or equal to
--     'Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor.VkPhysicalDevicePushDescriptorPropertiesKHR'::@maxPushDescriptors@
--
-- -   If any binding has the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--     bit set, @flags@ /must/ include
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_SET_LAYOUT_CREATE_UPDATE_AFTER_BIND_POOL_BIT_EXT'
--
-- -   If any binding has the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--     bit set, then all bindings /must/ not have @descriptorType@ of
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--
-- Unresolved directive in VkDescriptorSetLayoutCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkDescriptorSetLayoutCreateInfo.txt[]
--
-- = See Also
--
-- 'VkDescriptorSetLayoutBinding', 'VkDescriptorSetLayoutCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport'
data VkDescriptorSetLayoutCreateInfo = VkDescriptorSetLayoutCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is a bitmask of 'VkDescriptorSetLayoutCreateFlagBits' specifying
  -- options for descriptor set layout creation.
  vkFlags :: VkDescriptorSetLayoutCreateFlags
  , -- | @bindingCount@ is the number of elements in @pBindings@.
  vkBindingCount :: Word32
  , -- | @pBindings@ is a pointer to an array of 'VkDescriptorSetLayoutBinding'
  -- structures.
  vkPBindings :: Ptr VkDescriptorSetLayoutBinding
  }
  deriving (Eq, Show)

instance Storable VkDescriptorSetLayoutCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDescriptorSetLayoutCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 20)
                                             <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkBindingCount (poked :: VkDescriptorSetLayoutCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPBindings (poked :: VkDescriptorSetLayoutCreateInfo))

instance Zero VkDescriptorSetLayoutCreateInfo where
  zero = VkDescriptorSetLayoutCreateInfo VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
                                         zero
                                         zero
                                         zero
                                         zero

-- ** VkDescriptorType

-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   'VK_DESCRIPTOR_TYPE_SAMPLER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampler sampler descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler combined image sampler descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage image descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic dynamic uniform buffer descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic dynamic storage buffer descriptor>.
--
-- -   'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT' specifies an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-inputattachment input attachment descriptor>.
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     specifies an
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>.
--
-- When a descriptor set is updated via elements of 'VkWriteDescriptorSet',
-- members of @pImageInfo@, @pBufferInfo@ and @pTexelBufferView@ are only
-- accessed by the implementation when they correspond to descriptor type
-- being defined - otherwise they are ignored. The members accessed are as
-- follows for each descriptor type:
--
-- -   For 'VK_DESCRIPTOR_TYPE_SAMPLER', only the @sampler@ member of each
--     element of 'VkWriteDescriptorSet'::@pImageInfo@ is accessed.
--
-- -   For 'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE', or
--     'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT', only the @imageView@ and
--     @imageLayout@ members of each element of
--     'VkWriteDescriptorSet'::@pImageInfo@ are accessed.
--
-- -   For 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', all members of each
--     element of 'VkWriteDescriptorSet'::@pImageInfo@ are accessed.
--
-- -   For 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', all members of each
--     element of 'VkWriteDescriptorSet'::@pBufferInfo@ are accessed.
--
-- -   For 'VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER', each element of
--     'VkWriteDescriptorSet'::@pTexelBufferView@ is accessed.
--
-- When updating descriptors with a @descriptorType@ of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
-- none of the @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ members
-- are accessed, instead the source data of the descriptor update operation
-- is taken from the instance of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkWriteDescriptorSetInlineUniformBlockEXT'
-- in the @pNext@ chain of 'VkWriteDescriptorSet'. When updating
-- descriptors with a @descriptorType@ of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
-- none of the @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ members
-- are accessed, instead the source data of the descriptor update operation
-- is taken from the instance of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkWriteDescriptorSetAccelerationStructureNV'
-- in the @pNext@ chain of 'VkWriteDescriptorSet'.
--
-- = See Also
--
-- 'VkDescriptorPoolSize', 'VkDescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateEntry',
-- 'VkWriteDescriptorSet'
newtype VkDescriptorType = VkDescriptorType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkDescriptorType where
  showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLER = showString "VK_DESCRIPTOR_TYPE_SAMPLER"
  showsPrec _ VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = showString "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
  showsPrec _ VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = showString "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = showString "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
  showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = showString "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = showString "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
  showsPrec _ VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = showString "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
  showsPrec _ VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = showString "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
  showsPrec _ VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = showString "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkDescriptorType 1000138000) = showString "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
  showsPrec _ (VkDescriptorType 1000165000) = showString "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
  showsPrec p (VkDescriptorType x) = showParen (p >= 11) (showString "VkDescriptorType " . showsPrec 11 x)

instance Read VkDescriptorType where
  readPrec = parens ( choose [ ("VK_DESCRIPTOR_TYPE_SAMPLER",                pure VK_DESCRIPTOR_TYPE_SAMPLER)
                             , ("VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER", pure VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
                             , ("VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE",          pure VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_IMAGE",          pure VK_DESCRIPTOR_TYPE_STORAGE_IMAGE)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER",   pure VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER",   pure VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER",         pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER",         pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER)
                             , ("VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC", pure VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC)
                             , ("VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC", pure VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC)
                             , ("VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT",       pure VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT",  pure (VkDescriptorType 1000138000))
                             , ("VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV", pure (VkDescriptorType 1000165000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkDescriptorType")
                        v <- step readPrec
                        pure (VkDescriptorType v)
                        )
                    )

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLER"
pattern VK_DESCRIPTOR_TYPE_SAMPLER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_SAMPLER = VkDescriptorType 0

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = VkDescriptorType 1

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE = VkDescriptorType 2

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_IMAGE = VkDescriptorType 3

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = VkDescriptorType 4

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = VkDescriptorType 5

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER = VkDescriptorType 6

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER = VkDescriptorType 7

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = VkDescriptorType 8

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = VkDescriptorType 9

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT :: VkDescriptorType
pattern VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT = VkDescriptorType 10

-- | VkWriteDescriptorSet - Structure specifying the parameters of a
-- descriptor set write operation
--
-- = Description
--
-- Only one of @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ members
-- is used according to the descriptor type specified in the
-- @descriptorType@ member of the containing 'VkWriteDescriptorSet'
-- structure, or none of them in case @descriptorType@ is
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
-- in which case the source data for the descriptor writes is taken from
-- the instance of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkWriteDescriptorSetInlineUniformBlockEXT'
-- in the @pNext@ chain of 'VkWriteDescriptorSet', or if @descriptorType@
-- is
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
-- in which case the source data for the descriptor writes is taken from
-- the instance of
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkWriteDescriptorSetAccelerationStructureNV'
-- in the @pNext@ chain of 'VkWriteDescriptorSet', as specified below.
--
-- If the @dstBinding@ has fewer than @descriptorCount@ array elements
-- remaining starting from @dstArrayElement@, then the remainder will be
-- used to update the subsequent binding - @dstBinding@+1 starting at array
-- element zero. If a binding has a @descriptorCount@ of zero, it is
-- skipped. This behavior applies recursively, with the update affecting
-- consecutive bindings as needed to update all @descriptorCount@
-- descriptors.
--
-- __Note__
--
-- The same behavior applies to bindings with a descriptor type of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
-- where @descriptorCount@ specifies the number of bytes to update while
-- @dstArrayElement@ specifies the starting byte offset, thus in this case
-- if the @dstBinding@ has a smaller byte size than the sum of
-- @dstArrayElement@ and @descriptorCount@, then the remainder will be used
-- to update the subsequent binding - @dstBinding@+1 starting at offset
-- zero. This falls out as a special case of the above rule.
--
-- == Valid Usage
--
-- -   @dstBinding@ /must/ be less than or equal to the maximum value of
--     @binding@ of all 'VkDescriptorSetLayoutBinding' structures specified
--     when @dstSet@’s descriptor set layout was created
--
-- -   @dstBinding@ /must/ be a binding with a non-zero @descriptorCount@
--
-- -   All consecutive bindings updated via a single 'VkWriteDescriptorSet'
--     structure, except those with a @descriptorCount@ of zero, /must/
--     have identical @descriptorType@ and @stageFlags@.
--
-- -   All consecutive bindings updated via a single 'VkWriteDescriptorSet'
--     structure, except those with a @descriptorCount@ of zero, /must/ all
--     either use immutable samplers or /must/ all not use immutable
--     samplers.
--
-- -   @descriptorType@ /must/ match the type of @dstBinding@ within
--     @dstSet@
--
-- -   @dstSet@ /must/ be a valid 'VkDescriptorSet' handle
--
-- -   The sum of @dstArrayElement@ and @descriptorCount@ /must/ be less
--     than or equal to the number of array elements in the descriptor set
--     binding specified by @dstBinding@, and all applicable consecutive
--     bindings, as described by
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-updates-consecutive>
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @dstArrayElement@ /must/ be an integer multiple of @4@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     @descriptorCount@ /must/ be an integer multiple of @4@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_SAMPLER',
--     'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE', or
--     'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT', @pImageInfo@ /must/ be a
--     valid pointer to an array of @descriptorCount@ valid
--     'VkDescriptorImageInfo' structures
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER', @pTexelBufferView@ /must/
--     be a valid pointer to an array of @descriptorCount@ valid
--     'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' handles
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', @pBufferInfo@ /must/ be
--     a valid pointer to an array of @descriptorCount@ valid
--     'VkDescriptorBufferInfo' structures
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_SAMPLER' or
--     'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', and @dstSet@ was not
--     allocated with a layout that included immutable samplers for
--     @dstBinding@ with @descriptorType@, the @sampler@ member of each
--     element of @pImageInfo@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE', or
--     'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT', the @imageView@ and
--     @imageLayout@ members of each element of @pImageInfo@ /must/ be a
--     valid 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' and
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout', respectively
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT',
--     the @pNext@ chain /must/ include a
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkWriteDescriptorSetInlineUniformBlockEXT'
--     structure whose @dataSize@ member equals @descriptorCount@
--
-- -   If @descriptorType@ is
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV',
--     the @pNext@ chain /must/ include a
--     'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkWriteDescriptorSetAccelerationStructureNV'
--     structure whose @accelerationStructureCount@ member equals
--     @descriptorCount@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE', then the
--     @imageView@ member of each @pImageInfo@ element /must/ have been
--     created without a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and if any element of @pImageInfo@ has a @imageView@ member that was
--     created with a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain, then @dstSet@ /must/ have been
--     allocated with a layout that included immutable samplers for
--     @dstBinding@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and @dstSet@ was allocated with a layout that included immutable
--     samplers for @dstBinding@, then the @imageView@ member of each
--     element of @pImageInfo@ which corresponds to an immutable sampler
--     that enables
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y’CBCR conversion>
--     /must/ have been created with a
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--     structure in its @pNext@ chain with an /identically defined/
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo'
--     to the corresponding immutable sampler
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE', for each
--     descriptor that will be accessed via load or store operations the
--     @imageLayout@ member for corresponding elements of @pImageInfo@
--     /must/ be 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', the @offset@ member of
--     each element of @pBufferInfo@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', the @offset@ member of
--     each element of @pBufferInfo@ /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER', or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', and the @buffer@ member
--     of any element of @pBufferInfo@ is the handle of a non-sparse
--     buffer, then that buffer /must/ be bound completely and contiguously
--     to a single 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', the @buffer@ member of
--     each element of @pBufferInfo@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', the @buffer@ member of
--     each element of @pBufferInfo@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', the @range@ member of
--     each element of @pBufferInfo@, or the effective range if @range@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', /must/ be less
--     than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxUniformBufferRange@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER' or
--     'VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', the @range@ member of
--     each element of @pBufferInfo@, or the effective range if @range@ is
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', /must/ be less
--     than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxStorageBufferRange@
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
--     the 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' that each
--     element of @pTexelBufferView@ was created from /must/ have been
--     created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
--     the 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' that each
--     element of @pTexelBufferView@ was created from /must/ have been
--     created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
--     set
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE' or
--     'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT', the @imageView@ member of
--     each element of @pImageInfo@ /must/ have been created with the
--     identity swizzle
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE' or
--     'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', the @imageView@ member
--     of each element of @pImageInfo@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
--     set
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE' or
--     'VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', the @imageLayout@
--     member of each element of @pImageInfo@ /must/ be a member of the
--     list given in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage Sampled Image>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler Combined Image Sampler>,
--     corresponding to its type
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT', the
--     @imageView@ member of each element of @pImageInfo@ /must/ have been
--     created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     set
--
-- -   If @descriptorType@ is 'VK_DESCRIPTOR_TYPE_STORAGE_IMAGE', the
--     @imageView@ member of each element of @pImageInfo@ /must/ have been
--     created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT'
--     set
--
-- -   All consecutive bindings updated via a single 'VkWriteDescriptorSet'
--     structure, except those with a @descriptorCount@ of zero, /must/
--     have identical
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VkDescriptorBindingFlagBitsEXT'.
--
-- Unresolved directive in VkWriteDescriptorSet.txt -
-- include::{generated}\/validity\/structs\/VkWriteDescriptorSet.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView',
-- 'VkDescriptorBufferInfo', 'VkDescriptorImageInfo', 'VkDescriptorSet',
-- 'VkDescriptorType', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkUpdateDescriptorSets'
data VkWriteDescriptorSet = VkWriteDescriptorSet
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @dstSet@ is the destination descriptor set to update.
  vkDstSet :: VkDescriptorSet
  , -- | @dstBinding@ is the descriptor binding within that set.
  vkDstBinding :: Word32
  , -- | @dstArrayElement@ is the starting element in that array. If the
  -- descriptor binding identified by @dstSet@ and @dstBinding@ has a
  -- descriptor type of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @dstArrayElement@ specifies the starting byte offset within the
  -- binding.
  vkDstArrayElement :: Word32
  , -- | @descriptorCount@ is the number of descriptors to update (the number of
  -- elements in @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@ , or a
  -- value matching the @dataSize@ member of an instance of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkWriteDescriptorSetInlineUniformBlockEXT'
  -- in the @pNext@ chain , or a value matching the
  -- @accelerationStructureCount@ of an instance of
  -- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkWriteDescriptorSetAccelerationStructureNV'
  -- in the @pNext@ chain ). If the descriptor binding identified by @dstSet@
  -- and @dstBinding@ has a descriptor type of
  -- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
  -- then @descriptorCount@ specifies the number of bytes to update.
  vkDescriptorCount :: Word32
  , -- | @descriptorType@ is a 'VkDescriptorType' specifying the type of each
  -- descriptor in @pImageInfo@, @pBufferInfo@, or @pTexelBufferView@, as
  -- described below. It /must/ be the same type as that specified in
  -- 'VkDescriptorSetLayoutBinding' for @dstSet@ at @dstBinding@. The type of
  -- the descriptor also controls which array the descriptors are taken from.
  vkDescriptorType :: VkDescriptorType
  , -- | @pImageInfo@ points to an array of 'VkDescriptorImageInfo' structures or
  -- is ignored, as described below.
  vkPImageInfo :: Ptr VkDescriptorImageInfo
  , -- | @pBufferInfo@ points to an array of 'VkDescriptorBufferInfo' structures
  -- or is ignored, as described below.
  vkPBufferInfo :: Ptr VkDescriptorBufferInfo
  , -- | @pTexelBufferView@ points to an array of
  -- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferView' handles as described
  -- in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-buffer-views Buffer Views>
  -- section or is ignored, as described below.
  vkPTexelBufferView :: Ptr VkBufferView
  }
  deriving (Eq, Show)

instance Storable VkWriteDescriptorSet where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkWriteDescriptorSet <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 16) (vkDstSet (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 24) (vkDstBinding (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 28) (vkDstArrayElement (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 32) (vkDescriptorCount (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 36) (vkDescriptorType (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 40) (vkPImageInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 48) (vkPBufferInfo (poked :: VkWriteDescriptorSet))
                *> poke (ptr `plusPtr` 56) (vkPTexelBufferView (poked :: VkWriteDescriptorSet))

instance Zero VkWriteDescriptorSet where
  zero = VkWriteDescriptorSet VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero

-- | vkAllocateDescriptorSets - Allocate one or more descriptor sets
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the descriptor pool.
--
-- -   @pAllocateInfo@ is a pointer to an instance of the
--     'VkDescriptorSetAllocateInfo' structure describing parameters of the
--     allocation.
--
-- -   @pDescriptorSets@ is a pointer to an array of 'VkDescriptorSet'
--     handles in which the resulting descriptor set objects are returned.
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
-- -   For descriptor set bindings created with the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
--     bit set, all descriptors in that binding that are dynamically used
--     /must/ have been populated before the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-binding consumed>.
--
-- -   For descriptor set bindings created without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_PARTIALLY_BOUND_BIT_EXT'
--     bit set, all descriptors in that binding that are statically used
--     /must/ have been populated before the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-binding consumed>.
--
-- -   Descriptor bindings with descriptor type of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT'
--     /can/ be undefined when the descriptor set is
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-binding consumed>;
--     though values in that block will be undefined.
--
-- -   Entries that are not used by a pipeline /can/ have undefined
--     descriptors.
--
-- If a call to 'vkAllocateDescriptorSets' would cause the total number of
-- descriptor sets allocated from the pool to exceed the value of
-- 'VkDescriptorPoolCreateInfo'::@maxSets@ used to create
-- @pAllocateInfo@->@descriptorPool@, then the allocation /may/ fail due to
-- lack of space in the descriptor pool. Similarly, the allocation /may/
-- fail due to lack of space if the call to 'vkAllocateDescriptorSets'
-- would cause the number of any given descriptor type to exceed the sum of
-- all the @descriptorCount@ members of each element of
-- 'VkDescriptorPoolCreateInfo'::@pPoolSizes@ with a @member@ equal to that
-- type.
--
-- Additionally, the allocation /may/ also fail if a call to
-- 'vkAllocateDescriptorSets' would cause the total number of inline
-- uniform block bindings allocated from the pool to exceed the value of
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block.VkDescriptorPoolInlineUniformBlockCreateInfoEXT'::@maxInlineUniformBlockBindings@
-- used to create the descriptor pool.
--
-- If the allocation fails due to no more space in the descriptor pool, and
-- not because of system or device memory exhaustion, then
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_ERROR_OUT_OF_POOL_MEMORY'
-- /must/ be returned.
--
-- 'vkAllocateDescriptorSets' /can/ be used to create multiple descriptor
-- sets. If the creation of any of those descriptor sets fails, then the
-- implementation /must/ destroy all successfully created descriptor set
-- objects from this command, set all entries of the @pDescriptorSets@
-- array to 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and return
-- the error.
--
-- Unresolved directive in vkAllocateDescriptorSets.txt -
-- include::{generated}\/validity\/protos\/vkAllocateDescriptorSets.txt[]
--
-- = See Also
--
-- 'VkDescriptorSet', 'VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkAllocateDescriptorSets" vkAllocateDescriptorSets :: ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
#else
vkAllocateDescriptorSets :: DeviceCmds -> ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
vkAllocateDescriptorSets deviceCmds = mkVkAllocateDescriptorSets (pVkAllocateDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
#endif

type FN_vkAllocateDescriptorSets = ("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
type PFN_vkAllocateDescriptorSets = FunPtr FN_vkAllocateDescriptorSets

-- | vkCreateDescriptorPool - Creates a descriptor pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkDescriptorPoolCreateInfo' structure specifying the state of the
--     descriptor pool object.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pDescriptorPool@ points to a 'VkDescriptorPool' handle in which the
--     resulting descriptor pool object is returned.
--
-- = Description
--
-- @pAllocator@ controls host memory allocation as described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
-- chapter.
--
-- The created descriptor pool is returned in @pDescriptorPool@.
--
-- Unresolved directive in vkCreateDescriptorPool.txt -
-- include::{generated}\/validity\/protos\/vkCreateDescriptorPool.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDescriptorPool', 'VkDescriptorPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorPool" vkCreateDescriptorPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
#else
vkCreateDescriptorPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
vkCreateDescriptorPool deviceCmds = mkVkCreateDescriptorPool (pVkCreateDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult)
#endif

type FN_vkCreateDescriptorPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult
type PFN_vkCreateDescriptorPool = FunPtr FN_vkCreateDescriptorPool

-- | vkCreateDescriptorSetLayout - Create a new descriptor set layout
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the descriptor set
--     layout.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkDescriptorSetLayoutCreateInfo' structure specifying the state of
--     the descriptor set layout object.
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
-- = Description
--
-- Unresolved directive in vkCreateDescriptorSetLayout.txt -
-- include::{generated}\/validity\/protos\/vkCreateDescriptorSetLayout.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateDescriptorSetLayout" vkCreateDescriptorSetLayout :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
#else
vkCreateDescriptorSetLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
vkCreateDescriptorSetLayout deviceCmds = mkVkCreateDescriptorSetLayout (pVkCreateDescriptorSetLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorSetLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult)
#endif

type FN_vkCreateDescriptorSetLayout = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult
type PFN_vkCreateDescriptorSetLayout = FunPtr FN_vkCreateDescriptorSetLayout

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
-- Unresolved directive in vkDestroyDescriptorPool.txt -
-- include::{generated}\/validity\/protos\/vkDestroyDescriptorPool.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'VkDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorPool" vkDestroyDescriptorPool :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDescriptorPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorPool deviceCmds = mkVkDestroyDescriptorPool (pVkDestroyDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDescriptorPool = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorPool = FunPtr FN_vkDestroyDescriptorPool

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
-- Unresolved directive in vkDestroyDescriptorSetLayout.txt -
-- include::{generated}\/validity\/protos\/vkDestroyDescriptorSetLayout.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyDescriptorSetLayout" vkDestroyDescriptorSetLayout :: ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyDescriptorSetLayout :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyDescriptorSetLayout deviceCmds = mkVkDestroyDescriptorSetLayout (pVkDestroyDescriptorSetLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorSetLayout
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyDescriptorSetLayout = ("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDescriptorSetLayout = FunPtr FN_vkDestroyDescriptorSetLayout

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
-- -   @pDescriptorSets@ is an array of handles to 'VkDescriptorSet'
--     objects.
--
-- = Description
--
-- After a successful call to 'vkFreeDescriptorSets', all descriptor sets
-- in @pDescriptorSets@ are invalid.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to any element of
--     @pDescriptorSets@ /must/ have completed execution
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ 'VkDescriptorSet' handles, each element of
--     which /must/ either be a valid handle or
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   Each valid handle in @pDescriptorSets@ /must/ have been allocated
--     from @descriptorPool@
--
-- -   @descriptorPool@ /must/ have been created with the
--     'VK_DESCRIPTOR_POOL_CREATE_FREE_DESCRIPTOR_SET_BIT' flag
--
-- Unresolved directive in vkFreeDescriptorSets.txt -
-- include::{generated}\/validity\/protos\/vkFreeDescriptorSets.txt[]
--
-- = See Also
--
-- 'VkDescriptorPool', 'VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkFreeDescriptorSets" vkFreeDescriptorSets :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
#else
vkFreeDescriptorSets :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
vkFreeDescriptorSets deviceCmds = mkVkFreeDescriptorSets (pVkFreeDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
#endif

type FN_vkFreeDescriptorSets = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult
type PFN_vkFreeDescriptorSets = FunPtr FN_vkFreeDescriptorSets

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
-- Unresolved directive in vkResetDescriptorPool.txt -
-- include::{generated}\/validity\/protos\/vkResetDescriptorPool.txt[]
--
-- = See Also
--
-- 'VkDescriptorPool', 'VkDescriptorPoolResetFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetDescriptorPool" vkResetDescriptorPool :: ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
#else
vkResetDescriptorPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
vkResetDescriptorPool deviceCmds = mkVkResetDescriptorPool (pVkResetDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult)
#endif

type FN_vkResetDescriptorPool = ("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult
type PFN_vkResetDescriptorPool = FunPtr FN_vkResetDescriptorPool

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
--     'VkWriteDescriptorSet' structures describing the descriptor sets to
--     write to.
--
-- -   @descriptorCopyCount@ is the number of elements in the
--     @pDescriptorCopies@ array.
--
-- -   @pDescriptorCopies@ is a pointer to an array of
--     'VkCopyDescriptorSet' structures describing the descriptor sets to
--     copy between.
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
-- Each element in the @pDescriptorCopies@ array is a 'VkCopyDescriptorSet'
-- structure describing an operation copying descriptors between sets.
--
-- If the @dstSet@ member of any element of @pDescriptorWrites@ or
-- @pDescriptorCopies@ is bound, accessed, or modified by any command that
-- was recorded to a command buffer which is currently in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle recording or executable state>,
-- and any of the descriptor bindings that are updated were not created
-- with the
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
-- or
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
-- bits set, that command buffer becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   Descriptor bindings updated by this command which were created
--     without the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing.VK_DESCRIPTOR_BINDING_UPDATE_UNUSED_WHILE_PENDING_BIT_EXT'
--     bits set /must/ not be used by any command that was recorded to a
--     command buffer which is in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- Unresolved directive in vkUpdateDescriptorSets.txt -
-- include::{generated}\/validity\/protos\/vkUpdateDescriptorSets.txt[]
--
-- = See Also
--
-- 'VkCopyDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'VkWriteDescriptorSet'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkUpdateDescriptorSets" vkUpdateDescriptorSets :: ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
#else
vkUpdateDescriptorSets :: DeviceCmds -> ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
vkUpdateDescriptorSets deviceCmds = mkVkUpdateDescriptorSets (pVkUpdateDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ())
#endif

type FN_vkUpdateDescriptorSets = ("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()
type PFN_vkUpdateDescriptorSets = FunPtr FN_vkUpdateDescriptorSets
