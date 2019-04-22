{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  ) where

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
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- | VkMemoryDedicatedAllocateInfo - Specify a dedicated memory allocation
-- resource
--
-- == Valid Usage
--
-- -   At least one of @image@ and @buffer@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     of the image
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @image@ /must/
--     have been created without
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
--     set in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     of the buffer
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @buffer@ /must/
--     have been created without
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
--     set in 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO'
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @image@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
--     handle
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @buffer@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer'
--     handle
--
-- -   Both of @buffer@, and @image@ that are valid handles /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkMemoryDedicatedAllocateInfo = VkMemoryDedicatedAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @image@ is 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' or a
  -- handle of an image which this memory will be bound to.
  vkImage :: VkImage
  , -- | @buffer@ is 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' or a
  -- handle of a buffer which this memory will be bound to.
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkMemoryDedicatedAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryDedicatedAllocateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkMemoryDedicatedAllocateInfo))

instance Zero VkMemoryDedicatedAllocateInfo where
  zero = VkMemoryDedicatedAllocateInfo VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
                                       zero
                                       zero
                                       zero

-- | VkMemoryDedicatedRequirements - Structure describing dedicated
-- allocation requirements of buffer and image resources
--
-- = Description
--
-- When the implementation sets @requiresDedicatedAllocation@ to
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', it /must/ also set
-- @prefersDedicatedAllocation@ to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
--
-- If the 'VkMemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'
-- call, @requiresDedicatedAllocation@ /may/ be
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' under one of the following
-- conditions:
--
-- -   none
--
-- In all other cases, @requiresDedicatedAllocation@ /must/ be set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' by the implementation whenever
-- a 'VkMemoryDedicatedRequirements' structure is included in the @pNext@
-- chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed to a call to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'.
--
-- If the 'VkMemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'
-- call and
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
-- was set in 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo'::@flags@
-- when @buffer@ was created then the implementation /must/ set both
-- @prefersDedicatedAllocation@ and @requiresDedicatedAllocation@ to
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- If the 'VkMemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
-- call, @requiresDedicatedAllocation@ /may/ be
-- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' under one of the following
-- conditions:
--
-- -   none
--
-- In all other cases, @requiresDedicatedAllocation@ /must/ be set to
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' by the implementation whenever
-- a 'VkMemoryDedicatedRequirements' structure is included in the @pNext@
-- chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed to a call to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'.
--
-- If the 'VkMemoryDedicatedRequirements' structure is included in the
-- @pNext@ chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
-- call and
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
-- was set in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
-- when @image@ was created then the implementation /must/ set both
-- @prefersDedicatedAllocation@ and @requiresDedicatedAllocation@ to
-- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be 'VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkMemoryDedicatedRequirements = VkMemoryDedicatedRequirements
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @prefersDedicatedAllocation@ specifies that the implementation would
  -- prefer a dedicated allocation for this resource. The application is
  -- still free to suballocate the resource but it /may/ get better
  -- performance if a dedicated allocation is used.
  vkPrefersDedicatedAllocation :: VkBool32
  , -- | @requiresDedicatedAllocation@ specifies that a dedicated allocation is
  -- required for this resource.
  vkRequiresDedicatedAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkMemoryDedicatedRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryDedicatedRequirements <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 16) (vkPrefersDedicatedAllocation (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 20) (vkRequiresDedicatedAllocation (poked :: VkMemoryDedicatedRequirements))

instance Zero VkMemoryDedicatedRequirements where
  zero = VkMemoryDedicatedRequirements VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
                                       zero
                                       zero
                                       zero

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO = VkStructureType 1000127001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS = VkStructureType 1000127000
