{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , VkMemoryDedicatedRequirements(..)
  , VkMemoryDedicatedAllocateInfo(..)
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
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS = VkStructureType 1000127000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO = VkStructureType 1000127001
-- | VkMemoryDedicatedRequirements - Structure describing dedicated
-- allocation requirements of buffer and image resources
--
-- = Description
--
-- If the @VkMemoryDedicatedRequirements@ structure is included in the
-- @pNext@ chain of the
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed as the @pMemoryRequirements@ parameter of a
-- @vkGetBufferMemoryRequirements2@ call, @requiresDedicatedAllocation@
-- /may/ be @VK_TRUE@ under one of the following conditions:
--
-- -   The @pNext@ chain of @VkBufferCreateInfo@ for the call to
--     @vkCreateBuffer@ used to create the buffer being queried contained
--     an instance of @VkExternalMemoryBufferCreateInfo@, and any of the
--     handle types specified in
--     @VkExternalMemoryBufferCreateInfo@::@handleTypes@ requires dedicated
--     allocation, as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties'
--     in
--     @VkExternalBufferProperties@::@externalMemoryProperties@::@externalMemoryFeatures@,
--     the @requiresDedicatedAllocation@ field will be set to @VK_TRUE@.
--
-- In all other cases, @requiresDedicatedAllocation@ /must/ be set to
-- @VK_FALSE@ by the implementation whenever a
-- @VkMemoryDedicatedRequirements@ structure is included in the @pNext@
-- chain of the @VkMemoryRequirements2@ structure passed to a call to
-- @vkGetBufferMemoryRequirements2@.
--
-- If the @VkMemoryDedicatedRequirements@ structure is included in the
-- @pNext@ chain of the @VkMemoryRequirements2@ structure passed as the
-- @pMemoryRequirements@ parameter of a @vkGetBufferMemoryRequirements2@
-- call and @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@ was set in
-- @VkBufferCreateInfo@::@flags@ when @buffer@ was created then the
-- implementation /must/ set both @prefersDedicatedAllocation@ and
-- @requiresDedicatedAllocation@ to @VK_FALSE@.
--
-- If the @VkMemoryDedicatedRequirements@ structure is included in the
-- @pNext@ chain of the @VkMemoryRequirements2@ structure passed as the
-- @pMemoryRequirements@ parameter of a @vkGetImageMemoryRequirements2@
-- call, @requiresDedicatedAllocation@ /may/ be @VK_TRUE@ under one of the
-- following conditions:
--
-- -   The @pNext@ chain of @VkImageCreateInfo@ for the call to
--     @vkCreateImage@ used to create the image being queried contained an
--     instance of @VkExternalMemoryImageCreateInfo@, and any of the handle
--     types specified in @VkExternalMemoryImageCreateInfo@::@handleTypes@
--     requires dedicated allocation, as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--     in
--     @VkExternalImageFormatProperties@::@externalMemoryProperties@::@externalMemoryFeatures@,
--     the @requiresDedicatedAllocation@ field will be set to @VK_TRUE@.
--
-- In all other cases, @requiresDedicatedAllocation@ /must/ be set to
-- @VK_FALSE@ by the implementation whenever a
-- @VkMemoryDedicatedRequirements@ structure is included in the @pNext@
-- chain of the @VkMemoryRequirements2@ structure passed to a call to
-- @vkGetImageMemoryRequirements2@.
--
-- If the @VkMemoryDedicatedRequirements@ structure is included in the
-- @pNext@ chain of the @VkMemoryRequirements2@ structure passed as the
-- @pMemoryRequirements@ parameter of a @vkGetImageMemoryRequirements2@
-- call and @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@ was set in
-- @VkImageCreateInfo@::@flags@ when @image@ was created then the
-- implementation /must/ set both @prefersDedicatedAllocation@ and
-- @requiresDedicatedAllocation@ to @VK_FALSE@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS@
--
-- = See Also
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
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
-- | VkMemoryDedicatedAllocateInfo - Specify a dedicated memory allocation
-- resource
--
-- == Valid Usage
--
-- -   At least one of @image@ and @buffer@ /must/ be
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @VkMemoryAllocateInfo@::@allocationSize@ /must/ equal the
--     @VkMemoryRequirements@::@size@ of the image
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @image@ /must/ have been created without
--     @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@ set in
--     @VkImageCreateInfo@::@flags@
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @VkMemoryAllocateInfo@::@allocationSize@ /must/ equal the
--     @VkMemoryRequirements@::@size@ of the buffer
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @buffer@ /must/
--     have been created without @VK_BUFFER_CREATE_SPARSE_BINDING_BIT@ set
--     in 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateInfo'::@flags@
--
-- -   If @image@ is not @VK_NULL_HANDLE@ and
--     'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@, or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@, and the
--     external handle was created by the Vulkan API, then the memory being
--     imported /must/ also be a dedicated image allocation and @image@
--     must be identical to the image associated with the imported memory.
--
-- -   If @buffer@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--     and 'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT@,
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT@, or
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT@, and the
--     external handle was created by the Vulkan API, then the memory being
--     imported /must/ also be a dedicated buffer allocation and @buffer@
--     must be identical to the buffer associated with the imported memory.
--
-- -   If @image@ is not @VK_NULL_HANDLE@ and
--     'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT@, the memory being
--     imported /must/ also be a dedicated image allocation and @image@
--     must be identical to the image associated with the imported memory.
--
-- -   If @buffer@ is not @VK_NULL_HANDLE@ and
--     'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     @VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT@, the memory being
--     imported /must/ also be a dedicated buffer allocation and @buffer@
--     must be identical to the buffer associated with the imported memory.
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @image@ /must/ not have been created with
--     @VK_IMAGE_CREATE_DISJOINT_BIT@ set in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo'::@flags@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO@
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @image@ /must/ be a valid @VkImage@ handle
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @buffer@ /must/
--     be a valid @VkBuffer@ handle
--
-- -   Both of @buffer@, and @image@ that are valid handles /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkMemoryDedicatedAllocateInfo = VkMemoryDedicatedAllocateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @image@ is 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE' or a handle
  -- of an image which this memory will be bound to.
  vkImage :: VkImage
  , -- | @buffer@ is 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE' or a
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
