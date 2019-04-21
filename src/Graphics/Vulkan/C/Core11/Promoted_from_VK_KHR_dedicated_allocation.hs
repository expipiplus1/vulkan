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
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     and the external handle was created by the Vulkan API, then the
--     memory being imported /must/ also be a dedicated image allocation
--     and @image@ must be identical to the image associated with the
--     imported memory.
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_WIN32_KMT_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D11_TEXTURE_KMT_BIT',
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_HEAP_BIT',
--     or
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_D3D12_RESOURCE_BIT',
--     and the external handle was created by the Vulkan API, then the
--     memory being imported /must/ also be a dedicated buffer allocation
--     and @buffer@ must be identical to the buffer associated with the
--     imported memory.
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the memory being imported /must/ also be a dedicated image
--     allocation and @image@ must be identical to the image associated
--     with the imported memory.
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation with handle type
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VK_EXTERNAL_MEMORY_HANDLE_TYPE_OPAQUE_FD_BIT',
--     the memory being imported /must/ also be a dedicated buffer
--     allocation and @buffer@ must be identical to the buffer associated
--     with the imported memory.
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @image@ /must/
--     not have been created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     set in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@
--
-- Unresolved directive in VkMemoryDedicatedAllocateInfo.txt -
-- include::{generated}\/validity\/structs\/VkMemoryDedicatedAllocateInfo.txt[]
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
-- -   The @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' for the call to
--     'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer' used to create the
--     buffer being queried contained an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
--     and any of the handle types specified in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo'::@handleTypes@
--     requires dedicated allocation, as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties'::@externalMemoryProperties@::@externalMemoryFeatures@,
--     the @requiresDedicatedAllocation@ field will be set to
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
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
-- -   The @pNext@ chain of
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' for the call to
--     'Graphics.Vulkan.C.Core10.Image.vkCreateImage' used to create the
--     image being queried contained an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
--     and any of the handle types specified in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo'::@handleTypes@
--     requires dedicated allocation, as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties'::@externalMemoryProperties@::@externalMemoryFeatures@,
--     the @requiresDedicatedAllocation@ field will be set to
--     'Graphics.Vulkan.C.Core10.Core.VK_TRUE'.
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
-- Unresolved directive in VkMemoryDedicatedRequirements.txt -
-- include::{generated}\/validity\/structs\/VkMemoryDedicatedRequirements.txt[]
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
