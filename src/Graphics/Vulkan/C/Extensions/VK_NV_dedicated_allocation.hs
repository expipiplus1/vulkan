{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  ) where

import Data.String
  ( IsString
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
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- | VkDedicatedAllocationBufferCreateInfoNV - Specify that a buffer is bound
-- to a dedicated memory resource
--
-- = Description
--
-- Unresolved directive in VkDedicatedAllocationBufferCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkDedicatedAllocationBufferCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @dedicatedAllocation@ specifies whether the buffer will have a dedicated
  -- allocation bound to it.
  vkDedicatedAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDedicatedAllocationBufferCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDedicatedAllocationBufferCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDedicatedAllocationBufferCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDedicatedAllocationBufferCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkDedicatedAllocation (poked :: VkDedicatedAllocationBufferCreateInfoNV))

instance Zero VkDedicatedAllocationBufferCreateInfoNV where
  zero = VkDedicatedAllocationBufferCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
                                                 zero
                                                 zero

-- | VkDedicatedAllocationImageCreateInfoNV - Specify that an image is bound
-- to a dedicated memory resource
--
-- = Description
--
-- __Note__
--
-- Using a dedicated allocation for color and depth\/stencil attachments or
-- other large images /may/ improve performance on some devices.
--
-- == Valid Usage
--
-- -   If @dedicatedAllocation@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE',
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@ /must/
--     not include
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT',
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT'
--
-- Unresolved directive in VkDedicatedAllocationImageCreateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkDedicatedAllocationImageCreateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @dedicatedAllocation@ specifies whether the image will have a dedicated
  -- allocation bound to it.
  vkDedicatedAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDedicatedAllocationImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDedicatedAllocationImageCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDedicatedAllocationImageCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDedicatedAllocationImageCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkDedicatedAllocation (poked :: VkDedicatedAllocationImageCreateInfoNV))

instance Zero VkDedicatedAllocationImageCreateInfoNV where
  zero = VkDedicatedAllocationImageCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
                                                zero
                                                zero

-- | VkDedicatedAllocationMemoryAllocateInfoNV - Specify a dedicated memory
-- allocation resource
--
-- == Valid Usage
--
-- -   At least one of @image@ and @buffer@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the image
--     /must/ have been created with
--     'VkDedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', the buffer
--     /must/ have been created with
--     'VkDedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     of the image
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'::@allocationSize@
--     /must/ equal the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     of the buffer
--
-- -   If @image@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation, the memory being imported /must/ also be a
--     dedicated image allocation and @image@ /must/ be identical to the
--     image associated with the imported memory.
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE' and
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' defines a
--     memory import operation, the memory being imported /must/ also be a
--     dedicated buffer allocation and @buffer@ /must/ be identical to the
--     buffer associated with the imported memory.
--
-- Unresolved directive in VkDedicatedAllocationMemoryAllocateInfoNV.txt -
-- include::{generated}\/validity\/structs\/VkDedicatedAllocationMemoryAllocateInfoNV.txt[]
--
-- = See Also
--
-- No cross-references are available
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV
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

instance Storable VkDedicatedAllocationMemoryAllocateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDedicatedAllocationMemoryAllocateInfoNV <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))

instance Zero VkDedicatedAllocationMemoryAllocateInfoNV where
  zero = VkDedicatedAllocationMemoryAllocateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
                                                   zero
                                                   zero
                                                   zero

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_NV_dedicated_allocation"

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV = VkStructureType 1000026001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV = VkStructureType 1000026000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000026002
