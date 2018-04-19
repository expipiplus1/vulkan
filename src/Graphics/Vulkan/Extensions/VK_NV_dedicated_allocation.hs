{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  ) where

import Data.String
  ( IsString
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
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV = VkStructureType 1000026000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV = VkStructureType 1000026001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000026002
-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_NV_dedicated_allocation"
-- | VkDedicatedAllocationImageCreateInfoNV - Specify that an image is bound
-- to a dedicated memory resource
--
-- = Description
-- #_description#
--
-- __Note__
--
-- Using a dedicated allocation for color and depth\/stencil attachments or
-- other large images /may/ improve performance on some devices.
--
-- == Valid Usage
--
-- -   If @dedicatedAllocation@ is @VK_TRUE@, @VkImageCreateInfo@::@flags@
--     /must/ not include @VK_IMAGE_CREATE_SPARSE_BINDING_BIT@,
--     @VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT@, or
--     @VK_IMAGE_CREATE_SPARSE_ALIASED_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV@
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "vkDedicatedAllocation"
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
-- | VkDedicatedAllocationBufferCreateInfoNV - Specify that a buffer is bound
-- to a dedicated memory resource
--
-- = Description
-- #_description#
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV@
--
-- = See Also
-- #_see_also#
--
-- @VkBool32@, 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "vkDedicatedAllocation"
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
-- | VkDedicatedAllocationMemoryAllocateInfoNV - Specify a dedicated memory
-- allocation resource
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   At least one of @image@ and @buffer@ /must/ be
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     the image /must/ have been created with
--     @VkDedicatedAllocationImageCreateInfoNV@::@dedicatedAllocation@
--     equal to @VK_TRUE@
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', the buffer /must/
--     have been created with
--     @VkDedicatedAllocationBufferCreateInfoNV@::@dedicatedAllocation@
--     equal to @VK_TRUE@
--
-- -   If @image@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @VkMemoryAllocateInfo@::@allocationSize@ /must/ equal the
--     @VkMemoryRequirements@::@size@ of the image
--
-- -   If @buffer@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @VkMemoryAllocateInfo@::@allocationSize@ /must/ equal the
--     @VkMemoryRequirements@::@size@ of the buffer
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     @VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV@
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType'
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "vkImage"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "vkBuffer"
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
