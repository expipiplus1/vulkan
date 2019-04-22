{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( withCStructMemoryDedicatedAllocateInfo
  , fromCStructMemoryDedicatedAllocateInfo
  , MemoryDedicatedAllocateInfo(..)
  , withCStructMemoryDedicatedRequirements
  , fromCStructMemoryDedicatedRequirements
  , MemoryDedicatedRequirements(..)
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  ) where

import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )
import Graphics.Vulkan.Core10.Core
  ( bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
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
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO'
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
data MemoryDedicatedAllocateInfo = MemoryDedicatedAllocateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "image"
  image :: Image
  , -- No documentation found for Nested "MemoryDedicatedAllocateInfo" "buffer"
  buffer :: Buffer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryDedicatedAllocateInfo' and
-- marshal a 'MemoryDedicatedAllocateInfo' into it. The 'VkMemoryDedicatedAllocateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryDedicatedAllocateInfo :: MemoryDedicatedAllocateInfo -> (VkMemoryDedicatedAllocateInfo -> IO a) -> IO a
withCStructMemoryDedicatedAllocateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryDedicatedAllocateInfo)) (\pPNext -> cont (VkMemoryDedicatedAllocateInfo VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO pPNext (image (marshalled :: MemoryDedicatedAllocateInfo)) (buffer (marshalled :: MemoryDedicatedAllocateInfo))))

-- | A function to read a 'VkMemoryDedicatedAllocateInfo' and all additional
-- structures in the pointer chain into a 'MemoryDedicatedAllocateInfo'.
fromCStructMemoryDedicatedAllocateInfo :: VkMemoryDedicatedAllocateInfo -> IO MemoryDedicatedAllocateInfo
fromCStructMemoryDedicatedAllocateInfo c = MemoryDedicatedAllocateInfo <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryDedicatedAllocateInfo)))
                                                                       <*> pure (vkImage (c :: VkMemoryDedicatedAllocateInfo))
                                                                       <*> pure (vkBuffer (c :: VkMemoryDedicatedAllocateInfo))

instance Zero MemoryDedicatedAllocateInfo where
  zero = MemoryDedicatedAllocateInfo Nothing
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
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- structure is included in the @pNext@ chain of the
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
-- a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- structure is included in the @pNext@ chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed to a call to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'.
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- structure is included in the @pNext@ chain of the
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
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- structure is included in the @pNext@ chain of the
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
-- a
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- structure is included in the @pNext@ chain of the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2'
-- structure passed to a call to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'.
--
-- If the
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'
-- structure is included in the @pNext@ chain of the
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
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data MemoryDedicatedRequirements = MemoryDedicatedRequirements
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryDedicatedRequirements" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryDedicatedRequirements" "prefersDedicatedAllocation"
  prefersDedicatedAllocation :: Bool
  , -- No documentation found for Nested "MemoryDedicatedRequirements" "requiresDedicatedAllocation"
  requiresDedicatedAllocation :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryDedicatedRequirements' and
-- marshal a 'MemoryDedicatedRequirements' into it. The 'VkMemoryDedicatedRequirements' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryDedicatedRequirements :: MemoryDedicatedRequirements -> (VkMemoryDedicatedRequirements -> IO a) -> IO a
withCStructMemoryDedicatedRequirements marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryDedicatedRequirements)) (\pPNext -> cont (VkMemoryDedicatedRequirements VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS pPNext (boolToBool32 (prefersDedicatedAllocation (marshalled :: MemoryDedicatedRequirements))) (boolToBool32 (requiresDedicatedAllocation (marshalled :: MemoryDedicatedRequirements)))))

-- | A function to read a 'VkMemoryDedicatedRequirements' and all additional
-- structures in the pointer chain into a 'MemoryDedicatedRequirements'.
fromCStructMemoryDedicatedRequirements :: VkMemoryDedicatedRequirements -> IO MemoryDedicatedRequirements
fromCStructMemoryDedicatedRequirements c = MemoryDedicatedRequirements <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryDedicatedRequirements)))
                                                                       <*> pure (bool32ToBool (vkPrefersDedicatedAllocation (c :: VkMemoryDedicatedRequirements)))
                                                                       <*> pure (bool32ToBool (vkRequiresDedicatedAllocation (c :: VkMemoryDedicatedRequirements)))

instance Zero MemoryDedicatedRequirements where
  zero = MemoryDedicatedRequirements Nothing
                                     False
                                     False

