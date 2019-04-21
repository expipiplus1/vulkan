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
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
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
-- Unresolved directive in VkMemoryDedicatedRequirements.txt -
-- include::{generated}\/validity\/structs\/VkMemoryDedicatedRequirements.txt[]
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

