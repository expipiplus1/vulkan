{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  , vkBindBufferMemory2
  , vkBindImageMemory2
  , VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO = VkStructureType 1000157000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO = VkStructureType 1000157001
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_ALIAS_BIT"
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateFlagBits 0x00000400
-- | vkBindBufferMemory2 - Bind device memory to buffer objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffers and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'VkBindBufferMemoryInfo', describing buffers and memory to bind.
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pBindInfos@ /must/ be a valid pointer to an array of
--     @bindInfoCount@ valid @VkBindBufferMemoryInfo@ structures
--
-- -   @bindInfoCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindBufferMemory2" vkBindBufferMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
-- | vkBindImageMemory2 - Bind device memory to image objects
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the images and memory.
--
-- -   @bindInfoCount@ is the number of elements in @pBindInfos@.
--
-- -   @pBindInfos@ is a pointer to an array of structures of type
--     'VkBindImageMemoryInfo', describing images and memory to bind.
--
-- = Description
--
-- On some implementations, it /may/ be more efficient to batch memory
-- bindings into a single command.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pBindInfos@ /must/ be a valid pointer to an array of
--     @bindInfoCount@ valid @VkBindImageMemoryInfo@ structures
--
-- -   @bindInfoCount@ /must/ be greater than @0@
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindImageMemory2" vkBindImageMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
-- | VkBindBufferMemoryInfo - Structure specifying how to bind a buffer to
-- memory
--
-- == Valid Usage
--
-- -   @buffer@ /must/ not already be backed by a memory object
--
-- -   @buffer@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   If @buffer@ was created with the
--     @VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT@ or
--     @VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT@, @memoryOffset@ /must/ be
--     a multiple of
--     @VkPhysicalDeviceLimits@::@minTexelBufferOffsetAlignment@
--
-- -   If @buffer@ was created with the
--     @VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT@, @memoryOffset@ /must/ be a
--     multiple of
--     @VkPhysicalDeviceLimits@::@minUniformBufferOffsetAlignment@
--
-- -   If @buffer@ was created with the
--     @VK_BUFFER_USAGE_STORAGE_BUFFER_BIT@, @memoryOffset@ /must/ be a
--     multiple of
--     @VkPhysicalDeviceLimits@::@minStorageBufferOffsetAlignment@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the @VkMemoryRequirements@
--     structure returned from a call to @vkGetBufferMemoryRequirements@
--     with @buffer@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the @VkMemoryRequirements@ structure returned from a call
--     to @vkGetBufferMemoryRequirements@ with @buffer@
--
-- -   The @size@ member of the @VkMemoryRequirements@ structure returned
--     from a call to @vkGetBufferMemoryRequirements@ with @buffer@ /must/
--     be less than or equal to the size of @memory@ minus @memoryOffset@
--
-- -   If @buffer@ requires a dedicated allocation(as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'
--     in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @buffer@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     equal to @buffer@ and @memoryOffset@ /must/ be zero
--
-- -   If the @VkMemoryAllocateInfo@ provided when @memory@ was allocated
--     included an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     was not @VK_NULL_HANDLE@, then @buffer@ /must/ equal
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     and @memoryOffset@ /must/ be zero.
--
-- -   If @buffer@ was created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to @VK_TRUE@, @memory@ /must/ have been created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@buffer@
--     equal to @buffer@ and @memoryOffset@ /must/ be zero
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo',
--     all instances of @memory@ specified by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo'::@pDeviceIndices@
--     /must/ have been allocated
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo'
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   Both of @buffer@, and @memory@ /must/ have been created, allocated,
--     or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkBindBufferMemory2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_bind_memory2.vkBindBufferMemory2KHR'
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @buffer@ is the buffer to be attached to memory.
  vkBuffer :: VkBuffer
  , -- | @memory@ is a @VkDeviceMemory@ object describing the device memory to
  -- attach.
  vkMemory :: VkDeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
  -- be bound to the buffer. The number of bytes returned in the
  -- @VkMemoryRequirements@::@size@ member in @memory@, starting from
  -- @memoryOffset@ bytes, will be bound to the specified buffer.
  vkMemoryOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBindBufferMemoryInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindBufferMemoryInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindBufferMemoryInfo))
-- | VkBindImageMemoryInfo - Structure specifying how to bind an image to
-- memory
--
-- == Valid Usage
--
-- -   @image@ /must/ not already be backed by a memory object
--
-- -   @image@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   If the @pNext@ chain does not include an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memory@ /must/ have been allocated using one of the
--     memory types allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@
--
-- -   If the @pNext@ chain does not include an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memoryOffset@ /must/ be an integer multiple of the
--     @alignment@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@
--
-- -   If the @pNext@ chain does not include an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, the difference of the size of @memory@ and @memoryOffset@
--     /must/ be greater than or equal to the @size@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with the same @image@
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @image@ /must/ have been created with the
--     @VK_IMAGE_CREATE_DISJOINT_BIT@ bit set.
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memory@ /must/ have been allocated using one of the
--     memory types allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@ and the correct @planeAspect@ for this plane in the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     structure attached to the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'’s
--     @pNext@ chain
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, @memoryOffset@ /must/ be an integer multiple of the
--     @alignment@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with @image@ and the correct @planeAspect@ for this plane in the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     structure attached to the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'’s
--     @pNext@ chain
--
-- -   If the @pNext@ chain includes an instance of the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--     structure, the difference of the size of @memory@ and @memoryOffset@
--     /must/ be greater than or equal to the @size@ member of the
--     'Graphics.Vulkan.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     with the same @image@ and the correct @planeAspect@ for this plane
--     in the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo'
--     structure attached to the
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2'’s
--     @pNext@ chain
--
-- -   If @image@ requires a dedicated allocation (as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @image@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     equal to @image@ and @memoryOffset@ /must/ be zero
--
-- -   If the @VkMemoryAllocateInfo@ provided when @memory@ was allocated
--     included an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     was not @VK_NULL_HANDLE@, then @image@ /must/ equal
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     and @memoryOffset@ /must/ be zero.
--
-- -   If @image@ was created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to @VK_TRUE@, @memory@ /must/ have been created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@image@
--     equal to @image@ and @memoryOffset@ /must/ be zero
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     all instances of @memory@ specified by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@pDeviceIndices@
--     /must/ have been allocated
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     and
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@splitInstanceBindRegionCount@
--     is not zero, then @image@ /must/ have been created with the
--     @VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT@ bit set
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     all elements of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@pSplitInstanceBindRegions@
--     /must/ be valid rectangles contained within the dimensions of
--     @image@
--
-- -   If the @pNext@ chain includes
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     the union of the areas of all elements of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo'::@pSplitInstanceBindRegions@
--     that correspond to the same instance of @image@ /must/ cover the
--     entire image.
--
-- -   If @image@ was created with a valid swapchain handle in
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkImageSwapchainCreateInfoKHR'::@swapchain@,
--     then the @pNext@ chain /must/ include a valid instance of
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR'
--
-- -   If the @pNext@ chain includes an instance of
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
--     @memory@ /must/ be 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE'
--
-- -   If the @pNext@ chain does not include an instance of
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
--     @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
--     or
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   Both of @image@, and @memory@ that are valid handles /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkBindImageMemory2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_bind_memory2.vkBindImageMemory2KHR'
data VkBindImageMemoryInfo = VkBindImageMemoryInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @image@ is the image to be attached to memory.
  vkImage :: VkImage
  , -- | @memory@ is a @VkDeviceMemory@ object describing the device memory to
  -- attach.
  vkMemory :: VkDeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of @memory@ which is to
  -- be bound to the image. The number of bytes returned in the
  -- @VkMemoryRequirements@::@size@ member in @memory@, starting from
  -- @memoryOffset@ bytes, will be bound to the specified image.
  vkMemoryOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBindImageMemoryInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindImageMemoryInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindImageMemoryInfo))
