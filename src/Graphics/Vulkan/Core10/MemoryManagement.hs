{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  , vkGetBufferMemoryRequirements
  , vkBindBufferMemory
  , vkGetImageMemoryRequirements
  , vkBindImageMemory
  , VkMemoryRequirements(..)
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
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )


-- | Dummy data to tag the 'Ptr' with
data VkBuffer_T
-- | VkBuffer - Opaque handle to a buffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableVertexBufferEntryNVX',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseBufferMemoryBindInfo',
-- 'vkBindBufferMemory',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.Core10.Buffer.vkDestroyBuffer',
-- 'vkGetBufferMemoryRequirements'
type VkBuffer = Ptr VkBuffer_T
-- | Dummy data to tag the 'Ptr' with
data VkImage_T
-- | VkImage - Opaque handle to a image object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBindInfo',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkSparseImageOpaqueMemoryBindInfo',
-- 'vkBindImageMemory',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResolveImage',
-- 'Graphics.Vulkan.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.Core10.Image.vkDestroyImage',
-- 'vkGetImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.Core10.Image.vkGetImageSubresourceLayout',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.vkGetSwapchainImagesKHR'
type VkImage = Ptr VkImage_T
-- | vkGetBufferMemoryRequirements - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer.
--
-- -   @buffer@ is the buffer to query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements' structure in which the memory requirements of
--     the buffer object are returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @pMemoryRequirements@ /must/ be a valid pointer to a
--     @VkMemoryRequirements@ structure
--
-- -   @buffer@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'VkBuffer', 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMemoryRequirements'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
-- | vkBindBufferMemory - Bind device memory to a buffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer and memory.
--
-- -   @buffer@ is the buffer to be attached to memory.
--
-- -   @memory@ is a @VkDeviceMemory@ object describing the device memory
--     to attach.
--
-- -   @memoryOffset@ is the start offset of the region of @memory@ which
--     is to be bound to the buffer. The number of bytes returned in the
--     @VkMemoryRequirements@::@size@ member in @memory@, starting from
--     @memoryOffset@ bytes, will be bound to the specified buffer.
--
-- = Description
--
-- @vkBindBufferMemory@ is equivalent to passing the same parameters
-- through
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo'
-- to
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2'.
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
--     equal to @buffer@
--
-- -   If the @VkMemoryAllocateInfo@ provided when @memory@ was allocated
--     included an instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     was not @VK_NULL_HANDLE@, then @buffer@ /must/ equal
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@,
--     and @memoryOffset@ /must/ be zero.
--
-- -   If @buffer@ was created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to @VK_TRUE@, @memory@ /must/ have been created with
--     'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@buffer@
--     equal to a buffer handle created with identical creation parameters
--     to @buffer@ and @memoryOffset@ /must/ be zero
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @buffer@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @buffer@ /must/ be externally synchronized
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
-- 'VkBuffer', 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindBufferMemory" vkBindBufferMemory :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
-- | vkGetImageMemoryRequirements - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the image to query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements' structure in which the memory requirements of
--     the image object are returned.
--
-- == Valid Usage
--
-- -   @image@ /must/ not have been created with the
--     @VK_IMAGE_CREATE_DISJOINT_BIT@ flag set
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @pMemoryRequirements@ /must/ be a valid pointer to a
--     @VkMemoryRequirements@ structure
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkImage',
-- 'VkMemoryRequirements'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
-- | vkBindImageMemory - Bind device memory to an image object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image and memory.
--
-- -   @image@ is the image.
--
-- -   @memory@ is the @VkDeviceMemory@ object describing the device memory
--     to attach.
--
-- -   @memoryOffset@ is the start offset of the region of @memory@ which
--     is to be bound to the image. The number of bytes returned in the
--     @VkMemoryRequirements@::@size@ member in @memory@, starting from
--     @memoryOffset@ bytes, will be bound to the specified image.
--
-- = Description
--
-- @vkBindImageMemory@ is equivalent to passing the same parameters through
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo'
-- to
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'.
--
-- == Valid Usage
--
-- -   @image@ /must/ not have been created with the
--     @VK_IMAGE_CREATE_DISJOINT_BIT@ set.
--
-- -   @image@ /must/ not already be backed by a memory object
--
-- -   @image@ /must/ not have been created with any sparse memory binding
--     flags
--
-- -   @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the @VkMemoryRequirements@
--     structure returned from a call to @vkGetImageMemoryRequirements@
--     with @image@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the @VkMemoryRequirements@ structure returned from a call
--     to @vkGetImageMemoryRequirements@ with @image@
--
-- -   The @size@ member of the @VkMemoryRequirements@ structure returned
--     from a call to @vkGetImageMemoryRequirements@ with @image@ /must/ be
--     less than or equal to the size of @memory@ minus @memoryOffset@
--
-- -   If @image@ requires a dedicated allocation (as reported by
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     in
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @image@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     equal to @image@
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
--     equal to an image handle created with identical creation parameters
--     to @image@ and @memoryOffset@ /must/ be zero
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @memory@ /must/ be a valid @VkDeviceMemory@ handle
--
-- -   @image@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- -   @memory@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @image@ /must/ be externally synchronized
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory', @VkDeviceSize@,
-- 'VkImage'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindImageMemory" vkBindImageMemory :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
-- | VkMemoryRequirements - Structure specifying memory requirements
--
-- = See Also
--
-- @VkDeviceSize@,
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2',
-- 'vkGetBufferMemoryRequirements', 'vkGetImageMemoryRequirements'
data VkMemoryRequirements = VkMemoryRequirements
  { -- | @size@ is the size, in bytes, of the memory allocation /required/ for
  -- the resource.
  vkSize :: VkDeviceSize
  , -- | @alignment@ is the alignment, in bytes, of the offset within the
  -- allocation /required/ for the resource.
  vkAlignment :: VkDeviceSize
  , -- | @memoryTypeBits@ is a bitmask and contains one bit set for every
  -- supported memory type for the resource. Bit @i@ is set if and only if
  -- the memory type @i@ in the @VkPhysicalDeviceMemoryProperties@ structure
  -- for the physical device is supported for the resource.
  vkMemoryTypeBits :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryRequirements <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 8) (vkAlignment (poked :: VkMemoryRequirements))
                *> poke (ptr `plusPtr` 16) (vkMemoryTypeBits (poked :: VkMemoryRequirements))
