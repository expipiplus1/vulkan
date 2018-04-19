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
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )


-- | Dummy data to tag the 'Ptr' with
data VkBuffer_T
-- | VkBuffer - Opaque handle to a buffer object
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
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
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
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
-- #_parameters#
--
-- -   @device@ is the logical device that owns the buffer.
--
-- -   @buffer@ is the buffer to query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements' structure in which the memory requirements of
--     the buffer object are returned.
--
-- = Description
-- #_description#
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
-- #_see_also#
--
-- 'VkBuffer', 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'VkMemoryRequirements'
foreign import ccall "vkGetBufferMemoryRequirements" vkGetBufferMemoryRequirements :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
-- | vkBindBufferMemory - Bind device memory to a buffer object
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'VkBuffer', 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory', @VkDeviceSize@
foreign import ccall "vkBindBufferMemory" vkBindBufferMemory :: ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
-- | vkGetImageMemoryRequirements - Returns the memory requirements for
-- specified Vulkan object
--
-- = Parameters
-- #_parameters#
--
-- -   @device@ is the logical device that owns the image.
--
-- -   @image@ is the image to query.
--
-- -   @pMemoryRequirements@ points to an instance of the
--     'VkMemoryRequirements' structure in which the memory requirements of
--     the image object are returned.
--
-- = Description
-- #_description#
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkImage',
-- 'VkMemoryRequirements'
foreign import ccall "vkGetImageMemoryRequirements" vkGetImageMemoryRequirements :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()
-- | vkBindImageMemory - Bind device memory to an image object
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- [<#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory', @VkDeviceSize@,
-- 'VkImage'
foreign import ccall "vkBindImageMemory" vkBindImageMemory :: ("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult
-- | VkMemoryRequirements - Structure specifying memory requirements
--
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- @VkDeviceSize@,
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2',
-- 'vkGetBufferMemoryRequirements', 'vkGetImageMemoryRequirements'
data VkMemoryRequirements = VkMemoryRequirements
  { -- No documentation found for Nested "VkMemoryRequirements" "vkSize"
  vkSize :: VkDeviceSize
  , -- No documentation found for Nested "VkMemoryRequirements" "vkAlignment"
  vkAlignment :: VkDeviceSize
  , -- No documentation found for Nested "VkMemoryRequirements" "vkMemoryTypeBits"
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
