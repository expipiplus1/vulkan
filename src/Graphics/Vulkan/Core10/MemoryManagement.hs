{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  , withCStructMemoryRequirements
  , fromCStructMemoryRequirements
  , MemoryRequirements(..)
  , bindBufferMemory
  , bindImageMemory
  , getBufferMemoryRequirements
  , getImageMemoryRequirements
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  , vkBindBufferMemory
  , vkBindImageMemory
  , vkGetBufferMemoryRequirements
  , vkGetImageMemoryRequirements
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  , DeviceSize
  )
import Graphics.Vulkan.Core10.Memory
  ( DeviceMemory
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )


-- | VkBuffer - Opaque handle to a buffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressInfoEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingBeginInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryAABBNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryTrianglesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableVertexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseBufferMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdTraceRaysNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
type Buffer = VkBuffer

-- | VkImage - Opaque handle to an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageOpaqueMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage',
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.C.Core10.Image.vkDestroyImage',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.vkGetImageDrmFormatModifierPropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetSwapchainImagesKHR'
type Image = VkImage


-- | VkMemoryRequirements - Structure specifying memory requirements
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
data MemoryRequirements = MemoryRequirements
  { -- No documentation found for Nested "MemoryRequirements" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "MemoryRequirements" "alignment"
  alignment :: DeviceSize
  , -- No documentation found for Nested "MemoryRequirements" "memoryTypeBits"
  memoryTypeBits :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryRequirements' and
-- marshal a 'MemoryRequirements' into it. The 'VkMemoryRequirements' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryRequirements :: MemoryRequirements -> (VkMemoryRequirements -> IO a) -> IO a
withCStructMemoryRequirements marshalled cont = cont (VkMemoryRequirements (size (marshalled :: MemoryRequirements)) (alignment (marshalled :: MemoryRequirements)) (memoryTypeBits (marshalled :: MemoryRequirements)))

-- | A function to read a 'VkMemoryRequirements' and all additional
-- structures in the pointer chain into a 'MemoryRequirements'.
fromCStructMemoryRequirements :: VkMemoryRequirements -> IO MemoryRequirements
fromCStructMemoryRequirements c = MemoryRequirements <$> pure (vkSize (c :: VkMemoryRequirements))
                                                     <*> pure (vkAlignment (c :: VkMemoryRequirements))
                                                     <*> pure (vkMemoryTypeBits (c :: VkMemoryRequirements))

instance Zero MemoryRequirements where
  zero = MemoryRequirements zero
                            zero
                            zero



-- | vkBindBufferMemory - Bind device memory to a buffer object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the buffer and memory.
--
-- -   @buffer@ is the buffer to be attached to memory.
--
-- -   @memory@ is a 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
--     object describing the device memory to attach.
--
-- -   @memoryOffset@ is the start offset of the region of @memory@ which
--     is to be bound to the buffer. The number of bytes returned in the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     member in @memory@, starting from @memoryOffset@ bytes, will be
--     bound to the specified buffer.
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
-- -   @memory@ /must/ have been allocated using one of the memory types
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
--     with @buffer@ /must/ be less than or equal to the size of @memory@
--     minus @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
bindBufferMemory :: Device ->  Buffer ->  DeviceMemory ->  DeviceSize ->  IO ()
bindBufferMemory = \(Device device' commandTable) -> \buffer' -> \memory' -> \memoryOffset' -> vkBindBufferMemory commandTable device' buffer' memory' memoryOffset' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


-- | vkBindImageMemory - Bind device memory to an image object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the image and memory.
--
-- -   @image@ is the image.
--
-- -   @memory@ is the 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory'
--     object describing the device memory to attach.
--
-- -   @memoryOffset@ is the start offset of the region of @memory@ which
--     is to be bound to the image. The number of bytes returned in the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'::@size@
--     member in @memory@, starting from @memoryOffset@ bytes, will be
--     bound to the specified image.
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
--     allowed in the @memoryTypeBits@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
--     with @image@
--
-- -   @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
--     with @image@
--
-- -   The @size@ member of the
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure returned from a call to
--     'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements'
--     with @image@ /must/ be less than or equal to the size of @memory@
--     minus @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @memory@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' handle
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage'
bindImageMemory :: Device ->  Image ->  DeviceMemory ->  DeviceSize ->  IO ()
bindImageMemory = \(Device device' commandTable) -> \image' -> \memory' -> \memoryOffset' -> vkBindImageMemory commandTable device' image' memory' memoryOffset' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


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
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure in which the memory requirements of the buffer object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
getBufferMemoryRequirements :: Device ->  Buffer ->  IO (MemoryRequirements)
getBufferMemoryRequirements = \(Device device' commandTable) -> \buffer' -> alloca (\pMemoryRequirements' -> vkGetBufferMemoryRequirements commandTable device' buffer' pMemoryRequirements' *> ((fromCStructMemoryRequirements <=< peek) pMemoryRequirements'))


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
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
--     structure in which the memory requirements of the image object are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
getImageMemoryRequirements :: Device ->  Image ->  IO (MemoryRequirements)
getImageMemoryRequirements = \(Device device' commandTable) -> \image' -> alloca (\pMemoryRequirements' -> vkGetImageMemoryRequirements commandTable device' image' pMemoryRequirements' *> ((fromCStructMemoryRequirements <=< peek) pMemoryRequirements'))
