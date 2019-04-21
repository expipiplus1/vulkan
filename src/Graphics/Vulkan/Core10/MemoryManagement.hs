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
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseBufferMemoryBindInfo',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements'
type Buffer = VkBuffer

-- | VkImage - Opaque handle to an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
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
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout'
type Image = VkImage


-- | VkMemoryRequirements - Structure specifying memory requirements
--
-- = Description
--
-- Unresolved directive in VkMemoryRequirements.txt -
-- include::{generated}\/validity\/structs\/VkMemoryRequirements.txt[]
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
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory' is
-- equivalent to passing the same parameters through
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo'
-- to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2'.
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
-- -   If @buffer@ requires a dedicated allocation(as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @buffer@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     equal to @buffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo'
--     provided when @memory@ was allocated included an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@
--     was not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @buffer@ /must/ equal
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@buffer@,
--     and @memoryOffset@ /must/ be zero.
--
-- -   If @buffer@ was created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @memory@ /must/
--     have been created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@buffer@
--     equal to a buffer handle created with identical creation parameters
--     to @buffer@ and @memoryOffset@ /must/ be zero
--
-- Unresolved directive in vkBindBufferMemory.txt -
-- include::{generated}\/validity\/protos\/vkBindBufferMemory.txt[]
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
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory' is
-- equivalent to passing the same parameters through
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo'
-- to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2'.
--
-- == Valid Usage
--
-- -   @image@ /must/ not have been created with the
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VK_IMAGE_CREATE_DISJOINT_BIT'
--     set.
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
-- -   If @image@ requires a dedicated allocation (as reported by
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2'
--     in
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements'::requiresDedicatedAllocation
--     for @image@), @memory@ /must/ have been created with
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     equal to @image@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is not enabled, and the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' provided when
--     @memory@ was allocated included an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     was not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @image@ /must/ equal
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     and @memoryOffset@ /must/ be zero.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dedicatedAllocationImageAliasing dedicated allocation image aliasing>
--     feature is enabled, and the
--     'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo' provided when
--     @memory@ was allocated included an instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'
--     in its @pNext@ chain, and
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     was not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', then
--     @memoryOffset@ /must/ be zero, and @image@ /must/ be either equal to
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo'::@image@
--     or an image that was created using the same parameters in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo', with the
--     exception that @extent@ and @arrayLayers@ /may/ differ subject to
--     the following restrictions: every dimension in the @extent@
--     parameter of the image being bound /must/ be equal to or smaller
--     than the original image for which the allocation was created; and
--     the @arrayLayers@ parameter of the image being bound /must/ be equal
--     to or smaller than the original image for which the allocation was
--     created.
--
-- -   If @image@ was created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV'::@dedicatedAllocation@
--     equal to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', @memory@ /must/
--     have been created with
--     'Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV'::@image@
--     equal to an image handle created with identical creation parameters
--     to @image@ and @memoryOffset@ /must/ be zero
--
-- Unresolved directive in vkBindImageMemory.txt -
-- include::{generated}\/validity\/protos\/vkBindImageMemory.txt[]
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
-- = Description
--
-- Unresolved directive in vkGetBufferMemoryRequirements.txt -
-- include::{generated}\/validity\/protos\/vkGetBufferMemoryRequirements.txt[]
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
-- == Valid Usage
--
-- Unresolved directive in vkGetImageMemoryRequirements.txt -
-- include::{generated}\/validity\/protos\/vkGetImageMemoryRequirements.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements'
getImageMemoryRequirements :: Device ->  Image ->  IO (MemoryRequirements)
getImageMemoryRequirements = \(Device device' commandTable) -> \image' -> alloca (\pMemoryRequirements' -> vkGetImageMemoryRequirements commandTable device' image' pMemoryRequirements' *> ((fromCStructMemoryRequirements <=< peek) pMemoryRequirements'))
