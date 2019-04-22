{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.CommandBufferBuilding
  ( withCStructBufferCopy
  , fromCStructBufferCopy
  , BufferCopy(..)
  , withCStructBufferImageCopy
  , fromCStructBufferImageCopy
  , BufferImageCopy(..)
  , withCStructBufferMemoryBarrier
  , fromCStructBufferMemoryBarrier
  , BufferMemoryBarrier(..)
  , withCStructClearAttachment
  , ClearAttachment(..)
  , withCStructClearColorValue
  , ClearColorValue(..)
  , withCStructClearDepthStencilValue
  , fromCStructClearDepthStencilValue
  , ClearDepthStencilValue(..)
  , withCStructClearRect
  , fromCStructClearRect
  , ClearRect(..)
  , withCStructClearValue
  , ClearValue(..)
  , withCStructDispatchIndirectCommand
  , fromCStructDispatchIndirectCommand
  , DispatchIndirectCommand(..)
  , withCStructDrawIndexedIndirectCommand
  , fromCStructDrawIndexedIndirectCommand
  , DrawIndexedIndirectCommand(..)
  , withCStructDrawIndirectCommand
  , fromCStructDrawIndirectCommand
  , DrawIndirectCommand(..)
  , withCStructImageBlit
  , fromCStructImageBlit
  , ImageBlit(..)
  , withCStructImageCopy
  , fromCStructImageCopy
  , ImageCopy(..)
  , withCStructImageMemoryBarrier
  , fromCStructImageMemoryBarrier
  , ImageMemoryBarrier(..)
  , withCStructImageResolve
  , fromCStructImageResolve
  , ImageResolve(..)
  , withCStructImageSubresourceLayers
  , fromCStructImageSubresourceLayers
  , ImageSubresourceLayers(..)
  , IndexType
  , pattern INDEX_TYPE_UINT16
  , pattern INDEX_TYPE_UINT32
  , pattern INDEX_TYPE_NONE_NV
  , withCStructMemoryBarrier
  , fromCStructMemoryBarrier
  , MemoryBarrier(..)
  , withCStructRenderPassBeginInfo
  , RenderPassBeginInfo(..)
  , StencilFaceFlagBits
  , pattern STENCIL_FACE_FRONT_BIT
  , pattern STENCIL_FACE_BACK_BIT
  , pattern STENCIL_FRONT_AND_BACK
  , StencilFaceFlags
  , SubpassContents
  , pattern SUBPASS_CONTENTS_INLINE
  , pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  , cmdBeginQuery
  , cmdBeginRenderPass
  , cmdBindDescriptorSets
  , cmdBindIndexBuffer
  , cmdBindPipeline
  , cmdBindVertexBuffers
  , cmdBlitImage
  , cmdClearAttachments
  , cmdClearColorImage
  , cmdClearDepthStencilImage
  , cmdCopyBuffer
  , cmdCopyBufferToImage
  , cmdCopyImage
  , cmdCopyImageToBuffer
  , cmdCopyQueryPoolResults
  , cmdDispatch
  , cmdDispatchIndirect
  , cmdDraw
  , cmdDrawIndexed
  , cmdDrawIndexedIndirect
  , cmdDrawIndirect
  , cmdEndQuery
  , cmdEndRenderPass
  , cmdExecuteCommands
  , cmdFillBuffer
  , cmdNextSubpass
  , cmdPipelineBarrier
  , cmdPushConstants
  , cmdResetEvent
  , cmdResetQueryPool
  , cmdResolveImage
  , cmdSetBlendConstants
  , cmdSetDepthBias
  , cmdSetDepthBounds
  , cmdSetEvent
  , cmdSetLineWidth
  , cmdSetScissor
  , cmdSetStencilCompareMask
  , cmdSetStencilReference
  , cmdSetStencilWriteMask
  , cmdSetViewport
  , cmdUpdateBuffer
  , cmdWaitEvents
  , cmdWriteTimestamp
  ) where

import Data.Function
  ( (&)
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , head
  , length
  )
import Data.Vector.Generic.Sized
  ( convert
  , fromTuple
  )
import qualified Data.Vector.Storable.Sized
  ( unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( Storable
  , pokeElemOff
  , sizeOf
  )


import Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkBufferCopy(..)
  , VkBufferImageCopy(..)
  , VkBufferMemoryBarrier(..)
  , VkClearAttachment(..)
  , VkClearColorValue(..)
  , VkClearDepthStencilValue(..)
  , VkClearRect(..)
  , VkClearValue(..)
  , VkDispatchIndirectCommand(..)
  , VkDrawIndexedIndirectCommand(..)
  , VkDrawIndirectCommand(..)
  , VkImageBlit(..)
  , VkImageCopy(..)
  , VkImageMemoryBarrier(..)
  , VkImageResolve(..)
  , VkImageSubresourceLayers(..)
  , VkIndexType(..)
  , VkMemoryBarrier(..)
  , VkRenderPassBeginInfo(..)
  , VkStencilFaceFlagBits(..)
  , VkSubpassContents(..)
  , vkCmdBeginQuery
  , vkCmdBeginRenderPass
  , vkCmdBindDescriptorSets
  , vkCmdBindIndexBuffer
  , vkCmdBindPipeline
  , vkCmdBindVertexBuffers
  , vkCmdBlitImage
  , vkCmdClearAttachments
  , vkCmdClearColorImage
  , vkCmdClearDepthStencilImage
  , vkCmdCopyBuffer
  , vkCmdCopyBufferToImage
  , vkCmdCopyImage
  , vkCmdCopyImageToBuffer
  , vkCmdCopyQueryPoolResults
  , vkCmdDispatch
  , vkCmdDispatchIndirect
  , vkCmdDraw
  , vkCmdDrawIndexed
  , vkCmdDrawIndexedIndirect
  , vkCmdDrawIndirect
  , vkCmdEndQuery
  , vkCmdEndRenderPass
  , vkCmdExecuteCommands
  , vkCmdFillBuffer
  , vkCmdNextSubpass
  , vkCmdPipelineBarrier
  , vkCmdPushConstants
  , vkCmdResetEvent
  , vkCmdResetQueryPool
  , vkCmdResolveImage
  , vkCmdSetBlendConstants
  , vkCmdSetDepthBias
  , vkCmdSetDepthBounds
  , vkCmdSetEvent
  , vkCmdSetLineWidth
  , vkCmdSetScissor
  , vkCmdSetStencilCompareMask
  , vkCmdSetStencilReference
  , vkCmdSetStencilWriteMask
  , vkCmdSetViewport
  , vkCmdUpdateBuffer
  , vkCmdWaitEvents
  , vkCmdWriteTimestamp
  , pattern VK_INDEX_TYPE_UINT16
  , pattern VK_INDEX_TYPE_UINT32
  , pattern VK_STENCIL_FACE_BACK_BIT
  , pattern VK_STENCIL_FACE_FRONT_BIT
  , pattern VK_STENCIL_FRONT_AND_BACK
  , pattern VK_SUBPASS_CONTENTS_INLINE
  , pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_INDEX_TYPE_NONE_NV
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( QueryControlFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( DescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Extent3D(..)
  , DeviceSize
  , fromCStructExtent3D
  , withCStructExtent3D
  )
import Graphics.Vulkan.Core10.Event
  ( Event
  )
import Graphics.Vulkan.Core10.Image
  ( ImageLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageSubresourceRange(..)
  , fromCStructImageSubresourceRange
  , withCStructImageSubresourceRange
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( Buffer
  , Image
  )
import Graphics.Vulkan.Core10.Pass
  ( AccessFlags
  , DependencyFlags
  , Framebuffer
  , PipelineBindPoint
  )
import Graphics.Vulkan.Core10.Pipeline
  ( Rect2D(..)
  , Viewport(..)
  , Pipeline
  , PipelineLayout
  , RenderPass
  , fromCStructRect2D
  , withCStructRect2D
  , withCStructViewport
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( ShaderStageFlags
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPool
  , QueryResultFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , PipelineStageFlagBits
  , PipelineStageFlags
  )
import Graphics.Vulkan.Core10.Sampler
  ( Filter
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( Offset3D(..)
  , ImageAspectFlags
  , fromCStructOffset3D
  , withCStructOffset3D
  )
import Graphics.Vulkan.Marshal.Utils
  ( withSizedArray
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkBufferCopy - Structure specifying a buffer copy operation
--
-- == Valid Usage
--
-- -   The @size@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBuffer'
data BufferCopy = BufferCopy
  { -- No documentation found for Nested "BufferCopy" "srcOffset"
  srcOffset :: DeviceSize
  , -- No documentation found for Nested "BufferCopy" "dstOffset"
  dstOffset :: DeviceSize
  , -- No documentation found for Nested "BufferCopy" "size"
  size :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferCopy' and
-- marshal a 'BufferCopy' into it. The 'VkBufferCopy' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferCopy :: BufferCopy -> (VkBufferCopy -> IO a) -> IO a
withCStructBufferCopy marshalled cont = cont (VkBufferCopy (srcOffset (marshalled :: BufferCopy)) (dstOffset (marshalled :: BufferCopy)) (size (marshalled :: BufferCopy)))

-- | A function to read a 'VkBufferCopy' and all additional
-- structures in the pointer chain into a 'BufferCopy'.
fromCStructBufferCopy :: VkBufferCopy -> IO BufferCopy
fromCStructBufferCopy c = BufferCopy <$> pure (vkSrcOffset (c :: VkBufferCopy))
                                     <*> pure (vkDstOffset (c :: VkBufferCopy))
                                     <*> pure (vkSize (c :: VkBufferCopy))

instance Zero BufferCopy where
  zero = BufferCopy zero
                    zero
                    zero



-- | VkBufferImageCopy - Structure specifying a buffer image copy operation
--
-- = Description
--
-- When copying to or from a depth or stencil aspect, the data in buffer
-- memory uses a layout that is a (mostly) tightly packed representation of
-- the depth or stencil data. Specifically:
--
-- -   data copied to or from the stencil aspect of any depth\/stencil
--     format is tightly packed with one
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_S8_UINT' value per texel.
--
-- -   data copied to or from the depth aspect of a
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D16_UNORM' or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D16_UNORM_S8_UINT' format
--     is tightly packed with one
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D16_UNORM' value per texel.
--
-- -   data copied to or from the depth aspect of a
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D32_SFLOAT' or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D32_SFLOAT_S8_UINT' format
--     is tightly packed with one
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D32_SFLOAT' value per
--     texel.
--
-- -   data copied to or from the depth aspect of a
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_X8_D24_UNORM_PACK32' or
--     'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D24_UNORM_S8_UINT' format
--     is packed with one 32-bit word per texel with the D24 value in the
--     LSBs of the word, and undefined values in the eight MSBs.
--
-- __Note__
--
-- To copy both the depth and stencil aspects of a depth\/stencil format,
-- two entries in @pRegions@ /can/ be used, where one specifies the depth
-- aspect in @imageSubresource@, and the other specifies the stencil
-- aspect.
--
-- Because depth or stencil aspect buffer to image copies /may/ require
-- format conversions on some implementations, they are not supported on
-- queues that do not support graphics.
--
-- When copying to a depth aspect, the data in buffer memory /must/ be in
-- the range [0,1], or the resulting values are undefined.
--
-- Copies are done layer by layer starting with image layer
-- @baseArrayLayer@ member of @imageSubresource@. @layerCount@ layers are
-- copied from the source image or to the destination image.
--
-- == Valid Usage
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter’s
--     format is not a depth\/stencil format, then @bufferOffset@ /must/ be
--     a multiple of the format’s texel block size.
--
-- -   @bufferOffset@ /must/ be a multiple of @4@
--
-- -   @bufferRowLength@ /must/ be @0@, or greater than or equal to the
--     @width@ member of @imageExtent@
--
-- -   @bufferImageHeight@ /must/ be @0@, or greater than or equal to the
--     @height@ member of @imageExtent@
--
-- -   @imageOffset.x@ and (@imageExtent.width@ + @imageOffset.x@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource width
--
-- -   @imageOffset.y@ and (imageExtent.height + @imageOffset.y@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource height
--
-- -   If the calling command’s @srcImage@
--     ('Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer')
--     or @dstImage@
--     ('Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage')
--     is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @imageOffset.y@ /must/ be @0@ and @imageExtent.height@ /must/
--     be @1@.
--
-- -   @imageOffset.z@ and (imageExtent.depth + @imageOffset.z@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource depth
--
-- -   If the calling command’s @srcImage@
--     ('Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer')
--     or @dstImage@
--     ('Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage')
--     is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @imageOffset.z@ /must/ be @0@ and @imageExtent.depth@ /must/ be
--     @1@
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, @bufferRowLength@ /must/ be a multiple of the
--     compressed texel block width
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, @bufferImageHeight@ /must/ be a multiple of the
--     compressed texel block height
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, all members of @imageOffset@ /must/ be a multiple
--     of the corresponding dimensions of the compressed texel block
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, @bufferOffset@ /must/ be a multiple of the
--     compressed texel block size in bytes
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, @imageExtent.width@ /must/ be a multiple of the
--     compressed texel block width or (@imageExtent.width@ +
--     @imageOffset.x@) /must/ equal the image subresource width
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, @imageExtent.height@ /must/ be a multiple of the
--     compressed texel block height or (@imageExtent.height@ +
--     @imageOffset.y@) /must/ equal the image subresource height
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is a
--     compressed image, @imageExtent.depth@ /must/ be a multiple of the
--     compressed texel block depth or (@imageExtent.depth@ +
--     @imageOffset.z@) /must/ equal the image subresource depth
--
-- -   The @aspectMask@ member of @imageSubresource@ /must/ specify aspects
--     present in the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter
--
-- -   The @aspectMask@ member of @imageSubresource@ /must/ only have a
--     single bit set
--
-- -   If the calling command’s
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' parameter is of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType'
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D',
--     the @baseArrayLayer@ and @layerCount@ members of @imageSubresource@
--     /must/ be @0@ and @1@, respectively
--
-- -   When copying to the depth aspect of an image subresource, the data
--     in the source buffer /must/ be in the range [0,1]
--
-- == Valid Usage (Implicit)
--
-- -   @imageSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'
data BufferImageCopy = BufferImageCopy
  { -- No documentation found for Nested "BufferImageCopy" "bufferOffset"
  bufferOffset :: DeviceSize
  , -- No documentation found for Nested "BufferImageCopy" "bufferRowLength"
  bufferRowLength :: Word32
  , -- No documentation found for Nested "BufferImageCopy" "bufferImageHeight"
  bufferImageHeight :: Word32
  , -- No documentation found for Nested "BufferImageCopy" "imageSubresource"
  imageSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "BufferImageCopy" "imageOffset"
  imageOffset :: Offset3D
  , -- No documentation found for Nested "BufferImageCopy" "imageExtent"
  imageExtent :: Extent3D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferImageCopy' and
-- marshal a 'BufferImageCopy' into it. The 'VkBufferImageCopy' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferImageCopy :: BufferImageCopy -> (VkBufferImageCopy -> IO a) -> IO a
withCStructBufferImageCopy marshalled cont = withCStructExtent3D (imageExtent (marshalled :: BufferImageCopy)) (\imageExtent'' -> withCStructOffset3D (imageOffset (marshalled :: BufferImageCopy)) (\imageOffset'' -> withCStructImageSubresourceLayers (imageSubresource (marshalled :: BufferImageCopy)) (\imageSubresource'' -> cont (VkBufferImageCopy (bufferOffset (marshalled :: BufferImageCopy)) (bufferRowLength (marshalled :: BufferImageCopy)) (bufferImageHeight (marshalled :: BufferImageCopy)) imageSubresource'' imageOffset'' imageExtent''))))

-- | A function to read a 'VkBufferImageCopy' and all additional
-- structures in the pointer chain into a 'BufferImageCopy'.
fromCStructBufferImageCopy :: VkBufferImageCopy -> IO BufferImageCopy
fromCStructBufferImageCopy c = BufferImageCopy <$> pure (vkBufferOffset (c :: VkBufferImageCopy))
                                               <*> pure (vkBufferRowLength (c :: VkBufferImageCopy))
                                               <*> pure (vkBufferImageHeight (c :: VkBufferImageCopy))
                                               <*> (fromCStructImageSubresourceLayers (vkImageSubresource (c :: VkBufferImageCopy)))
                                               <*> (fromCStructOffset3D (vkImageOffset (c :: VkBufferImageCopy)))
                                               <*> (fromCStructExtent3D (vkImageExtent (c :: VkBufferImageCopy)))

instance Zero BufferImageCopy where
  zero = BufferImageCopy zero
                         zero
                         zero
                         zero
                         zero
                         zero



-- | VkBufferMemoryBarrier - Structure specifying a buffer memory barrier
--
-- = Description
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT', memory writes
-- performed by that access type are also made visible, as that access type
-- is not performed through a resource.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks destination access mask>.
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT' or
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_READ_BIT', available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified buffer range, and the second access scope includes no
-- access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified buffer range, and the first access scope includes no
-- access, as if @srcAccessMask@ was @0@.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     greater than @0@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     less than or equal to than the size of @buffer@ minus @offset@
--
-- -   If @buffer@ was created with a sharing mode of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be
--     'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED'
--
-- -   If @buffer@ was created with a sharing mode of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ either both
--     be 'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED', or
--     both be a valid queue family (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-queueprops>)
--
-- -   If @buffer@ was created with a sharing mode of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED', at
--     least one of them /must/ be the same as the family of the queue that
--     will execute this barrier
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @srcAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits' values
--
-- -   @dstAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits' values
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
data BufferMemoryBarrier = BufferMemoryBarrier
  { -- Univalued member elided
  -- No documentation found for Nested "BufferMemoryBarrier" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "BufferMemoryBarrier" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "BufferMemoryBarrier" "dstAccessMask"
  dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "BufferMemoryBarrier" "srcQueueFamilyIndex"
  srcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "BufferMemoryBarrier" "dstQueueFamilyIndex"
  dstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "BufferMemoryBarrier" "buffer"
  buffer :: Buffer
  , -- No documentation found for Nested "BufferMemoryBarrier" "offset"
  offset :: DeviceSize
  , -- No documentation found for Nested "BufferMemoryBarrier" "size"
  size :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkBufferMemoryBarrier' and
-- marshal a 'BufferMemoryBarrier' into it. The 'VkBufferMemoryBarrier' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructBufferMemoryBarrier :: BufferMemoryBarrier -> (VkBufferMemoryBarrier -> IO a) -> IO a
withCStructBufferMemoryBarrier marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: BufferMemoryBarrier)) (\pPNext -> cont (VkBufferMemoryBarrier VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER pPNext (srcAccessMask (marshalled :: BufferMemoryBarrier)) (dstAccessMask (marshalled :: BufferMemoryBarrier)) (srcQueueFamilyIndex (marshalled :: BufferMemoryBarrier)) (dstQueueFamilyIndex (marshalled :: BufferMemoryBarrier)) (buffer (marshalled :: BufferMemoryBarrier)) (offset (marshalled :: BufferMemoryBarrier)) (size (marshalled :: BufferMemoryBarrier))))

-- | A function to read a 'VkBufferMemoryBarrier' and all additional
-- structures in the pointer chain into a 'BufferMemoryBarrier'.
fromCStructBufferMemoryBarrier :: VkBufferMemoryBarrier -> IO BufferMemoryBarrier
fromCStructBufferMemoryBarrier c = BufferMemoryBarrier <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkBufferMemoryBarrier)))
                                                       <*> pure (vkSrcAccessMask (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkDstAccessMask (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkSrcQueueFamilyIndex (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkDstQueueFamilyIndex (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkBuffer (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkOffset (c :: VkBufferMemoryBarrier))
                                                       <*> pure (vkSize (c :: VkBufferMemoryBarrier))

instance Zero BufferMemoryBarrier where
  zero = BufferMemoryBarrier Nothing
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero
                             zero



-- | VkClearAttachment - Structure specifying a clear attachment
--
-- = Description
--
-- No memory barriers are needed between
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'
-- and preceding or subsequent draw or attachment clear commands in the
-- same subpass.
--
-- The
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'
-- command is not affected by the bound pipeline state.
--
-- Attachments /can/ also be cleared at the beginning of a render pass
-- instance by setting @loadOp@ (or @stencilLoadOp@) of
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR', as
-- described for 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass'.
--
-- == Valid Usage
--
-- -   If @aspectMask@ includes
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
--     it /must/ not include
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
--
-- -   @aspectMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_METADATA_BIT'
--
-- -   @clearValue@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearValue' union
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearValue',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'
data ClearAttachment = ClearAttachment
  { -- No documentation found for Nested "ClearAttachment" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ClearAttachment" "colorAttachment"
  colorAttachment :: Word32
  , -- No documentation found for Nested "ClearAttachment" "clearValue"
  clearValue :: ClearValue
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkClearAttachment' and
-- marshal a 'ClearAttachment' into it. The 'VkClearAttachment' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructClearAttachment :: ClearAttachment -> (VkClearAttachment -> IO a) -> IO a
withCStructClearAttachment marshalled cont = withCStructClearValue (clearValue (marshalled :: ClearAttachment)) (\clearValue'' -> cont (VkClearAttachment (aspectMask (marshalled :: ClearAttachment)) (colorAttachment (marshalled :: ClearAttachment)) clearValue''))

-- No fromCStruct function for types containing unions

instance Zero ClearAttachment where
  zero = ClearAttachment zero
                         zero
                         zero



-- | VkClearColorValue - Structure specifying a clear color value
--
-- = Description
--
-- The four array elements of the clear color map to R, G, B, and A
-- components of image formats, in order.
--
-- If the image has more than one sample, the same value is written to all
-- samples for any pixels being cleared.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearValue',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearColorImage'
data ClearColorValue
  = Float32 (CFloat, CFloat, CFloat, CFloat)
  | Int32 (Int32, Int32, Int32, Int32)
  | Uint32 (Word32, Word32, Word32, Word32)
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkClearColorValue' and
-- marshal a 'ClearColorValue' into it. The 'VkClearColorValue' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructClearColorValue :: ClearColorValue -> (VkClearColorValue -> IO a) -> IO a
withCStructClearColorValue marshalled cont = case marshalled of
  Float32 t -> cont (VkFloat32 (fromTuple t))
  Int32 t -> cont (VkInt32 (fromTuple t))
  Uint32 t -> cont (VkUint32 (fromTuple t))

-- No FromCStruct function for sum types

instance Zero ClearColorValue where
  zero = Float32 (zero, zero, zero, zero)



-- | VkClearDepthStencilValue - Structure specifying a clear depth stencil
-- value
--
-- == Valid Usage
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearValue',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearDepthStencilImage'
data ClearDepthStencilValue = ClearDepthStencilValue
  { -- No documentation found for Nested "ClearDepthStencilValue" "depth"
  depth :: CFloat
  , -- No documentation found for Nested "ClearDepthStencilValue" "stencil"
  stencil :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkClearDepthStencilValue' and
-- marshal a 'ClearDepthStencilValue' into it. The 'VkClearDepthStencilValue' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructClearDepthStencilValue :: ClearDepthStencilValue -> (VkClearDepthStencilValue -> IO a) -> IO a
withCStructClearDepthStencilValue marshalled cont = cont (VkClearDepthStencilValue (depth (marshalled :: ClearDepthStencilValue)) (stencil (marshalled :: ClearDepthStencilValue)))

-- | A function to read a 'VkClearDepthStencilValue' and all additional
-- structures in the pointer chain into a 'ClearDepthStencilValue'.
fromCStructClearDepthStencilValue :: VkClearDepthStencilValue -> IO ClearDepthStencilValue
fromCStructClearDepthStencilValue c = ClearDepthStencilValue <$> pure (vkDepth (c :: VkClearDepthStencilValue))
                                                             <*> pure (vkStencil (c :: VkClearDepthStencilValue))

instance Zero ClearDepthStencilValue where
  zero = ClearDepthStencilValue zero
                                zero



-- | VkClearRect - Structure specifying a clear rectangle
--
-- = Description
--
-- The layers [@baseArrayLayer@, @baseArrayLayer@ + @layerCount@) counting
-- from the base layer of the attachment image view are cleared.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'
data ClearRect = ClearRect
  { -- No documentation found for Nested "ClearRect" "rect"
  rect :: Rect2D
  , -- No documentation found for Nested "ClearRect" "baseArrayLayer"
  baseArrayLayer :: Word32
  , -- No documentation found for Nested "ClearRect" "layerCount"
  layerCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkClearRect' and
-- marshal a 'ClearRect' into it. The 'VkClearRect' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructClearRect :: ClearRect -> (VkClearRect -> IO a) -> IO a
withCStructClearRect marshalled cont = withCStructRect2D (rect (marshalled :: ClearRect)) (\rect'' -> cont (VkClearRect rect'' (baseArrayLayer (marshalled :: ClearRect)) (layerCount (marshalled :: ClearRect))))

-- | A function to read a 'VkClearRect' and all additional
-- structures in the pointer chain into a 'ClearRect'.
fromCStructClearRect :: VkClearRect -> IO ClearRect
fromCStructClearRect c = ClearRect <$> (fromCStructRect2D (vkRect (c :: VkClearRect)))
                                   <*> pure (vkBaseArrayLayer (c :: VkClearRect))
                                   <*> pure (vkLayerCount (c :: VkClearRect))

instance Zero ClearRect where
  zero = ClearRect zero
                   zero
                   zero



-- | VkClearValue - Structure specifying a clear value
--
-- = Description
--
-- This union is used where part of the API requires either color or
-- depth\/stencil clear values, depending on the attachment, and defines
-- the initial clear values in the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'
-- structure.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearAttachment',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearColorValue',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearDepthStencilValue',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'
data ClearValue
  = Color ClearColorValue
  | DepthStencil ClearDepthStencilValue
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkClearValue' and
-- marshal a 'ClearValue' into it. The 'VkClearValue' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructClearValue :: ClearValue -> (VkClearValue -> IO a) -> IO a
withCStructClearValue marshalled cont = case marshalled of
  Color s -> withCStructClearColorValue s (cont . VkColor)
  DepthStencil s -> withCStructClearDepthStencilValue s (cont . VkDepthStencil)

-- No FromCStruct function for sum types

instance Zero ClearValue where
  zero = Color zero



-- | VkDispatchIndirectCommand - Structure specifying a dispatch indirect
-- command
--
-- = Description
--
-- The members of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDispatchIndirectCommand'
-- have the same meaning as the corresponding parameters of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch'.
--
-- == Valid Usage
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect'
data DispatchIndirectCommand = DispatchIndirectCommand
  { -- No documentation found for Nested "DispatchIndirectCommand" "x"
  x :: Word32
  , -- No documentation found for Nested "DispatchIndirectCommand" "y"
  y :: Word32
  , -- No documentation found for Nested "DispatchIndirectCommand" "z"
  z :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDispatchIndirectCommand' and
-- marshal a 'DispatchIndirectCommand' into it. The 'VkDispatchIndirectCommand' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDispatchIndirectCommand :: DispatchIndirectCommand -> (VkDispatchIndirectCommand -> IO a) -> IO a
withCStructDispatchIndirectCommand marshalled cont = cont (VkDispatchIndirectCommand (x (marshalled :: DispatchIndirectCommand)) (y (marshalled :: DispatchIndirectCommand)) (z (marshalled :: DispatchIndirectCommand)))

-- | A function to read a 'VkDispatchIndirectCommand' and all additional
-- structures in the pointer chain into a 'DispatchIndirectCommand'.
fromCStructDispatchIndirectCommand :: VkDispatchIndirectCommand -> IO DispatchIndirectCommand
fromCStructDispatchIndirectCommand c = DispatchIndirectCommand <$> pure (vkX (c :: VkDispatchIndirectCommand))
                                                               <*> pure (vkY (c :: VkDispatchIndirectCommand))
                                                               <*> pure (vkZ (c :: VkDispatchIndirectCommand))

instance Zero DispatchIndirectCommand where
  zero = DispatchIndirectCommand zero
                                 zero
                                 zero



-- | VkDrawIndexedIndirectCommand - Structure specifying a draw indexed
-- indirect command
--
-- = Description
--
-- The members of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'
-- have the same meaning as the similarly named parameters of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexed'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-input>
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     @indexSize@ being based on the type specified by @indexType@, where
--     the index buffer, @indexType@, and @offset@ are specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
data DrawIndexedIndirectCommand = DrawIndexedIndirectCommand
  { -- No documentation found for Nested "DrawIndexedIndirectCommand" "indexCount"
  indexCount :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "instanceCount"
  instanceCount :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "firstIndex"
  firstIndex :: Word32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "vertexOffset"
  vertexOffset :: Int32
  , -- No documentation found for Nested "DrawIndexedIndirectCommand" "firstInstance"
  firstInstance :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDrawIndexedIndirectCommand' and
-- marshal a 'DrawIndexedIndirectCommand' into it. The 'VkDrawIndexedIndirectCommand' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDrawIndexedIndirectCommand :: DrawIndexedIndirectCommand -> (VkDrawIndexedIndirectCommand -> IO a) -> IO a
withCStructDrawIndexedIndirectCommand marshalled cont = cont (VkDrawIndexedIndirectCommand (indexCount (marshalled :: DrawIndexedIndirectCommand)) (instanceCount (marshalled :: DrawIndexedIndirectCommand)) (firstIndex (marshalled :: DrawIndexedIndirectCommand)) (vertexOffset (marshalled :: DrawIndexedIndirectCommand)) (firstInstance (marshalled :: DrawIndexedIndirectCommand)))

-- | A function to read a 'VkDrawIndexedIndirectCommand' and all additional
-- structures in the pointer chain into a 'DrawIndexedIndirectCommand'.
fromCStructDrawIndexedIndirectCommand :: VkDrawIndexedIndirectCommand -> IO DrawIndexedIndirectCommand
fromCStructDrawIndexedIndirectCommand c = DrawIndexedIndirectCommand <$> pure (vkIndexCount (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkInstanceCount (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkFirstIndex (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkVertexOffset (c :: VkDrawIndexedIndirectCommand))
                                                                     <*> pure (vkFirstInstance (c :: VkDrawIndexedIndirectCommand))

instance Zero DrawIndexedIndirectCommand where
  zero = DrawIndexedIndirectCommand zero
                                    zero
                                    zero
                                    zero
                                    zero



-- | VkDrawIndirectCommand - Structure specifying a draw indirect command
--
-- = Description
--
-- The members of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'
-- have the same meaning as the similarly named parameters of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-input>
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect'
data DrawIndirectCommand = DrawIndirectCommand
  { -- No documentation found for Nested "DrawIndirectCommand" "vertexCount"
  vertexCount :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "instanceCount"
  instanceCount :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "firstVertex"
  firstVertex :: Word32
  , -- No documentation found for Nested "DrawIndirectCommand" "firstInstance"
  firstInstance :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkDrawIndirectCommand' and
-- marshal a 'DrawIndirectCommand' into it. The 'VkDrawIndirectCommand' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructDrawIndirectCommand :: DrawIndirectCommand -> (VkDrawIndirectCommand -> IO a) -> IO a
withCStructDrawIndirectCommand marshalled cont = cont (VkDrawIndirectCommand (vertexCount (marshalled :: DrawIndirectCommand)) (instanceCount (marshalled :: DrawIndirectCommand)) (firstVertex (marshalled :: DrawIndirectCommand)) (firstInstance (marshalled :: DrawIndirectCommand)))

-- | A function to read a 'VkDrawIndirectCommand' and all additional
-- structures in the pointer chain into a 'DrawIndirectCommand'.
fromCStructDrawIndirectCommand :: VkDrawIndirectCommand -> IO DrawIndirectCommand
fromCStructDrawIndirectCommand c = DrawIndirectCommand <$> pure (vkVertexCount (c :: VkDrawIndirectCommand))
                                                       <*> pure (vkInstanceCount (c :: VkDrawIndirectCommand))
                                                       <*> pure (vkFirstVertex (c :: VkDrawIndirectCommand))
                                                       <*> pure (vkFirstInstance (c :: VkDrawIndirectCommand))

instance Zero DrawIndirectCommand where
  zero = DrawIndirectCommand zero
                             zero
                             zero
                             zero



-- | VkImageBlit - Structure specifying an image blit operation
--
-- = Description
--
-- For each element of the @pRegions@ array, a blit operation is performed
-- the specified source and destination regions.
--
-- == Valid Usage
--
-- -   The @aspectMask@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   The @layerCount@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType'
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D',
--     the @baseArrayLayer@ and @layerCount@ members of both
--     @srcSubresource@ and @dstSubresource@ /must/ be @0@ and @1@,
--     respectively
--
-- -   The @aspectMask@ member of @srcSubresource@ /must/ specify aspects
--     present in the calling command’s @srcImage@
--
-- -   The @aspectMask@ member of @dstSubresource@ /must/ specify aspects
--     present in the calling command’s @dstImage@
--
-- -   @srcOffset@[0].@x@ and @srcOffset@[1].@x@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the source image
--     subresource width
--
-- -   @srcOffset@[0].@y@ and @srcOffset@[1].@y@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the source image
--     subresource height
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @srcOffset@[0].y /must/ be @0@ and @srcOffset@[1].y /must/ be
--     @1@.
--
-- -   @srcOffset@[0].@z@ and @srcOffset@[1].@z@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the source image
--     subresource depth
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @srcOffset@[0].z /must/ be @0@ and @srcOffset@[1].z /must/ be
--     @1@.
--
-- -   @dstOffset@[0].@x@ and @dstOffset@[1].@x@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the destination image
--     subresource width
--
-- -   @dstOffset@[0].@y@ and @dstOffset@[1].@y@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the destination image
--     subresource height
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @dstOffset@[0].y /must/ be @0@ and @dstOffset@[1].y /must/ be
--     @1@.
--
-- -   @dstOffset@[0].@z@ and @dstOffset@[1].@z@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the destination image
--     subresource depth
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @dstOffset@[0].z /must/ be @0@ and @dstOffset@[1].z /must/ be
--     @1@.
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- -   @dstSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
data ImageBlit = ImageBlit
  { -- No documentation found for Nested "ImageBlit" "srcSubresource"
  srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageBlit" "srcOffsets"
  srcOffsets :: (Offset3D, Offset3D)
  , -- No documentation found for Nested "ImageBlit" "dstSubresource"
  dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageBlit" "dstOffsets"
  dstOffsets :: (Offset3D, Offset3D)
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageBlit' and
-- marshal a 'ImageBlit' into it. The 'VkImageBlit' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageBlit :: ImageBlit -> (VkImageBlit -> IO a) -> IO a
withCStructImageBlit marshalled cont = withSizedArray withCStructOffset3D (fromTuple (dstOffsets (marshalled :: ImageBlit))) (\dstOffsets' -> withCStructImageSubresourceLayers (dstSubresource (marshalled :: ImageBlit)) (\dstSubresource'' -> withSizedArray withCStructOffset3D (fromTuple (srcOffsets (marshalled :: ImageBlit))) (\srcOffsets' -> withCStructImageSubresourceLayers (srcSubresource (marshalled :: ImageBlit)) (\srcSubresource'' -> cont (VkImageBlit srcSubresource'' (Data.Vector.Generic.Sized.convert srcOffsets') dstSubresource'' (Data.Vector.Generic.Sized.convert dstOffsets'))))))

-- | A function to read a 'VkImageBlit' and all additional
-- structures in the pointer chain into a 'ImageBlit'.
fromCStructImageBlit :: VkImageBlit -> IO ImageBlit
fromCStructImageBlit c = ImageBlit <$> (fromCStructImageSubresourceLayers (vkSrcSubresource (c :: VkImageBlit)))
                                   <*> (let v = (vkSrcOffsets (c :: VkImageBlit)) in (, ) <$> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex v 0)
                                                                                          <*> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex v 1))
                                   <*> (fromCStructImageSubresourceLayers (vkDstSubresource (c :: VkImageBlit)))
                                   <*> (let v = (vkDstOffsets (c :: VkImageBlit)) in (, ) <$> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex v 0)
                                                                                          <*> fromCStructOffset3D (Data.Vector.Storable.Sized.unsafeIndex v 1))

instance Zero ImageBlit where
  zero = ImageBlit zero
                   (zero, zero)
                   zero
                   (zero, zero)



-- | VkImageCopy - Structure specifying an image copy operation
--
-- = Description
--
-- Copies are done layer by layer starting with @baseArrayLayer@ member of
-- @srcSubresource@ for the source and @dstSubresource@ for the
-- destination. @layerCount@ layers are copied to the destination image.
--
-- == Valid Usage
--
-- -   The @aspectMask@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   The @layerCount@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType'
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D',
--     the @baseArrayLayer@ and @layerCount@ members of both
--     @srcSubresource@ and @dstSubresource@ /must/ be @0@ and @1@,
--     respectively
--
-- -   The @aspectMask@ member of @srcSubresource@ /must/ specify aspects
--     present in the calling command’s @srcImage@
--
-- -   The @aspectMask@ member of @dstSubresource@ /must/ specify aspects
--     present in the calling command’s @dstImage@
--
-- -   @srcOffset.x@ and (@extent.width@ + @srcOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource width
--
-- -   @srcOffset.y@ and (@extent.height@ + @srcOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource height
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @srcOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @srcOffset.z@ /must/ be @0@.
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @dstOffset.z@ /must/ be @0@.
--
-- -   If the calling command’s @srcImage@ or @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @extent.depth@ /must/ be @1@.
--
-- -   @dstOffset.x@ and (@extent.width@ + @dstOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource width
--
-- -   @dstOffset.y@ and (@extent.height@ + @dstOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @dstOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource depth
--
-- -   If the calling command’s @srcImage@ is a compressed image, all
--     members of @srcOffset@ /must/ be a multiple of the corresponding
--     dimensions of the compressed texel block
--
-- -   If the calling command’s @srcImage@ is a compressed image,
--     @extent.width@ /must/ be a multiple of the compressed texel block
--     width or (@extent.width@ + @srcOffset.x@) /must/ equal the source
--     image subresource width
--
-- -   If the calling command’s @srcImage@ is a compressed image,
--     @extent.height@ /must/ be a multiple of the compressed texel block
--     height or (@extent.height@ + @srcOffset.y@) /must/ equal the source
--     image subresource height
--
-- -   If the calling command’s @srcImage@ is a compressed image,
--     @extent.depth@ /must/ be a multiple of the compressed texel block
--     depth or (@extent.depth@ + @srcOffset.z@) /must/ equal the source
--     image subresource depth
--
-- -   If the calling command’s @dstImage@ is a compressed format image,
--     all members of @dstOffset@ /must/ be a multiple of the corresponding
--     dimensions of the compressed texel block
--
-- -   If the calling command’s @dstImage@ is a compressed format image,
--     @extent.width@ /must/ be a multiple of the compressed texel block
--     width or (@extent.width@ + @dstOffset.x@) /must/ equal the
--     destination image subresource width
--
-- -   If the calling command’s @dstImage@ is a compressed format image,
--     @extent.height@ /must/ be a multiple of the compressed texel block
--     height or (@extent.height@ + @dstOffset.y@) /must/ equal the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is a compressed format image,
--     @extent.depth@ /must/ be a multiple of the compressed texel block
--     depth or (@extent.depth@ + @dstOffset.z@) /must/ equal the
--     destination image subresource depth
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- -   @dstSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage'
data ImageCopy = ImageCopy
  { -- No documentation found for Nested "ImageCopy" "srcSubresource"
  srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageCopy" "srcOffset"
  srcOffset :: Offset3D
  , -- No documentation found for Nested "ImageCopy" "dstSubresource"
  dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageCopy" "dstOffset"
  dstOffset :: Offset3D
  , -- No documentation found for Nested "ImageCopy" "extent"
  extent :: Extent3D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageCopy' and
-- marshal a 'ImageCopy' into it. The 'VkImageCopy' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageCopy :: ImageCopy -> (VkImageCopy -> IO a) -> IO a
withCStructImageCopy marshalled cont = withCStructExtent3D (extent (marshalled :: ImageCopy)) (\extent'' -> withCStructOffset3D (dstOffset (marshalled :: ImageCopy)) (\dstOffset'' -> withCStructImageSubresourceLayers (dstSubresource (marshalled :: ImageCopy)) (\dstSubresource'' -> withCStructOffset3D (srcOffset (marshalled :: ImageCopy)) (\srcOffset'' -> withCStructImageSubresourceLayers (srcSubresource (marshalled :: ImageCopy)) (\srcSubresource'' -> cont (VkImageCopy srcSubresource'' srcOffset'' dstSubresource'' dstOffset'' extent''))))))

-- | A function to read a 'VkImageCopy' and all additional
-- structures in the pointer chain into a 'ImageCopy'.
fromCStructImageCopy :: VkImageCopy -> IO ImageCopy
fromCStructImageCopy c = ImageCopy <$> (fromCStructImageSubresourceLayers (vkSrcSubresource (c :: VkImageCopy)))
                                   <*> (fromCStructOffset3D (vkSrcOffset (c :: VkImageCopy)))
                                   <*> (fromCStructImageSubresourceLayers (vkDstSubresource (c :: VkImageCopy)))
                                   <*> (fromCStructOffset3D (vkDstOffset (c :: VkImageCopy)))
                                   <*> (fromCStructExtent3D (vkExtent (c :: VkImageCopy)))

instance Zero ImageCopy where
  zero = ImageCopy zero
                   zero
                   zero
                   zero
                   zero



-- | VkImageMemoryBarrier - Structure specifying the parameters of an image
-- memory barrier
--
-- = Description
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT', memory writes
-- performed by that access type are also made visible, as that access type
-- is not performed through a resource.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_WRITE_BIT' or
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_HOST_READ_BIT', available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-release queue family release operation>
-- for the specified image subresource range, and the second access scope
-- includes no access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified image subresource range, and the first access scope
-- includes no access, as if @srcAccessMask@ was @0@.
--
-- If @oldLayout@ is not equal to @newLayout@, then the memory barrier
-- defines an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-image-layout-transitions image layout transition>
-- for the specified image subresource range.
--
-- Layout transitions that are performed via image memory barriers execute
-- in their entirety in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- relative to other image layout transitions submitted to the same queue,
-- including those performed by
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass render passes>.
-- In effect there is an implicit execution dependency from each such
-- layout transition to all layout transitions previously submitted to the
-- same queue.
--
-- == Valid Usage
--
-- -   @oldLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or the
--     current layout of the image subresources affected by the barrier
--
-- -   @newLayout@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_PREINITIALIZED'
--
-- -   If @image@ was created with a sharing mode of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_CONCURRENT',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ both be
--     'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED'
--
-- -   If @image@ was created with a sharing mode of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE',
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ /must/ either both
--     be 'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED', or
--     both be a valid queue family (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-queueprops>).
--
-- -   If @image@ was created with a sharing mode of
--     'Graphics.Vulkan.C.Core10.Buffer.VK_SHARING_MODE_EXCLUSIVE', and
--     @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ are not
--     'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED', at
--     least one of them /must/ be the same as the family of the queue that
--     will execute this barrier
--
-- -   @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when
--     @image@ was created
--
-- -   If @subresourceRange.levelCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_MIP_LEVELS',
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange.layerCount@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS',
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ has a depth\/stencil format with both depth and stencil
--     components, then the @aspectMask@ member of @subresourceRange@
--     /must/ include both
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
--     and
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--     set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--     set
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER'
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT'
--
-- -   @srcAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits' values
--
-- -   @dstAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pass.VkAccessFlagBits' values
--
-- -   @oldLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @newLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @subresourceRange@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
data ImageMemoryBarrier = ImageMemoryBarrier
  { -- Univalued member elided
  -- No documentation found for Nested "ImageMemoryBarrier" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImageMemoryBarrier" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "ImageMemoryBarrier" "dstAccessMask"
  dstAccessMask :: AccessFlags
  , -- No documentation found for Nested "ImageMemoryBarrier" "oldLayout"
  oldLayout :: ImageLayout
  , -- No documentation found for Nested "ImageMemoryBarrier" "newLayout"
  newLayout :: ImageLayout
  , -- No documentation found for Nested "ImageMemoryBarrier" "srcQueueFamilyIndex"
  srcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "ImageMemoryBarrier" "dstQueueFamilyIndex"
  dstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "ImageMemoryBarrier" "image"
  image :: Image
  , -- No documentation found for Nested "ImageMemoryBarrier" "subresourceRange"
  subresourceRange :: ImageSubresourceRange
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageMemoryBarrier' and
-- marshal a 'ImageMemoryBarrier' into it. The 'VkImageMemoryBarrier' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageMemoryBarrier :: ImageMemoryBarrier -> (VkImageMemoryBarrier -> IO a) -> IO a
withCStructImageMemoryBarrier marshalled cont = withCStructImageSubresourceRange (subresourceRange (marshalled :: ImageMemoryBarrier)) (\subresourceRange'' -> maybeWith withSomeVkStruct (next (marshalled :: ImageMemoryBarrier)) (\pPNext -> cont (VkImageMemoryBarrier VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER pPNext (srcAccessMask (marshalled :: ImageMemoryBarrier)) (dstAccessMask (marshalled :: ImageMemoryBarrier)) (oldLayout (marshalled :: ImageMemoryBarrier)) (newLayout (marshalled :: ImageMemoryBarrier)) (srcQueueFamilyIndex (marshalled :: ImageMemoryBarrier)) (dstQueueFamilyIndex (marshalled :: ImageMemoryBarrier)) (image (marshalled :: ImageMemoryBarrier)) subresourceRange'')))

-- | A function to read a 'VkImageMemoryBarrier' and all additional
-- structures in the pointer chain into a 'ImageMemoryBarrier'.
fromCStructImageMemoryBarrier :: VkImageMemoryBarrier -> IO ImageMemoryBarrier
fromCStructImageMemoryBarrier c = ImageMemoryBarrier <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImageMemoryBarrier)))
                                                     <*> pure (vkSrcAccessMask (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkDstAccessMask (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkOldLayout (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkNewLayout (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkSrcQueueFamilyIndex (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkDstQueueFamilyIndex (c :: VkImageMemoryBarrier))
                                                     <*> pure (vkImage (c :: VkImageMemoryBarrier))
                                                     <*> (fromCStructImageSubresourceRange (vkSubresourceRange (c :: VkImageMemoryBarrier)))

instance Zero ImageMemoryBarrier where
  zero = ImageMemoryBarrier Nothing
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero
                            zero



-- | VkImageResolve - Structure specifying an image resolve operation
--
-- == Valid Usage
--
-- -   The @aspectMask@ member of @srcSubresource@ and @dstSubresource@
--     /must/ only contain
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT'
--
-- -   The @layerCount@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType'
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D',
--     the @baseArrayLayer@ and @layerCount@ members of both
--     @srcSubresource@ and @dstSubresource@ /must/ be @0@ and @1@,
--     respectively
--
-- -   @srcOffset.x@ and (@extent.width@ + @srcOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource width
--
-- -   @srcOffset.y@ and (@extent.height@ + @srcOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource height
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @srcOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   @dstOffset.x@ and (@extent.width@ + @dstOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource width
--
-- -   @dstOffset.y@ and (@extent.height@ + @dstOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D',
--     then @dstOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource depth
--
-- -   If the calling command’s @dstImage@ is of type
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D' or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D',
--     then @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- -   @dstSubresource@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers'
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageSubresourceLayers',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage'
data ImageResolve = ImageResolve
  { -- No documentation found for Nested "ImageResolve" "srcSubresource"
  srcSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageResolve" "srcOffset"
  srcOffset :: Offset3D
  , -- No documentation found for Nested "ImageResolve" "dstSubresource"
  dstSubresource :: ImageSubresourceLayers
  , -- No documentation found for Nested "ImageResolve" "dstOffset"
  dstOffset :: Offset3D
  , -- No documentation found for Nested "ImageResolve" "extent"
  extent :: Extent3D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageResolve' and
-- marshal a 'ImageResolve' into it. The 'VkImageResolve' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageResolve :: ImageResolve -> (VkImageResolve -> IO a) -> IO a
withCStructImageResolve marshalled cont = withCStructExtent3D (extent (marshalled :: ImageResolve)) (\extent'' -> withCStructOffset3D (dstOffset (marshalled :: ImageResolve)) (\dstOffset'' -> withCStructImageSubresourceLayers (dstSubresource (marshalled :: ImageResolve)) (\dstSubresource'' -> withCStructOffset3D (srcOffset (marshalled :: ImageResolve)) (\srcOffset'' -> withCStructImageSubresourceLayers (srcSubresource (marshalled :: ImageResolve)) (\srcSubresource'' -> cont (VkImageResolve srcSubresource'' srcOffset'' dstSubresource'' dstOffset'' extent''))))))

-- | A function to read a 'VkImageResolve' and all additional
-- structures in the pointer chain into a 'ImageResolve'.
fromCStructImageResolve :: VkImageResolve -> IO ImageResolve
fromCStructImageResolve c = ImageResolve <$> (fromCStructImageSubresourceLayers (vkSrcSubresource (c :: VkImageResolve)))
                                         <*> (fromCStructOffset3D (vkSrcOffset (c :: VkImageResolve)))
                                         <*> (fromCStructImageSubresourceLayers (vkDstSubresource (c :: VkImageResolve)))
                                         <*> (fromCStructOffset3D (vkDstOffset (c :: VkImageResolve)))
                                         <*> (fromCStructExtent3D (vkExtent (c :: VkImageResolve)))

instance Zero ImageResolve where
  zero = ImageResolve zero
                      zero
                      zero
                      zero
                      zero



-- | VkImageSubresourceLayers - Structure specifying an image subresource
-- layers
--
-- == Valid Usage
--
-- -   If @aspectMask@ contains
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
--     it /must/ not contain either of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
--
-- -   @aspectMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_METADATA_BIT'
--
-- -   @layerCount@ /must/ be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageBlit',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve'
data ImageSubresourceLayers = ImageSubresourceLayers
  { -- No documentation found for Nested "ImageSubresourceLayers" "aspectMask"
  aspectMask :: ImageAspectFlags
  , -- No documentation found for Nested "ImageSubresourceLayers" "mipLevel"
  mipLevel :: Word32
  , -- No documentation found for Nested "ImageSubresourceLayers" "baseArrayLayer"
  baseArrayLayer :: Word32
  , -- No documentation found for Nested "ImageSubresourceLayers" "layerCount"
  layerCount :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageSubresourceLayers' and
-- marshal a 'ImageSubresourceLayers' into it. The 'VkImageSubresourceLayers' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageSubresourceLayers :: ImageSubresourceLayers -> (VkImageSubresourceLayers -> IO a) -> IO a
withCStructImageSubresourceLayers marshalled cont = cont (VkImageSubresourceLayers (aspectMask (marshalled :: ImageSubresourceLayers)) (mipLevel (marshalled :: ImageSubresourceLayers)) (baseArrayLayer (marshalled :: ImageSubresourceLayers)) (layerCount (marshalled :: ImageSubresourceLayers)))

-- | A function to read a 'VkImageSubresourceLayers' and all additional
-- structures in the pointer chain into a 'ImageSubresourceLayers'.
fromCStructImageSubresourceLayers :: VkImageSubresourceLayers -> IO ImageSubresourceLayers
fromCStructImageSubresourceLayers c = ImageSubresourceLayers <$> pure (vkAspectMask (c :: VkImageSubresourceLayers))
                                                             <*> pure (vkMipLevel (c :: VkImageSubresourceLayers))
                                                             <*> pure (vkBaseArrayLayer (c :: VkImageSubresourceLayers))
                                                             <*> pure (vkLayerCount (c :: VkImageSubresourceLayers))

instance Zero ImageSubresourceLayers where
  zero = ImageSubresourceLayers zero
                                zero
                                zero
                                zero


-- | VkIndexType - Type of index buffer indices
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryTrianglesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'
type IndexType = VkIndexType


{-# complete INDEX_TYPE_UINT16, INDEX_TYPE_UINT32, INDEX_TYPE_NONE_NV :: IndexType #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT16'
-- specifies that indices are 16-bit unsigned integer values.
pattern INDEX_TYPE_UINT16 :: (a ~ IndexType) => a
pattern INDEX_TYPE_UINT16 = VK_INDEX_TYPE_UINT16


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT32'
-- specifies that indices are 32-bit unsigned integer values.
pattern INDEX_TYPE_UINT32 :: (a ~ IndexType) => a
pattern INDEX_TYPE_UINT32 = VK_INDEX_TYPE_UINT32


-- No documentation found for Nested "IndexType" "INDEX_TYPE_NONE_NV"
pattern INDEX_TYPE_NONE_NV :: (a ~ IndexType) => a
pattern INDEX_TYPE_NONE_NV = VK_INDEX_TYPE_NONE_NV


-- | VkMemoryBarrier - Structure specifying a global memory barrier
--
-- = Description
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
data MemoryBarrier = MemoryBarrier
  { -- Univalued member elided
  -- No documentation found for Nested "MemoryBarrier" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MemoryBarrier" "srcAccessMask"
  srcAccessMask :: AccessFlags
  , -- No documentation found for Nested "MemoryBarrier" "dstAccessMask"
  dstAccessMask :: AccessFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryBarrier' and
-- marshal a 'MemoryBarrier' into it. The 'VkMemoryBarrier' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryBarrier :: MemoryBarrier -> (VkMemoryBarrier -> IO a) -> IO a
withCStructMemoryBarrier marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MemoryBarrier)) (\pPNext -> cont (VkMemoryBarrier VK_STRUCTURE_TYPE_MEMORY_BARRIER pPNext (srcAccessMask (marshalled :: MemoryBarrier)) (dstAccessMask (marshalled :: MemoryBarrier))))

-- | A function to read a 'VkMemoryBarrier' and all additional
-- structures in the pointer chain into a 'MemoryBarrier'.
fromCStructMemoryBarrier :: VkMemoryBarrier -> IO MemoryBarrier
fromCStructMemoryBarrier c = MemoryBarrier <$> -- Univalued Member elided
                                           maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMemoryBarrier)))
                                           <*> pure (vkSrcAccessMask (c :: VkMemoryBarrier))
                                           <*> pure (vkDstAccessMask (c :: VkMemoryBarrier))

instance Zero MemoryBarrier where
  zero = MemoryBarrier Nothing
                       zero
                       zero



-- | VkRenderPassBeginInfo - Structure specifying render pass begin info
--
-- = Description
--
-- @renderArea@ is the render area that is affected by the render pass
-- instance. The effects of attachment load, store and multisample resolve
-- operations are restricted to the pixels whose x and y coordinates fall
-- within the render area on all attachments. The render area extends to
-- all layers of @framebuffer@. The application /must/ ensure (using
-- scissor if necessary) that all rendering is contained within the render
-- area. The render area /must/ be contained within the framebuffer
-- dimensions.
--
-- __Note__
--
-- There /may/ be a performance cost for using a render area smaller than
-- the framebuffer, unless it matches the render area granularity for the
-- render pass.
--
-- == Valid Usage
--
-- -   @clearValueCount@ /must/ be greater than the largest attachment
--     index in @renderPass@ that specifies a @loadOp@ (or @stencilLoadOp@,
--     if the attachment has a depth\/stencil format) of
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_LOAD_OP_CLEAR'
--
-- -   @renderPass@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo' structure
--     specified when creating @framebuffer@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo'
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkRenderPassSampleLocationsBeginInfoEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @renderPass@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass' handle
--
-- -   @framebuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' handle
--
-- -   If @clearValueCount@ is not @0@, @pClearValues@ /must/ be a valid
--     pointer to an array of @clearValueCount@
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearValue' unions
--
-- -   Both of @framebuffer@, and @renderPass@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearValue',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCmdBeginRenderPass2KHR'
data RenderPassBeginInfo = RenderPassBeginInfo
  { -- Univalued member elided
  -- No documentation found for Nested "RenderPassBeginInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "RenderPassBeginInfo" "renderPass"
  renderPass :: RenderPass
  , -- No documentation found for Nested "RenderPassBeginInfo" "framebuffer"
  framebuffer :: Framebuffer
  , -- No documentation found for Nested "RenderPassBeginInfo" "renderArea"
  renderArea :: Rect2D
  -- Length valued member elided
  , -- No documentation found for Nested "RenderPassBeginInfo" "pClearValues"
  clearValues :: Vector ClearValue
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkRenderPassBeginInfo' and
-- marshal a 'RenderPassBeginInfo' into it. The 'VkRenderPassBeginInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructRenderPassBeginInfo :: RenderPassBeginInfo -> (VkRenderPassBeginInfo -> IO a) -> IO a
withCStructRenderPassBeginInfo marshalled cont = withVec withCStructClearValue (clearValues (marshalled :: RenderPassBeginInfo)) (\pPClearValues -> withCStructRect2D (renderArea (marshalled :: RenderPassBeginInfo)) (\renderArea'' -> maybeWith withSomeVkStruct (next (marshalled :: RenderPassBeginInfo)) (\pPNext -> cont (VkRenderPassBeginInfo VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO pPNext (renderPass (marshalled :: RenderPassBeginInfo)) (framebuffer (marshalled :: RenderPassBeginInfo)) renderArea'' (fromIntegral (Data.Vector.length (clearValues (marshalled :: RenderPassBeginInfo)))) pPClearValues))))

-- No fromCStruct function for types containing unions

instance Zero RenderPassBeginInfo where
  zero = RenderPassBeginInfo Nothing
                             zero
                             zero
                             zero
                             Data.Vector.empty


-- | VkStencilFaceFlagBits - Bitmask specifying sets of stencil state for
-- which to update the compare mask
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags'
type StencilFaceFlagBits = VkStencilFaceFlagBits


{-# complete STENCIL_FACE_FRONT_BIT, STENCIL_FACE_BACK_BIT, STENCIL_FRONT_AND_BACK :: StencilFaceFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_STENCIL_FACE_FRONT_BIT'
-- specifies that only the front set of stencil state is updated.
pattern STENCIL_FACE_FRONT_BIT :: (a ~ StencilFaceFlagBits) => a
pattern STENCIL_FACE_FRONT_BIT = VK_STENCIL_FACE_FRONT_BIT


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_STENCIL_FACE_BACK_BIT'
-- specifies that only the back set of stencil state is updated.
pattern STENCIL_FACE_BACK_BIT :: (a ~ StencilFaceFlagBits) => a
pattern STENCIL_FACE_BACK_BIT = VK_STENCIL_FACE_BACK_BIT


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_STENCIL_FRONT_AND_BACK'
-- is the combination of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_STENCIL_FACE_FRONT_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_STENCIL_FACE_BACK_BIT',
-- and specifies that both sets of stencil state are updated.
pattern STENCIL_FRONT_AND_BACK :: (a ~ StencilFaceFlagBits) => a
pattern STENCIL_FRONT_AND_BACK = VK_STENCIL_FRONT_AND_BACK

-- | VkStencilFaceFlags - Bitmask of VkStencilFaceFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilReference',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilWriteMask'
type StencilFaceFlags = StencilFaceFlagBits

-- | VkSubpassContents - Specify how commands in the first subpass of a
-- render pass are provided
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkSubpassBeginInfoKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass'
type SubpassContents = VkSubpassContents


{-# complete SUBPASS_CONTENTS_INLINE, SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: SubpassContents #-}


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_SUBPASS_CONTENTS_INLINE'
-- specifies that the contents of the subpass will be recorded inline in
-- the primary command buffer, and secondary command buffers /must/ not be
-- executed within the subpass.
pattern SUBPASS_CONTENTS_INLINE :: (a ~ SubpassContents) => a
pattern SUBPASS_CONTENTS_INLINE = VK_SUBPASS_CONTENTS_INLINE


-- | 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS'
-- specifies that the contents are recorded in secondary command buffers
-- that will be called from the primary command buffer, and
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands' is
-- the only valid command on the command buffer until
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass' or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndRenderPass'.
pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: (a ~ SubpassContents) => a
pattern SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS


-- | vkCmdBeginQuery - Begin a query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that will manage the results of the
--     query.
--
-- -   @query@ is the query index within the query pool that will contain
--     the results.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'
--     specifying constraints on the types of queries that /can/ be
--     performed.
--
-- = Description
--
-- If the @queryType@ of the pool is
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION' and @flags@
-- contains
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT',
-- an implementation /must/ return a result that matches the actual number
-- of samples passed. This is described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-occlusion Occlusion Queries>.
--
-- After beginning a query, that query is considered /active/ within the
-- command buffer it was called in until that same query is ended. Queries
-- active in a primary command buffer when secondary command buffers are
-- executed are considered active for those secondary command buffers.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ that differs
--     from that of any queries that are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--     within @commandBuffer@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-occlusionQueryPrecise precise occlusion queries>
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT'
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION', the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate graphics operations,
--     the 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     and any of the @pipelineStatistics@ indicate compute operations, the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'
--     values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
cmdBeginQuery :: CommandBuffer ->  QueryPool ->  Word32 ->  QueryControlFlags ->  IO ()
cmdBeginQuery = \(CommandBuffer commandBuffer' commandTable) -> \queryPool' -> \query' -> \flags' -> vkCmdBeginQuery commandTable commandBuffer' queryPool' query' flags' *> (pure ())


-- | vkCmdBeginRenderPass - Begin a new render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @pRenderPassBegin@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'
--     structure (defined below) which specifies the render pass to begin
--     an instance of, and the framebuffer the instance uses.
--
-- -   @contents@ is a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkSubpassContents'
--     value specifying how the commands in the first subpass will be
--     provided.
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record the commands for the first subpass of that render pass.
--
-- == Valid Usage
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL',
--     or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     or the @layout@ member of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentReference' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     then the corresponding attachment image view of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with a @usage@ value including
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--
-- -   If any of the @initialLayout@ members of the
--     'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription' structures
--     specified when creating the render pass specified in the
--     @renderPass@ member of @pRenderPassBegin@ is not
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_UNDEFINED', then
--     each such @initialLayout@ /must/ be equal to the current layout of
--     the corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   The @srcStageMask@ and @dstStageMask@ members of any element of the
--     @pDependencies@ member of
--     'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo' used to
--     create @renderPass@ /must/ be supported by the capabilities of the
--     queue family identified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo' used
--     to create the command pool which @commandBuffer@ was allocated from
--
-- -   For any attachment in @framebuffer@ that is used by @renderPass@ and
--     is bound to memory locations that are also bound to another
--     attachment used by @renderPass@, and if at least one of those uses
--     causes either attachment to be written to, both attachments /must/
--     have had the
--     'Graphics.Vulkan.C.Core10.Pass.VK_ATTACHMENT_DESCRIPTION_MAY_ALIAS_BIT'
--     set
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pRenderPassBegin@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo'
--     structure
--
-- -   @contents@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkSubpassContents'
--     value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a primary
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Graphics        |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkSubpassContents'
cmdBeginRenderPass :: CommandBuffer ->  RenderPassBeginInfo ->  SubpassContents ->  IO ()
cmdBeginRenderPass = \(CommandBuffer commandBuffer' commandTable) -> \renderPassBegin' -> \contents' -> (\marshalled -> withCStructRenderPassBeginInfo marshalled . flip with) renderPassBegin' (\pRenderPassBegin' -> vkCmdBeginRenderPass commandTable commandBuffer' pRenderPassBegin' contents' *> (pure ()))


-- | vkCmdBindDescriptorSets - Binds descriptor sets to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the descriptor sets will
--     be bound to.
--
-- -   @pipelineBindPoint@ is a
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' indicating
--     whether the descriptors will be used by graphics pipelines or
--     compute pipelines. There is a separate set of bind points for each
--     of graphics and compute, so binding one does not disturb the other.
--
-- -   @layout@ is a 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
--     object used to program the bindings.
--
-- -   @firstSet@ is the set number of the first descriptor set to be
--     bound.
--
-- -   @descriptorSetCount@ is the number of elements in the
--     @pDescriptorSets@ array.
--
-- -   @pDescriptorSets@ is an array of handles to
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' objects
--     describing the descriptor sets to write to.
--
-- -   @dynamicOffsetCount@ is the number of dynamic offsets in the
--     @pDynamicOffsets@ array.
--
-- -   @pDynamicOffsets@ is a pointer to an array of @uint32_t@ values
--     specifying dynamic offsets.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets'
-- causes the sets numbered [@firstSet@..
-- @firstSet@+@descriptorSetCount@-1] to use the bindings stored in
-- @pDescriptorSets@[0..@descriptorSetCount@-1] for subsequent rendering
-- commands (either compute or graphics, according to the
-- @pipelineBindPoint@). Any bindings that were previously applied via
-- these sets are no longer valid.
--
-- Once bound, a descriptor set affects rendering of subsequent graphics or
-- compute commands in the command buffer until a different set is bound to
-- the same set number, or else until the set is disturbed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>.
--
-- A compatible descriptor set /must/ be bound for all set numbers that any
-- shaders in a pipeline access, at the time that a draw or dispatch
-- command is recorded to execute using that pipeline. However, if none of
-- the shaders in a pipeline statically use any bindings with a particular
-- set number, then no descriptor set need be bound for that set number,
-- even if the pipeline layout includes a non-trivial descriptor set layout
-- for that set number.
--
-- If any of the sets being bound include dynamic uniform or storage
-- buffers, then @pDynamicOffsets@ includes one element for each array
-- element in each dynamic descriptor type binding in each set. Values are
-- taken from @pDynamicOffsets@ in an order such that all entries for set N
-- come before set N+1; within a set, entries are ordered by the binding
-- numbers in the descriptor set layouts; and within a binding array,
-- elements are in order. @dynamicOffsetCount@ /must/ equal the total
-- number of dynamic descriptors in the sets being bound.
--
-- The effective offset used for dynamic uniform and storage buffer
-- bindings is the sum of the relative offset taken from @pDynamicOffsets@,
-- and the base address of the buffer plus base offset in the descriptor
-- set. The range of the dynamic uniform and storage buffer bindings is the
-- buffer range as specified in the descriptor set.
--
-- Each of the @pDescriptorSets@ /must/ be compatible with the pipeline
-- layout specified by @layout@. The layout used to program the bindings
-- /must/ also be compatible with the pipeline used in subsequent graphics
-- or compute commands, as defined in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-compatibility Pipeline Layout Compatibility>
-- section.
--
-- The descriptor set contents bound by a call to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets'
-- /may/ be consumed at the following times:
--
-- -   during host execution of the command, or during shader execution of
--     the resulting draws and dispatches, or any time in between.
--
-- Thus, the contents of a descriptor set binding /must/ not be altered
-- (overwritten by an update command, or freed) between the first point in
-- time that it /may/ be consumed, and when the command completes executing
-- on the queue.
--
-- The contents of @pDynamicOffsets@ are consumed immediately during
-- execution of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets'.
-- Once all pending uses have completed, it is legal to update and reuse a
-- descriptor set.
--
-- == Valid Usage
--
-- -   Each element of @pDescriptorSets@ /must/ have been allocated with a
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout' that
--     matches (is the same as, or identically defined as) the
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkDescriptorSetLayout' at
--     set /n/ in @layout@, where /n/ is the sum of @firstSet@ and the
--     index into @pDescriptorSets@
--
-- -   @dynamicOffsetCount@ /must/ be equal to the total number of dynamic
--     descriptors in @pDescriptorSets@
--
-- -   The sum of @firstSet@ and @descriptorSetCount@ /must/ be less than
--     or equal to
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'::@setLayoutCount@
--     provided when @layout@ was created
--
-- -   @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool'’s queue
--     family
--
-- -   Each element of @pDynamicOffsets@ which corresponds to a descriptor
--     binding with type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minUniformBufferOffsetAlignment@
--
-- -   Each element of @pDynamicOffsets@ which corresponds to a descriptor
--     binding with type
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
--     /must/ be a multiple of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@minStorageBufferOffsetAlignment@
--
-- -   For each dynamic uniform or storage buffer binding in
--     @pDescriptorSets@, the sum of the effective offset, as defined
--     above, and the range of the binding /must/ be less than or equal to
--     the size of the buffer
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' handles
--
-- -   If @dynamicOffsetCount@ is not @0@, @pDynamicOffsets@ /must/ be a
--     valid pointer to an array of @dynamicOffsetCount@ @uint32_t@ values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @layout@, and the elements of
--     @pDescriptorSets@ /must/ have been created, allocated, or retrieved
--     from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout'
cmdBindDescriptorSets :: CommandBuffer ->  PipelineBindPoint ->  PipelineLayout ->  Word32 ->  Vector DescriptorSet ->  Vector Word32 ->  IO ()
cmdBindDescriptorSets = \(CommandBuffer commandBuffer' commandTable) -> \pipelineBindPoint' -> \layout' -> \firstSet' -> \descriptorSets' -> \dynamicOffsets' -> withVec (&) dynamicOffsets' (\pDynamicOffsets' -> withVec (&) descriptorSets' (\pDescriptorSets' -> vkCmdBindDescriptorSets commandTable commandBuffer' pipelineBindPoint' layout' firstSet' (fromIntegral $ Data.Vector.length descriptorSets') pDescriptorSets' (fromIntegral $ Data.Vector.length dynamicOffsets') pDynamicOffsets' *> (pure ())))


-- | vkCmdBindIndexBuffer - Bind an index buffer to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer being bound.
--
-- -   @offset@ is the starting offset in bytes within @buffer@ used in
--     index buffer address calculations.
--
-- -   @indexType@ is a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' value
--     specifying whether indices are treated as 16 bits or 32 bits.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   The sum of @offset@ and the address of the range of
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object that is
--     backing @buffer@, /must/ be a multiple of the type indicated by
--     @indexType@
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDEX_BUFFER_BIT'
--     flag
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @indexType@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType'
cmdBindIndexBuffer :: CommandBuffer ->  Buffer ->  DeviceSize ->  IndexType ->  IO ()
cmdBindIndexBuffer = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \indexType' -> vkCmdBindIndexBuffer commandTable commandBuffer' buffer' offset' indexType' *> (pure ())


-- | vkCmdBindPipeline - Bind a pipeline object to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the pipeline will be
--     bound to.
--
-- -   @pipelineBindPoint@ is a
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' value specifying
--     whether to bind to the compute or graphics bind point. Binding one
--     does not disturb the other.
--
-- -   @pipeline@ is the pipeline to be bound.
--
-- = Description
--
-- Once bound, a pipeline binding affects subsequent graphics or compute
-- commands in the command buffer until a different pipeline is bound to
-- the bind point. The pipeline bound to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE' controls
-- the behavior of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch' and
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect'.
-- The pipeline bound to
-- 'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS' controls
-- the behavior of all
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing drawing commands>.
-- No other commands are affected by the pipeline state.
--
-- == Valid Usage
--
-- -   If @pipelineBindPoint@ is
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE', the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   If @pipelineBindPoint@ is
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS', the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   If @pipelineBindPoint@ is
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_COMPUTE',
--     @pipeline@ /must/ be a compute pipeline
--
-- -   If @pipelineBindPoint@ is
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS',
--     @pipeline@ /must/ be a graphics pipeline
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-variableMultisampleRate variable multisample rate>
--     feature is not supported, @pipeline@ is a graphics pipeline, the
--     current subpass has no attachments, and this is not the first call
--     to this function with a graphics pipeline after transitioning to the
--     current subpass, then the sample count specified by this pipeline
--     /must/ match that set in the previous pipeline
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @pipeline@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   Both of @commandBuffer@, and @pipeline@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.C.Core10.Pass.VkPipelineBindPoint'
cmdBindPipeline :: CommandBuffer ->  PipelineBindPoint ->  Pipeline ->  IO ()
cmdBindPipeline = \(CommandBuffer commandBuffer' commandTable) -> \pipelineBindPoint' -> \pipeline' -> vkCmdBindPipeline commandTable commandBuffer' pipelineBindPoint' pipeline' *> (pure ())


-- | vkCmdBindVertexBuffers - Bind vertex buffers to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @firstBinding@ is the index of the first vertex input binding whose
--     state is updated by the command.
--
-- -   @bindingCount@ is the number of vertex input bindings whose state is
--     updated by the command.
--
-- -   @pBuffers@ is a pointer to an array of buffer handles.
--
-- -   @pOffsets@ is a pointer to an array of buffer offsets.
--
-- = Description
--
-- The values taken from elements i of @pBuffers@ and @pOffsets@ replace
-- the current state for the vertex input binding @firstBinding@ + i, for i
-- in [0, @bindingCount@). The vertex input binding is updated to start at
-- the offset indicated by @pOffsets@[i] from the start of the buffer
-- @pBuffers@[i]. All vertex input attributes that use each of these
-- bindings will use these updated addresses in their address calculations
-- for subsequent draw commands.
--
-- == Valid Usage
--
-- -   @firstBinding@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxVertexInputBindings@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_VERTEX_BUFFER_BIT'
--     flag
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handles
--
-- -   @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   @bindingCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pBuffers@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdBindVertexBuffers :: CommandBuffer ->  Word32 ->  Vector Buffer ->  Vector DeviceSize ->  IO ()
cmdBindVertexBuffers = \(CommandBuffer commandBuffer' commandTable) -> \firstBinding' -> \buffers' -> \offsets' -> withVec (&) offsets' (\pOffsets' -> withVec (&) buffers' (\pBuffers' -> vkCmdBindVertexBuffers commandTable commandBuffer' firstBinding' (fromIntegral $ Data.Vector.length buffers' `min` Data.Vector.length offsets') pBuffers' pOffsets' *> (pure ())))


-- | vkCmdBlitImage - Copy regions of an image, potentially performing format
-- conversion,
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the layout of the source image subresources for
--     the blit.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the layout of the destination image subresources
--     for the blit.
--
-- -   @regionCount@ is the number of regions to blit.
--
-- -   @pRegions@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageBlit'
--     structures specifying the regions to blit.
--
-- -   @filter@ is a 'Graphics.Vulkan.C.Core10.Sampler.VkFilter' specifying
--     the filter to apply if the blits require scaling.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' /must/
-- not be used for multisampled source or destination images. Use
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResolveImage' for
-- this purpose.
--
-- As the sizes of the source and destination extents /can/ differ in any
-- dimension, texels in the source extent are scaled and filtered to the
-- destination extent. Scaling occurs via the following operations:
--
-- -   For each destination texel, the integer coordinate of that texel is
--     converted to an unnormalized texture coordinate, using the effective
--     inverse of the equations described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures-unnormalized-to-integer unnormalized to integer conversion>:
--
--     -   ubase = i + ½
--
--     -   vbase = j + ½
--
--     -   wbase = k + ½
--
-- -   These base coordinates are then offset by the first destination
--     offset:
--
--     -   uoffset = ubase - xdst0
--
--     -   voffset = vbase - ydst0
--
--     -   woffset = wbase - zdst0
--
--     -   aoffset = a - @baseArrayCount@dst
--
-- -   The scale is determined from the source and destination regions, and
--     applied to the offset coordinates:
--
--     -   scale_u = (xsrc1 - xsrc0) \/ (xdst1 - xdst0)
--
--     -   scale_v = (ysrc1 - ysrc0) \/ (ydst1 - ydst0)
--
--     -   scale_w = (zsrc1 - zsrc0) \/ (zdst1 - zdst0)
--
--     -   uscaled = uoffset * scaleu
--
--     -   vscaled = voffset * scalev
--
--     -   wscaled = woffset * scalew
--
-- -   Finally the source offset is added to the scaled coordinates, to
--     determine the final unnormalized coordinates used to sample from
--     @srcImage@:
--
--     -   u = uscaled + xsrc0
--
--     -   v = vscaled + ysrc0
--
--     -   w = wscaled + zsrc0
--
--     -   q = @mipLevel@
--
--     -   a = aoffset + @baseArrayCount@src
--
-- These coordinates are used to sample from the source image, as described
-- in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#textures Image Operations chapter>,
-- with the filter mode equal to that of @filter@, a mipmap mode of
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_NEAREST' and an
-- address mode of
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'.
-- Implementations /must/ clamp at the edge of the source image, and /may/
-- additionally clamp to the edge of the source region.
--
-- __Note__
--
-- Due to allowable rounding errors in the generation of the source texture
-- coordinates, it is not always possible to guarantee exactly which source
-- texels will be sampled for a given blit. As rounding errors are
-- implementation dependent, the exact results of a blitting operation are
-- also implementation dependent.
--
-- Blits are done layer by layer starting with the @baseArrayLayer@ member
-- of @srcSubresource@ for the source and @dstSubresource@ for the
-- destination. @layerCount@ layers are blitted to the destination image.
--
-- 3D textures are blitted slice by slice. Slices in the source region
-- bounded by @srcOffsets@[0].@z@ and @srcOffsets@[1].@z@ are copied to
-- slices in the destination region bounded by @dstOffsets@[0].@z@ and
-- @dstOffsets@[1].@z@. For each destination slice, a source __z__
-- coordinate is linearly interpolated between @srcOffsets@[0].@z@ and
-- @srcOffsets@[1].@z@. If the @filter@ parameter is
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' then the value
-- sampled from the source image is taken by doing linear filtering using
-- the interpolated __z__ coordinate. If @filter@ parameter is
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST' then the value
-- sampled from the source image is taken from the single nearest slice,
-- with an implementation-dependent arithmetic rounding mode.
--
-- The following filtering and conversion rules apply:
--
-- -   Integer formats /can/ only be converted to other integer formats
--     with the same signedness.
--
-- -   No format conversion is supported between depth\/stencil images. The
--     formats /must/ match.
--
-- -   Format conversions on unorm, snorm, unscaled and packed float
--     formats of the copied aspect of the image are performed by first
--     converting the pixels to float values.
--
-- -   For sRGB source formats, nonlinear RGB values are converted to
--     linear representation prior to filtering.
--
-- -   After filtering, the float values are first clamped and then cast to
--     the destination image format. In case of sRGB destination format,
--     linear RGB values are converted to nonlinear representation before
--     writing the pixel to the image.
--
-- Signed and unsigned integers are converted by first clamping to the
-- representable range of the destination format, then casting the value.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@
--
-- -   The union of all destination regions, specified by the elements of
--     @pRegions@, /must/ not overlap in memory with any texel that /may/
--     be sampled during the blit operation
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'.
--
-- -   @srcImage@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_DST_BIT'.
--
-- -   @dstImage@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The sample count of @srcImage@ and @dstImage@ /must/ both be equal
--     to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If either of @srcImage@ or @dstImage@ was created with a signed
--     integer 'Graphics.Vulkan.C.Core10.Core.VkFormat', the other /must/
--     also have been created with a signed integer
--     'Graphics.Vulkan.C.Core10.Core.VkFormat'
--
-- -   If either of @srcImage@ or @dstImage@ was created with an unsigned
--     integer 'Graphics.Vulkan.C.Core10.Core.VkFormat', the other /must/
--     also have been created with an unsigned integer
--     'Graphics.Vulkan.C.Core10.Core.VkFormat'
--
-- -   If either of @srcImage@ or @dstImage@ was created with a
--     depth\/stencil format, the other /must/ have exactly the same format
--
-- -   If @srcImage@ was created with a depth\/stencil format, @filter@
--     /must/ be 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_NEAREST'
--
-- -   @srcImage@ /must/ have been created with a @samples@ value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   @dstImage@ /must/ have been created with a @samples@ value of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @filter@ is 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR',
--     then the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @srcImage@ /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'.
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @dstImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageBlit'
--     structures
--
-- -   @filter@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Sampler.VkFilter' value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcImage@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageBlit',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout'
cmdBlitImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageBlit ->  Filter ->  IO ()
cmdBlitImage = \(CommandBuffer commandBuffer' commandTable) -> \srcImage' -> \srcImageLayout' -> \dstImage' -> \dstImageLayout' -> \regions' -> \filter' -> withVec withCStructImageBlit regions' (\pRegions' -> vkCmdBlitImage commandTable commandBuffer' srcImage' srcImageLayout' dstImage' dstImageLayout' (fromIntegral $ Data.Vector.length regions') pRegions' filter' *> (pure ()))


-- | vkCmdClearAttachments - Clear regions within bound framebuffer
-- attachments
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @attachmentCount@ is the number of entries in the @pAttachments@
--     array.
--
-- -   @pAttachments@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearAttachment'
--     structures defining the attachments to clear and the clear values to
--     use. If any attachment to be cleared in the current subpass is
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', then the
--     clear has no effect on that attachment.
--
-- -   @rectCount@ is the number of entries in the @pRects@ array.
--
-- -   @pRects@ points to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearRect'
--     structures defining regions within each selected attachment to
--     clear.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'
-- /can/ clear multiple regions of each attachment used in the current
-- subpass of a render pass instance. This command /must/ be called only
-- inside a render pass instance, and implicitly selects the images to
-- clear based on the current framebuffer attachments and the command
-- parameters.
--
-- Unlike other
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears clear commands>,
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'
-- executes as a drawing command, rather than a transfer command, with
-- writes performed by it executing in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primrast-order rasterization order>.
-- Clears to color attachments are executed as color attachment writes, by
-- the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- stage. Clears to depth\/stencil attachments are executed as
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-depth depth writes>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fragops-stencil writes>
-- by the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- stages.
--
-- == Valid Usage
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT',
--     then the @colorAttachment@ member of that element /must/ either
--     refer to a color attachment which is
--     'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', or /must/
--     be a valid color attachment.
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT',
--     then the current subpass\' depth\/stencil attachment /must/ either
--     be 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', or
--     /must/ have a depth component
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT',
--     then the current subpass\' depth\/stencil attachment /must/ either
--     be 'Graphics.Vulkan.C.Core10.Constants.VK_ATTACHMENT_UNUSED', or
--     /must/ have a stencil component
--
-- -   The @rect@ member of each element of @pRects@ /must/ have an
--     @extent.width@ greater than @0@
--
-- -   The @rect@ member of each element of @pRects@ /must/ have an
--     @extent.height@ greater than @0@
--
-- -   The rectangular region specified by each element of @pRects@ /must/
--     be contained within the render area of the current render pass
--     instance
--
-- -   The layers specified by each element of @pRects@ /must/ be contained
--     within every attachment that @pAttachments@ refers to
--
-- -   The @layerCount@ member of each element of @pRects@ /must/ not be
--     @0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pAttachments@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearAttachment'
--     structures
--
-- -   @pRects@ /must/ be a valid pointer to an array of @rectCount@
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearRect'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @attachmentCount@ /must/ be greater than @0@
--
-- -   @rectCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearAttachment',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearRect',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdClearAttachments :: CommandBuffer ->  Vector ClearAttachment ->  Vector ClearRect ->  IO ()
cmdClearAttachments = \(CommandBuffer commandBuffer' commandTable) -> \attachments' -> \rects' -> withVec withCStructClearRect rects' (\pRects' -> withVec withCStructClearAttachment attachments' (\pAttachments' -> vkCmdClearAttachments commandTable commandBuffer' (fromIntegral $ Data.Vector.length attachments') pAttachments' (fromIntegral $ Data.Vector.length rects') pRects' *> (pure ())))


-- | vkCmdClearColorImage - Clear regions of a color image
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @image@ is the image to be cleared.
--
-- -   @imageLayout@ specifies the current layout of the image subresource
--     ranges to be cleared, and /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'.
--
-- -   @pColor@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearColorValue'
--     structure that contains the values the image subresource ranges will
--     be cleared to (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears-values>
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ points to an array of
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
--     structures that describe a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views Image Views>.
--
-- = Description
--
-- Each specified range in @pRanges@ is cleared to the value specified by
-- @pColor@.
--
-- == Valid Usage
--
-- -   @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @imageLayout@ /must/ specify the layout of the image subresource
--     ranges of @image@ specified in @pRanges@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @imageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@aspectMask@
--     members of the elements of the @pRanges@ array /must/ each only
--     include
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_COLOR_BIT'
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange' element
--     of @pRanges@, if the @levelCount@ member is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_MIP_LEVELS', then
--     @baseMipLevel@ + @levelCount@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when
--     @image@ was created
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange' element
--     of @pRanges@, if the @layerCount@ member is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS', then
--     @baseArrayLayer@ + @layerCount@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   @image@ /must/ not have a compressed or depth\/stencil format
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @imageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @pColor@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearColorValue'
--     union
--
-- -   @pRanges@ /must/ be a valid pointer to an array of @rangeCount@
--     valid 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @rangeCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @image@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearColorValue',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
cmdClearColorImage :: CommandBuffer ->  Image ->  ImageLayout ->  ClearColorValue ->  Vector ImageSubresourceRange ->  IO ()
cmdClearColorImage = \(CommandBuffer commandBuffer' commandTable) -> \image' -> \imageLayout' -> \color' -> \ranges' -> withVec withCStructImageSubresourceRange ranges' (\pRanges' -> (\marshalled -> withCStructClearColorValue marshalled . flip with) color' (\pColor' -> vkCmdClearColorImage commandTable commandBuffer' image' imageLayout' pColor' (fromIntegral $ Data.Vector.length ranges') pRanges' *> (pure ())))


-- | vkCmdClearDepthStencilImage - Fill regions of a combined depth\/stencil
-- image
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @image@ is the image to be cleared.
--
-- -   @imageLayout@ specifies the current layout of the image subresource
--     ranges to be cleared, and /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL' or
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'.
--
-- -   @pDepthStencil@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearDepthStencilValue'
--     structure that contains the values the depth and stencil image
--     subresource ranges will be cleared to (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears-values>
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ points to an array of
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
--     structures that describe a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-views Image Views>.
--
-- == Valid Usage
--
-- -   @image@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @imageLayout@ /must/ specify the layout of the image subresource
--     ranges of @image@ specified in @pRanges@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @imageLayout@ /must/ be either of
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@aspectMask@
--     members of the elements of the @pRanges@ array /must/ each only
--     include
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_DEPTH_BIT'
--     if the image format has a depth component
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@aspectMask@
--     members of the elements of the @pRanges@ array /must/ each only
--     include
--     'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_IMAGE_ASPECT_STENCIL_BIT'
--     if the image format has a stencil component
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange' element
--     of @pRanges@, if the @levelCount@ member is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_MIP_LEVELS', then
--     @baseMipLevel@ + @levelCount@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when
--     @image@ was created
--
-- -   The
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange' element
--     of @pRanges@, if the @layerCount@ member is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_REMAINING_ARRAY_LAYERS', then
--     @baseArrayLayer@ + @layerCount@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   @image@ /must/ have a depth\/stencil format
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @image@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @imageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @pDepthStencil@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearDepthStencilValue'
--     structure
--
-- -   @pRanges@ /must/ be a valid pointer to an array of @rangeCount@
--     valid 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @rangeCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @image@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkClearDepthStencilValue',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageSubresourceRange'
cmdClearDepthStencilImage :: CommandBuffer ->  Image ->  ImageLayout ->  ClearDepthStencilValue ->  Vector ImageSubresourceRange ->  IO ()
cmdClearDepthStencilImage = \(CommandBuffer commandBuffer' commandTable) -> \image' -> \imageLayout' -> \depthStencil' -> \ranges' -> withVec withCStructImageSubresourceRange ranges' (\pRanges' -> (\marshalled -> withCStructClearDepthStencilValue marshalled . flip with) depthStencil' (\pDepthStencil' -> vkCmdClearDepthStencilImage commandTable commandBuffer' image' imageLayout' pDepthStencil' (fromIntegral $ Data.Vector.length ranges') pRanges' *> (pure ())))


-- | vkCmdCopyBuffer - Copy data between buffer regions
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @srcBuffer@ is the source buffer.
--
-- -   @dstBuffer@ is the destination buffer.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferCopy'
--     structures specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the source buffer to the same
-- region of the destination buffer. @srcBuffer@ and @dstBuffer@ /can/ be
-- the same buffer or alias the same memory, but the resulting values are
-- undefined if the copy regions overlap in memory.
--
-- == Valid Usage
--
-- -   The @srcOffset@ member of each element of @pRegions@ /must/ be less
--     than the size of @srcBuffer@
--
-- -   The @dstOffset@ member of each element of @pRegions@ /must/ be less
--     than the size of @dstBuffer@
--
-- -   The @size@ member of each element of @pRegions@ /must/ be less than
--     or equal to the size of @srcBuffer@ minus @srcOffset@
--
-- -   The @size@ member of each element of @pRegions@ /must/ be less than
--     or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   The union of the source regions, and the union of the destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferCopy'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstBuffer@, and @srcBuffer@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferCopy',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdCopyBuffer :: CommandBuffer ->  Buffer ->  Buffer ->  Vector BufferCopy ->  IO ()
cmdCopyBuffer = \(CommandBuffer commandBuffer' commandTable) -> \srcBuffer' -> \dstBuffer' -> \regions' -> withVec withCStructBufferCopy regions' (\pRegions' -> vkCmdCopyBuffer commandTable commandBuffer' srcBuffer' dstBuffer' (fromIntegral $ Data.Vector.length regions') pRegions' *> (pure ()))


-- | vkCmdCopyBufferToImage - Copy data from a buffer into an image
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @srcBuffer@ is the source buffer.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the layout of the destination image subresources
--     for the copy.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy'
--     structures specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source buffer to the specified region of the destination image.
--
-- == Valid Usage
--
-- -   @srcBuffer@ /must/ be large enough to contain all buffer locations
--     that are accessed according to
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @dstImage@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstImage@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstImage@ /must/ have a sample count equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- -   The @imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@ of each element of @pRegions@ /must/
--     be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- -   The @imageOffset@ and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @dstImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcBuffer@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout'
cmdCopyBufferToImage :: CommandBuffer ->  Buffer ->  Image ->  ImageLayout ->  Vector BufferImageCopy ->  IO ()
cmdCopyBufferToImage = \(CommandBuffer commandBuffer' commandTable) -> \srcBuffer' -> \dstImage' -> \dstImageLayout' -> \regions' -> withVec withCStructBufferImageCopy regions' (\pRegions' -> vkCmdCopyBufferToImage commandTable commandBuffer' srcBuffer' dstImage' dstImageLayout' (fromIntegral $ Data.Vector.length regions') pRegions' *> (pure ()))


-- | vkCmdCopyImage - Copy data between images
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the current layout of the source image
--     subresource.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the current layout of the destination image
--     subresource.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy'
--     structures specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the source image to the same
-- region of the destination image. @srcImage@ and @dstImage@ /can/ be the
-- same image or alias the same memory.
--
-- The formats of @srcImage@ and @dstImage@ /must/ be compatible. Formats
-- are compatible if they share the same class, as shown in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#formats-compatibility Compatible Formats>
-- table. Depth\/stencil formats /must/ match exactly.
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage' allows
-- copying between /size-compatible/ compressed and uncompressed internal
-- formats. Formats are size-compatible if the texel block size of the
-- uncompressed format is equal to the texel block size of the compressed
-- format. Such a copy does not perform on-the-fly compression or
-- decompression. When copying from an uncompressed format to a compressed
-- format, each texel of uncompressed data of the source image is copied as
-- a raw value to the corresponding compressed texel block of the
-- destination image. When copying from a compressed format to an
-- uncompressed format, each compressed texel block of the source image is
-- copied as a raw value to the corresponding texel of uncompressed data in
-- the destination image. Thus, for example, it is legal to copy between a
-- 128-bit uncompressed format and a compressed format which has a 128-bit
-- sized compressed texel block representing 4×4 texels (using 8 bits per
-- texel), or between a 64-bit uncompressed format and a compressed format
-- which has a 64-bit sized compressed texel block representing 4×4 texels
-- (using 4 bits per texel).
--
-- When copying between compressed and uncompressed formats the @extent@
-- members represent the texel dimensions of the source image and not the
-- destination. When copying from a compressed image to an uncompressed
-- image the image texel dimensions written to the uncompressed image will
-- be source extent divided by the compressed texel block dimensions. When
-- copying from an uncompressed image to a compressed image the image texel
-- dimensions written to the compressed image will be the source extent
-- multiplied by the compressed texel block dimensions. In both cases the
-- number of bytes read and the number of bytes written will be identical.
--
-- Copying to or from block-compressed images is typically done in
-- multiples of the compressed texel block size. For this reason the
-- @extent@ /must/ be a multiple of the compressed texel block dimension.
-- There is one exception to this rule which is /required/ to handle
-- compressed images created with dimensions that are not a multiple of the
-- compressed texel block dimensions: if the @srcImage@ is compressed,
-- then:
--
-- -   If @extent.width@ is not a multiple of the compressed texel block
--     width, then (@extent.width@ + @srcOffset.x@) /must/ equal the image
--     subresource width.
--
-- -   If @extent.height@ is not a multiple of the compressed texel block
--     height, then (@extent.height@ + @srcOffset.y@) /must/ equal the
--     image subresource height.
--
-- -   If @extent.depth@ is not a multiple of the compressed texel block
--     depth, then (@extent.depth@ + @srcOffset.z@) /must/ equal the image
--     subresource depth.
--
-- Similarly, if the @dstImage@ is compressed, then:
--
-- -   If @extent.width@ is not a multiple of the compressed texel block
--     width, then (@extent.width@ + @dstOffset.x@) /must/ equal the image
--     subresource width.
--
-- -   If @extent.height@ is not a multiple of the compressed texel block
--     height, then (@extent.height@ + @dstOffset.y@) /must/ equal the
--     image subresource height.
--
-- -   If @extent.depth@ is not a multiple of the compressed texel block
--     depth, then (@extent.depth@ + @dstOffset.z@) /must/ equal the image
--     subresource depth.
--
-- This allows the last compressed texel block of the image in each
-- non-multiple dimension to be included as a source or destination of the
-- copy.
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImage' /can/ be
-- used to copy image data between multisample images, but both images
-- /must/ have the same number of samples.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcImage@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   @dstImage@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The 'Graphics.Vulkan.C.Core10.Core.VkFormat' of each of @srcImage@
--     and @dstImage@ /must/ be compatible, as defined
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies-images-format-compatibility above>
--
-- -   The sample count of @srcImage@ and @dstImage@ /must/ match
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- -   The @srcOffset@ and @extent@ members of each element of @pRegions@
--     /must/ respect the image transfer granularity requirements of
--     @commandBuffer@’s command pool’s queue family, as described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- -   The @dstOffset@ and @extent@ members of each element of @pRegions@
--     /must/ respect the image transfer granularity requirements of
--     @commandBuffer@’s command pool’s queue family, as described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @dstImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcImage@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout'
cmdCopyImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageCopy ->  IO ()
cmdCopyImage = \(CommandBuffer commandBuffer' commandTable) -> \srcImage' -> \srcImageLayout' -> \dstImage' -> \dstImageLayout' -> \regions' -> withVec withCStructImageCopy regions' (\pRegions' -> vkCmdCopyImage commandTable commandBuffer' srcImage' srcImageLayout' dstImage' dstImageLayout' (fromIntegral $ Data.Vector.length regions') pRegions' *> (pure ()))


-- | vkCmdCopyImageToBuffer - Copy image data into a buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the layout of the source image subresources for
--     the copy.
--
-- -   @dstBuffer@ is the destination buffer.
--
-- -   @regionCount@ is the number of regions to copy.
--
-- -   @pRegions@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy'
--     structures specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source image to the specified region of the destination buffer.
--
-- == Valid Usage
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @srcImage@
--
-- -   @dstBuffer@ /must/ be large enough to contain all buffer locations
--     that are accessed according to
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies-buffers-images-addressing Buffer and Image Addressing>,
--     for each element of @pRegions@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcImage@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
--     usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @srcImage@ /must/ have a sample count equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   The @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@ of each element of @pRegions@ /must/
--     be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @imageOffset@ and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstBuffer@, and @srcImage@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout'
cmdCopyImageToBuffer :: CommandBuffer ->  Image ->  ImageLayout ->  Buffer ->  Vector BufferImageCopy ->  IO ()
cmdCopyImageToBuffer = \(CommandBuffer commandBuffer' commandTable) -> \srcImage' -> \srcImageLayout' -> \dstBuffer' -> \regions' -> withVec withCStructBufferImageCopy regions' (\pRegions' -> vkCmdCopyImageToBuffer commandTable commandBuffer' srcImage' srcImageLayout' dstBuffer' (fromIntegral $ Data.Vector.length regions') pRegions' *> (pure ()))


-- | vkCmdCopyQueryPoolResults - Copy the results of queries in a query pool
-- to a buffer object
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool managing the queries containing the
--     desired results.
--
-- -   @firstQuery@ is the initial query index.
--
-- -   @queryCount@ is the number of queries. @firstQuery@ and @queryCount@
--     together define a range of queries.
--
-- -   @dstBuffer@ is a
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' object that
--     will receive the results of the copy command.
--
-- -   @dstOffset@ is an offset into @dstBuffer@.
--
-- -   @stride@ is the stride in bytes between results for individual
--     queries within @dstBuffer@. The required size of the backing memory
--     for @dstBuffer@ is determined as described above for
--     'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults'.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlagBits' specifying
--     how and when results are returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'
-- is guaranteed to see the effect of previous uses of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool' in
-- the same queue, without any additional synchronization. Thus, the
-- results will always reflect the most recent use of the query.
--
-- @flags@ has the same possible values described above for the @flags@
-- parameter of 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults', but
-- the different style of execution causes some subtle behavioral
-- differences. Because
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'
-- executes in order with respect to other query commands, there is less
-- ambiguity about which use of a query is being requested.
--
-- If no bits are set in @flags@, results for all requested queries in the
-- available state are written as 32-bit unsigned integer values, and
-- nothing is written for queries in the unavailable state.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is set, the
-- results are written as an array of 64-bit unsigned integer values as
-- described for 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults'.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' is set, the
-- implementation will wait for each query’s status to be in the available
-- state before retrieving the numerical results for that query. This is
-- guaranteed to reflect the most recent use of the query on the same
-- queue, assuming that the query is not being simultaneously used by other
-- queues. If the query does not become available in a finite amount of
-- time (e.g. due to not issuing a query since the last reset), a
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST' error /may/ occur.
--
-- Similarly, if
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set and 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' is
-- not set, the availability is guaranteed to reflect the most recent use
-- of the query on the same queue, assuming that the query is not being
-- simultaneously used by other queues. As with
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults', implementations
-- /must/ guarantee that if they return a non-zero availability value, then
-- the numerical results are valid.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT' is set,
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' is not set,
-- and the query’s status is unavailable, an intermediate result value
-- between zero and the final result value is written for that query.
--
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT' /must/ not
-- be used if the pool’s @queryType@ is
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_TIMESTAMP'.
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'
-- is considered to be a transfer operation, and its writes to buffer
-- memory /must/ be synchronized using
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TRANSFER_BIT' and
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_TRANSFER_WRITE_BIT' before
-- using the results.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- -   If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is not
--     set in @flags@ then @dstOffset@ and @stride@ /must/ be multiples of
--     @4@
--
-- -   If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is set in
--     @flags@ then @dstOffset@ and @stride@ /must/ be multiples of @8@
--
-- -   @dstBuffer@ /must/ have enough storage, from @dstOffset@, to contain
--     the result of each query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_TIMESTAMP', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlagBits' values
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of @commandBuffer@, @dstBuffer@, and @queryPool@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlags'
cmdCopyQueryPoolResults :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  Buffer ->  DeviceSize ->  DeviceSize ->  QueryResultFlags ->  IO ()
cmdCopyQueryPoolResults = \(CommandBuffer commandBuffer' commandTable) -> \queryPool' -> \firstQuery' -> \queryCount' -> \dstBuffer' -> \dstOffset' -> \stride' -> \flags' -> vkCmdCopyQueryPoolResults commandTable commandBuffer' queryPool' firstQuery' queryCount' dstBuffer' dstOffset' stride' flags' *> (pure ())


-- | vkCmdDispatch - Dispatch compute work items
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @groupCountX@ is the number of local workgroups to dispatch in the X
--     dimension.
--
-- -   @groupCountY@ is the number of local workgroups to dispatch in the Y
--     dimension.
--
-- -   @groupCountZ@ is the number of local workgroups to dispatch in the Z
--     dimension.
--
-- = Description
--
-- When the command is executed, a global workgroup consisting of
-- @groupCountX@ × @groupCountY@ × @groupCountZ@ local workgroups is
-- assembled.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   @groupCountX@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   @groupCountY@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   @groupCountZ@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Compute         | Compute         |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdDispatch :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDispatch = \(CommandBuffer commandBuffer' commandTable) -> \groupCountX' -> \groupCountY' -> \groupCountZ' -> vkCmdDispatch commandTable commandBuffer' groupCountX' groupCountY' groupCountZ' *> (pure ())


-- | vkCmdDispatchIndirect - Dispatch compute work items using indirect
-- parameters
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @buffer@ is the buffer containing dispatch parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatch' except
-- that the parameters are read by the device from a buffer during
-- execution. The parameters of the dispatch are encoded in a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDispatchIndirectCommand'
-- structure taken from @buffer@ starting at @offset@.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   The sum of @offset@ and the size of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDispatchIndirectCommand'
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Compute         | Compute         |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdDispatchIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  IO ()
cmdDispatchIndirect = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> vkCmdDispatchIndirect commandTable commandBuffer' buffer' offset' *> (pure ())


-- | vkCmdDraw - Draw primitives
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @vertexCount@ is the number of vertices to draw.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstVertex@ is the index of the first vertex to draw.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- = Description
--
-- When the command is executed, primitives are assembled using the current
-- primitive topology and @vertexCount@ consecutive vertex indices with the
-- first @vertexIndex@ value equal to @firstVertex@. The primitives are
-- drawn @instanceCount@ times with @instanceIndex@ starting with
-- @firstInstance@ and increasing sequentially for each instance. The
-- assembled primitives execute the bound graphics pipeline.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input ???>
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdDraw :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ()
cmdDraw = \(CommandBuffer commandBuffer' commandTable) -> \vertexCount' -> \instanceCount' -> \firstVertex' -> \firstInstance' -> vkCmdDraw commandTable commandBuffer' vertexCount' instanceCount' firstVertex' firstInstance' *> (pure ())


-- | vkCmdDrawIndexed - Issue an indexed draw into a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @indexCount@ is the number of vertices to draw.
--
-- -   @instanceCount@ is the number of instances to draw.
--
-- -   @firstIndex@ is the base index within the index buffer.
--
-- -   @vertexOffset@ is the value added to the vertex index before
--     indexing into the vertex buffer.
--
-- -   @firstInstance@ is the instance ID of the first instance to draw.
--
-- = Description
--
-- When the command is executed, primitives are assembled using the current
-- primitive topology and @indexCount@ vertices whose indices are retrieved
-- from the index buffer. The index buffer is treated as an array of
-- tightly packed unsigned integers of size defined by the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'::@indexType@
-- parameter with which the buffer was bound.
--
-- The first vertex index is at an offset of @firstIndex@ * @indexSize@ +
-- @offset@ within the bound index buffer, where @offset@ is the offset
-- specified by
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'
-- and @indexSize@ is the byte size of the type specified by @indexType@.
-- Subsequent index values are retrieved from consecutive locations in the
-- index buffer. Indices are first compared to the primitive restart value,
-- then zero extended to 32 bits (if the @indexType@ is
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT16')
-- and have @vertexOffset@ added to them, before being supplied as the
-- @vertexIndex@ value.
--
-- The primitives are drawn @instanceCount@ times with @instanceIndex@
-- starting with @firstInstance@ and increasing sequentially for each
-- instance. The assembled primitives execute the bound graphics pipeline.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     @indexSize@ being based on the type specified by @indexType@, where
--     the index buffer, @indexType@, and @offset@ are specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdDrawIndexed :: CommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Int32 ->  Word32 ->  IO ()
cmdDrawIndexed = \(CommandBuffer commandBuffer' commandTable) -> \indexCount' -> \instanceCount' -> \firstIndex' -> \vertexOffset' -> \firstInstance' -> vkCmdDrawIndexed commandTable commandBuffer' indexCount' instanceCount' firstIndex' vertexOffset' firstInstance' *> (pure ())


-- | vkCmdDrawIndexedIndirect - Perform an indexed indirect draw
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @drawCount@ is the number of draws to execute, and /can/ be zero.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexed' except
-- that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from @buffer@ starting at @offset@ and increasing by
-- @stride@ bytes for each successive draw. The parameters of each draw are
-- encoded in an array of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'
-- structures. If @drawCount@ is less than or equal to one, @stride@ is
-- ignored.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand')
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'
--     structures accessed by this command /must/ be @0@
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdDrawIndexedIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndexedIndirect = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \drawCount' -> \stride' -> vkCmdDrawIndexedIndirect commandTable commandBuffer' buffer' offset' drawCount' stride' *> (pure ())


-- | vkCmdDrawIndirect - Issue an indirect draw into a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @buffer@ is the buffer containing draw parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- -   @drawCount@ is the number of draws to execute, and /can/ be zero.
--
-- -   @stride@ is the byte stride between successive sets of draw
--     parameters.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect'
-- behaves similarly to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDraw' except that
-- the parameters are read by the device from a buffer during execution.
-- @drawCount@ draws are executed by the command, with parameters taken
-- from @buffer@ starting at @offset@ and increasing by @stride@ bytes for
-- each successive draw. The parameters of each draw are encoded in an
-- array of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'
-- structures. If @drawCount@ is less than or equal to one, @stride@ is
-- ignored.
--
-- == Valid Usage
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is sampled
--     with 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   If a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' is accessed
--     using atomic operations as a result of this command, then the image
--     view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   For each set /n/ that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a descriptor set /must/ have been
--     bound to /n/ at the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for set /n/, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   For each push constant that is statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command, a push constant value /must/ have
--     been set for the same pipeline bind point, with a
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' that is
--     compatible for push constants, with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' used to create
--     the current 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline', as
--     described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   Descriptors in each bound descriptor set, specified via
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindDescriptorSets',
--     /must/ be valid if they are statically used by the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to the pipeline
--     bind point used by this command
--
-- -   A valid pipeline /must/ be bound to the pipeline bind point used by
--     this command
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command requires any dynamic
--     state, that state /must/ have been set for @commandBuffer@
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used to sample
--     from any 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' with a
--     'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of the type
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_3D',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY',
--     in any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions with @ImplicitLod@, @Dref@ or @Proj@ in their name, in
--     any shader stage
--
-- -   If the 'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound
--     to the pipeline bind point used by this command accesses a
--     'Graphics.Vulkan.C.Core10.Sampler.VkSampler' object that uses
--     unnormalized coordinates, that sampler /must/ not be used with any
--     of the SPIR-V @OpImageSample*@ or @OpImageSparseSample*@
--     instructions that includes a LOD bias or any offset values, in any
--     shader stage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a uniform buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' object bound to the
--     pipeline bind point used by this command accesses a storage buffer,
--     it /must/ not access values outside of the range of the buffer as
--     specified in the descriptor set bound to the same pipeline bind
--     point
--
-- -   The current render pass /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the @renderPass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo'
--     structure specified when creating the
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipeline' bound to
--     'Graphics.Vulkan.C.Core10.Pass.VK_PIPELINE_BIND_POINT_GRAPHICS'.
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input ???>
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @buffer@ /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   @drawCount@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxDrawIndirectCount@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'
--     structures accessed by this command /must/ be @0@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand')
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ +
--     @sizeof@('Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'))
--     /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @buffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdDrawIndirect :: CommandBuffer ->  Buffer ->  DeviceSize ->  Word32 ->  Word32 ->  IO ()
cmdDrawIndirect = \(CommandBuffer commandBuffer' commandTable) -> \buffer' -> \offset' -> \drawCount' -> \stride' -> vkCmdDrawIndirect commandTable commandBuffer' buffer' offset' drawCount' stride' *> (pure ())


-- | vkCmdEndQuery - Ends a query
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the query pool that is managing the results of the
--     query.
--
-- -   @query@ is the query index within the query pool where the result is
--     stored.
--
-- = Description
--
-- As queries operate asynchronously, ending a query does not immediately
-- set the query’s status to available. A query is considered /finished/
-- when the final results of the query are ready to be retrieved by
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults' and
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- and this is when the query’s status is set to available.
--
-- Once a query is ended the query /must/ finish in finite time, unless the
-- state of the query is changed using other commands, e.g. by issuing a
-- reset of the query.
--
-- == Valid Usage
--
-- -   All queries used by the command /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
cmdEndQuery :: CommandBuffer ->  QueryPool ->  Word32 ->  IO ()
cmdEndQuery = \(CommandBuffer commandBuffer' commandTable) -> \queryPool' -> \query' -> vkCmdEndQuery commandTable commandBuffer' queryPool' query' *> (pure ())


-- | vkCmdEndRenderPass - End the current render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to end the current
--     render pass instance.
--
-- = Description
--
-- Ending a render pass instance performs any multisample resolve
-- operations on the final subpass.
--
-- == Valid Usage
--
-- -   The current subpass index /must/ be equal to the number of subpasses
--     in the render pass minus one
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a primary
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdEndRenderPass :: CommandBuffer ->  IO ()
cmdEndRenderPass = \(CommandBuffer commandBuffer' commandTable) -> vkCmdEndRenderPass commandTable commandBuffer' *> (pure ())


-- | vkCmdExecuteCommands - Execute a secondary command buffer from a primary
-- command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is a handle to a primary command buffer that the
--     secondary command buffers are executed in.
--
-- -   @commandBufferCount@ is the length of the @pCommandBuffers@ array.
--
-- -   @pCommandBuffers@ is an array of secondary command buffer handles,
--     which are recorded to execute in the primary command buffer in the
--     order they are listed in the array.
--
-- = Description
--
-- If any element of @pCommandBuffers@ was not recorded with the
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
-- flag, and it was recorded into any other primary command buffer which is
-- currently in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle executable or recording state>,
-- that primary command buffer becomes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ have been allocated with a @level@ of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_LEVEL_PRIMARY'
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated with a
--     @level@ of
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   Each element of @pCommandBuffers@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>.
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, and it was recorded into any other primary command buffer,
--     that primary command buffer /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not have already been recorded to @commandBuffer@.
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT'
--     flag, it /must/ not appear more than once in @pCommandBuffers@.
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated from a
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that was
--     created for the same queue family as the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' from which
--     @commandBuffer@ was allocated
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands'
--     is being called within a render pass instance, that render pass
--     instance /must/ have been begun with the @contents@ parameter of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass'
--     set to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS'
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands'
--     is being called within a render pass instance, each element of
--     @pCommandBuffers@ /must/ have been recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands'
--     is being called within a render pass instance, each element of
--     @pCommandBuffers@ /must/ have been recorded with
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo'::@subpass@
--     set to the index of the subpass which the given command buffer will
--     be executed in
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands'
--     is being called within a render pass instance, the render passes
--     specified in the @pBeginInfo@::@pInheritanceInfo@::@renderPass@
--     members of the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.vkBeginCommandBuffer'
--     commands used to begin recording each element of @pCommandBuffers@
--     /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-compatibility compatible>
--     with the current render pass.
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands'
--     is being called within a render pass instance, and any element of
--     @pCommandBuffers@ was recorded with
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo'::@framebuffer@
--     not equal to 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE',
--     that 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' /must/ match the
--     'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer' used in the current
--     render pass instance
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdExecuteCommands'
--     is not being called within a render pass instance, each element of
--     @pCommandBuffers@ /must/ not have been recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-inheritedQueries inherited queries>
--     feature is not enabled, @commandBuffer@ /must/ not have any queries
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--
-- -   If @commandBuffer@ has a
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION' query
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo'::@occlusionQueryEnable@
--     set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--
-- -   If @commandBuffer@ has a
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION' query
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo'::@queryFlags@
--     having all bits set that are set for the query
--
-- -   If @commandBuffer@ has a
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--     query
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>,
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo'::@pipelineStatistics@
--     having all bits set that are set in the
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' the query uses
--
-- -   Each element of @pCommandBuffers@ /must/ not begin any query types
--     that are
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-active active>
--     in @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@ valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handles
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   @commandBuffer@ /must/ be a primary
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
--
-- -   @commandBufferCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pCommandBuffers@
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Transfer        |                 |
-- > |                 |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdExecuteCommands :: CommandBuffer ->  Vector CommandBuffer ->  IO ()
cmdExecuteCommands = \(CommandBuffer commandBuffer' commandTable) -> \commandBuffers' -> withVec ((&) . commandBufferHandle) commandBuffers' (\pCommandBuffers' -> vkCmdExecuteCommands commandTable commandBuffer' (fromIntegral $ Data.Vector.length commandBuffers') pCommandBuffers' *> (pure ()))


-- | vkCmdFillBuffer - Fill a region of a buffer with a fixed value
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @dstBuffer@ is the buffer to be filled.
--
-- -   @dstOffset@ is the byte offset into the buffer at which to start
--     filling, and /must/ be a multiple of 4.
--
-- -   @size@ is the number of bytes to fill, and /must/ be either a
--     multiple of 4, or 'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE'
--     to fill the range from @offset@ to the end of the buffer. If
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE' is used and the
--     remaining size of the buffer is not a multiple of 4, then the
--     nearest smaller multiple is used.
--
-- -   @data@ is the 4-byte word written repeatedly to the buffer to fill
--     @size@ bytes of data. The data word is written to memory according
--     to the host endianness.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer' is
-- treated as “transfer” operation for the purposes of synchronization
-- barriers. The
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
-- /must/ be specified in @usage@ of
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' in order for the
-- buffer to be compatible with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer'.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     greater than @0@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     less than or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   If @size@ is not equal to
--     'Graphics.Vulkan.C.Core10.Constants.VK_WHOLE_SIZE', @size@ /must/ be
--     a multiple of @4@
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics or
--     compute operations
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer, graphics
--     or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdFillBuffer :: CommandBuffer ->  Buffer ->  DeviceSize ->  DeviceSize ->  Word32 ->  IO ()
cmdFillBuffer = \(CommandBuffer commandBuffer' commandTable) -> \dstBuffer' -> \dstOffset' -> \size' -> \data' -> vkCmdFillBuffer commandTable commandBuffer' dstBuffer' dstOffset' size' data' *> (pure ())


-- | vkCmdNextSubpass - Transition to the next subpass of a render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @contents@ specifies how the commands in the next subpass will be
--     provided, in the same fashion as the corresponding parameter of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass'.
--
-- = Description
--
-- The subpass index for a render pass begins at zero when
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginRenderPass' is
-- recorded, and increments each time
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdNextSubpass' is
-- recorded.
--
-- Moving to the next subpass automatically performs any multisample
-- resolve operations in the subpass being ended. End-of-subpass
-- multisample resolves are treated as color attachment writes for the
-- purposes of synchronization. That is, they are considered to execute in
-- the
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage and their writes are synchronized with
-- 'Graphics.Vulkan.C.Core10.Pass.VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT'.
-- Synchronization between rendering within a subpass and any resolve
-- operations at the end of the subpass occurs automatically, without need
-- for explicit dependencies or pipeline barriers. However, if the resolve
-- attachment is also used in a different subpass, an explicit dependency
-- is needed.
--
-- After transitioning to the next subpass, the application /can/ record
-- the commands for that subpass.
--
-- == Valid Usage
--
-- -   The current subpass index /must/ be less than the number of
--     subpasses in the render pass minus one
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @contents@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkSubpassContents'
--     value
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a primary
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkSubpassContents'
cmdNextSubpass :: CommandBuffer ->  SubpassContents ->  IO ()
cmdNextSubpass = \(CommandBuffer commandBuffer' commandTable) -> \contents' -> vkCmdNextSubpass commandTable commandBuffer' contents' *> (pure ())


-- | vkCmdPipelineBarrier - Insert a memory dependency
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @srcStageMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>.
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>.
--
-- -   @dependencyFlags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlagBits' specifying how
--     execution and memory dependencies are formed.
--
-- -   @memoryBarrierCount@ is the length of the @pMemoryBarriers@ array.
--
-- -   @pMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier'
--     structures.
--
-- -   @bufferMemoryBarrierCount@ is the length of the
--     @pBufferMemoryBarriers@ array.
--
-- -   @pBufferMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier'
--     structures.
--
-- -   @imageMemoryBarrierCount@ is the length of the
--     @pImageMemoryBarriers@ array.
--
-- -   @pImageMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
--     structures.
--
-- = Description
--
-- When
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier' is
-- submitted to a queue, it defines a memory dependency between commands
-- that were submitted before it, and those submitted after it.
--
-- If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
-- was recorded outside a render pass instance, the first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
-- was recorded inside a render pass instance, the first synchronization
-- scope includes only commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- within the same subpass. In either case, the first synchronization scope
-- is limited to operations on the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
-- was recorded outside a render pass instance, the second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- If 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
-- was recorded inside a render pass instance, the second synchronization
-- scope includes only commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- within the same subpass. In either case, the second synchronization
-- scope is limited to operations on the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- If @dependencyFlags@ includes
-- 'Graphics.Vulkan.C.Core10.Pass.VK_DEPENDENCY_BY_REGION_BIT', then any
-- dependency between
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-space>
-- pipeline stages is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-local>
-- - otherwise it is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-framebuffer-regions framebuffer-global>.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
--     is called within a render pass instance, the render pass /must/ have
--     been created with at least one
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency' instance in
--     'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo'::@pDependencies@
--     that expresses a dependency from the current subpass to itself, and
--     for which @srcStageMask@ contains a subset of the bit values in
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency'::@srcStageMask@,
--     @dstStageMask@ contains a subset of the bit values in
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency'::@dstStageMask@,
--     @dependencyFlags@ is equal to
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency'::@dependencyFlags@,
--     @srcAccessMask@ member of each element of @pMemoryBarriers@ and
--     @pImageMemoryBarriers@ contains a subset of the bit values in
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency'::@srcAccessMask@,
--     and @dstAccessMask@ member of each element of @pMemoryBarriers@ and
--     @pImageMemoryBarriers@ contains a subset of the bit values in
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency'::@dstAccessMask@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
--     is called within a render pass instance, @bufferMemoryBarrierCount@
--     /must/ be @0@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
--     is called within a render pass instance, the @image@ member of any
--     element of @pImageMemoryBarriers@ /must/ be equal to one of the
--     elements of @pAttachments@ that the current @framebuffer@ was
--     created with, that is also referred to by one of the elements of the
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@ members of the
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription' instance that
--     the current subpass was created with
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
--     is called within a render pass instance, the @oldLayout@ and
--     @newLayout@ members of any element of @pImageMemoryBarriers@ /must/
--     be equal to the @layout@ member of an element of the
--     @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@ members of the
--     'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription' instance that
--     the current subpass was created with, that refers to the same
--     @image@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
--     is called within a render pass instance, the @oldLayout@ and
--     @newLayout@ members of an element of @pImageMemoryBarriers@ /must/
--     be equal
--
-- -   If
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier'
--     is called within a render pass instance, the @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ members of any element of
--     @pImageMemoryBarriers@ /must/ be
--     'Graphics.Vulkan.C.Core10.Constants.VK_QUEUE_FAMILY_IGNORED'
--
-- -   Any pipeline stage included in @srcStageMask@ or @dstStageMask@
--     /must/ be supported by the capabilities of the queue family
--     specified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo'
--     structure that was used to create the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ and
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ and
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   @dependencyFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlagBits' values
--
-- -   If @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a
--     valid pointer to an array of @memoryBarrierCount@ valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier'
--     structures
--
-- -   If @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier'
--     structures
--
-- -   If @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@
--     /must/ be a valid pointer to an array of @imageMemoryBarrierCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Transfer        |                 |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Pass.VkDependencyFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags'
cmdPipelineBarrier :: CommandBuffer ->  PipelineStageFlags ->  PipelineStageFlags ->  DependencyFlags ->  Vector MemoryBarrier ->  Vector BufferMemoryBarrier ->  Vector ImageMemoryBarrier ->  IO ()
cmdPipelineBarrier = \(CommandBuffer commandBuffer' commandTable) -> \srcStageMask' -> \dstStageMask' -> \dependencyFlags' -> \memoryBarriers' -> \bufferMemoryBarriers' -> \imageMemoryBarriers' -> withVec withCStructImageMemoryBarrier imageMemoryBarriers' (\pImageMemoryBarriers' -> withVec withCStructBufferMemoryBarrier bufferMemoryBarriers' (\pBufferMemoryBarriers' -> withVec withCStructMemoryBarrier memoryBarriers' (\pMemoryBarriers' -> vkCmdPipelineBarrier commandTable commandBuffer' srcStageMask' dstStageMask' dependencyFlags' (fromIntegral $ Data.Vector.length memoryBarriers') pMemoryBarriers' (fromIntegral $ Data.Vector.length bufferMemoryBarriers') pBufferMemoryBarriers' (fromIntegral $ Data.Vector.length imageMemoryBarriers') pImageMemoryBarriers' *> (pure ()))))


-- | vkCmdPushConstants - Update the values of push constants
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which the push constant
--     update will be recorded.
--
-- -   @layout@ is the pipeline layout used to program the push constant
--     updates.
--
-- -   @stageFlags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits' specifying
--     the shader stages that will use the push constants in the updated
--     range.
--
-- -   @offset@ is the start offset of the push constant range to update,
--     in units of bytes.
--
-- -   @size@ is the size of the push constant range to update, in units of
--     bytes.
--
-- -   @pValues@ is an array of @size@ bytes containing the new push
--     constant values.
--
-- = Description
--
-- __Note__
--
-- As @stageFlags@ needs to include all flags the relevant push constant
-- ranges were created with, any flags that are not supported by the queue
-- family that the 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool'
-- used to allocate @commandBuffer@ was created on are ignored.
--
-- == Valid Usage
--
-- -   For each byte in the range specified by @offset@ and @size@ and for
--     each shader stage in @stageFlags@, there /must/ be a push constant
--     range in @layout@ that includes that byte and that stage
--
-- -   For each byte in the range specified by @offset@ and @size@ and for
--     each push constant range that overlaps that byte, @stageFlags@
--     /must/ include all stages in that push constant range’s
--     'Graphics.Vulkan.C.Core10.PipelineLayout.VkPushConstantRange'::@stageFlags@
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @size@ /must/ be a multiple of @4@
--
-- -   @offset@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPushConstantsSize@
--
-- -   @size@ /must/ be less than or equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxPushConstantsSize@
--     minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @layout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout' handle
--
-- -   @stageFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkShaderStageFlagBits' values
--
-- -   @stageFlags@ /must/ not be @0@
--
-- -   @pValues@ /must/ be a valid pointer to an array of @size@ bytes
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   @size@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @layout@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkShaderStageFlags'
cmdPushConstants :: (Storable a) => CommandBuffer ->  PipelineLayout ->  ShaderStageFlags ->  Word32 ->  Vector a ->  IO ()
cmdPushConstants = \(CommandBuffer commandBuffer' commandTable) -> \layout' -> \stageFlags' -> \offset' -> \values' -> withVec (&) values' (\pValues' -> vkCmdPushConstants commandTable commandBuffer' layout' stageFlags' offset' (fromIntegral $ sizeOf (Data.Vector.head values') * Data.Vector.length values') (castPtr pValues') *> (pure ()))


-- | vkCmdResetEvent - Reset an event object to non-signaled state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @event@ is the event that will be unsignaled.
--
-- -   @stageMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
--     used to determine when the @event@ is unsignaled.
--
-- = Description
--
-- When 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' is
-- submitted to a queue, it defines an execution dependency on commands
-- that were submitted before it, and defines an event unsignal operation
-- which resets the event to the unsignaled state.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @stageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event unsignal operation.
--
-- If @event@ is already in the unsignaled state when
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' is
-- executed on the device, then
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' has no
-- effect, no event unsignal operation occurs, and no execution dependency
-- is generated.
--
-- == Valid Usage
--
-- -   @stageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   When this command executes, @event@ /must/ not be waited on by a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
--     command that is currently executing
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @event@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Event.VkEvent'
--     handle
--
-- -   @stageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @stageMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @event@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags'
cmdResetEvent :: CommandBuffer ->  Event ->  PipelineStageFlags ->  IO ()
cmdResetEvent = \(CommandBuffer commandBuffer' commandTable) -> \event' -> \stageMask' -> vkCmdResetEvent commandTable commandBuffer' event' stageMask' *> (pure ())


-- | vkCmdResetQueryPool - Reset queries in a query pool
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which this command will
--     be recorded.
--
-- -   @queryPool@ is the handle of the query pool managing the queries
--     being reset.
--
-- -   @firstQuery@ is the initial query index to reset.
--
-- -   @queryCount@ is the number of queries to reset.
--
-- = Description
--
-- When executed on a queue, this command sets the status of query indices
-- [@firstQuery@, @firstQuery@ + @queryCount@ - 1] to unavailable.
--
-- == Valid Usage
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
cmdResetQueryPool :: CommandBuffer ->  QueryPool ->  Word32 ->  Word32 ->  IO ()
cmdResetQueryPool = \(CommandBuffer commandBuffer' commandTable) -> \queryPool' -> \firstQuery' -> \queryCount' -> vkCmdResetQueryPool commandTable commandBuffer' queryPool' firstQuery' queryCount' *> (pure ())


-- | vkCmdResolveImage - Resolve regions of an image
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @srcImage@ is the source image.
--
-- -   @srcImageLayout@ is the layout of the source image subresources for
--     the resolve.
--
-- -   @dstImage@ is the destination image.
--
-- -   @dstImageLayout@ is the layout of the destination image subresources
--     for the resolve.
--
-- -   @regionCount@ is the number of regions to resolve.
--
-- -   @pRegions@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve'
--     structures specifying the regions to resolve.
--
-- = Description
--
-- During the resolve the samples corresponding to each pixel location in
-- the source are converted to a single sample before being written to the
-- destination. If the source formats are floating-point or normalized
-- types, the sample values for each pixel are resolved in an
-- implementation-dependent manner. If the source formats are integer
-- types, a single sample’s value is selected for each pixel.
--
-- @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
-- in texels of the sub-regions of the source and destination image data.
-- @extent@ is the size in texels of the source image to resolve in
-- @width@, @height@ and @depth@.
--
-- Resolves are done layer by layer starting with @baseArrayLayer@ member
-- of @srcSubresource@ for the source and @dstSubresource@ for the
-- destination. @layerCount@ layers are resolved to the destination image.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @srcImage@ /must/ have a sample count equal to any valid sample
--     count value other than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstImage@ /must/ have a sample count equal to
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @srcImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- -   @dstImageLayout@ /must/ be
--     'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL'
--     or 'Graphics.Vulkan.C.Core10.Image.VK_IMAGE_LAYOUT_GENERAL'
--
-- -   The
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-image-format-features format features>
--     of @dstImage@ /must/ contain
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'.
--
-- -   @srcImage@ and @dstImage@ /must/ have been created with the same
--     image format
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @srcImage@
--     was created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' when @dstImage@
--     was created
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @srcImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @dstImage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcImage@ /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve'
cmdResolveImage :: CommandBuffer ->  Image ->  ImageLayout ->  Image ->  ImageLayout ->  Vector ImageResolve ->  IO ()
cmdResolveImage = \(CommandBuffer commandBuffer' commandTable) -> \srcImage' -> \srcImageLayout' -> \dstImage' -> \dstImageLayout' -> \regions' -> withVec withCStructImageResolve regions' (\pRegions' -> vkCmdResolveImage commandTable commandBuffer' srcImage' srcImageLayout' dstImage' dstImageLayout' (fromIntegral $ Data.Vector.length regions') pRegions' *> (pure ()))


-- | vkCmdSetBlendConstants - Set the values of blend constants
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @blendConstants@ is an array of four values specifying the R, G, B,
--     and A components of the blend constant color used in blending,
--     depending on the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-blendfactors blend factor>.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_BLEND_CONSTANTS'
--     dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetBlendConstants :: CommandBuffer ->  (CFloat, CFloat, CFloat, CFloat) ->  IO ()
cmdSetBlendConstants = \(CommandBuffer commandBuffer' commandTable) -> \(blendConstants'0, blendConstants'1, blendConstants'2, blendConstants'3) -> allocaArray 4 (\pBlendConstants' -> pokeElemOff pBlendConstants' 0 blendConstants'0*> pokeElemOff pBlendConstants' 1 blendConstants'1*> pokeElemOff pBlendConstants' 2 blendConstants'2*> pokeElemOff pBlendConstants' 3 blendConstants'3 *> vkCmdSetBlendConstants commandTable commandBuffer' pBlendConstants' *> (pure ()))


-- | vkCmdSetDepthBias - Set the depth bias dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @depthBiasConstantFactor@ is a scalar factor controlling the
--     constant depth value added to each fragment.
--
-- -   @depthBiasClamp@ is the maximum (or minimum) depth bias of a
--     fragment.
--
-- -   @depthBiasSlopeFactor@ is a scalar factor applied to a fragment’s
--     slope in depth bias calculations.
--
-- = Description
--
-- If @depthBiasEnable@ is 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', no
-- depth bias is applied and the fragment’s depth values are unchanged.
--
-- @depthBiasSlopeFactor@ scales the maximum depth slope of the polygon,
-- and @depthBiasConstantFactor@ scales an implementation-dependent
-- constant that relates to the usable resolution of the depth buffer. The
-- resulting values are summed to produce the depth bias value which is
-- then clamped to a minimum or maximum value specified by
-- @depthBiasClamp@. @depthBiasSlopeFactor@, @depthBiasConstantFactor@, and
-- @depthBiasClamp@ /can/ each be positive, negative, or zero.
--
-- The maximum depth slope m of a triangle is
--
-- \[m = \sqrt{ \left({{\partial z_f} \over {\partial x_f}}\right)^2
--         +  \left({{\partial z_f} \over {\partial y_f}}\right)^2}\]
--
-- where (xf, yf, zf) is a point on the triangle. m /may/ be approximated
-- as
--
-- \[m = \max\left( \left| { {\partial z_f} \over {\partial x_f} } \right|,
--                \left| { {\partial z_f} \over {\partial y_f} } \right|
--        \right).\]
--
-- The minimum resolvable difference r is an implementation-dependent
-- parameter that depends on the depth buffer representation. It is the
-- smallest difference in framebuffer coordinate z values that is
-- guaranteed to remain distinct throughout polygon rasterization and in
-- the depth buffer. All pairs of fragments generated by the rasterization
-- of two polygons with otherwise identical vertices, but @z@f values that
-- differ by r, will have distinct depth values.
--
-- For fixed-point depth buffer representations, r is constant throughout
-- the range of the entire depth buffer. For floating-point depth buffers,
-- there is no single minimum resolvable difference. In this case, the
-- minimum resolvable difference for a given polygon is dependent on the
-- maximum exponent, e, in the range of z values spanned by the primitive.
-- If n is the number of bits in the floating-point mantissa, the minimum
-- resolvable difference, r, for the given primitive is defined as
--
-- -   r = 2e-n
--
-- If no depth buffer is present, r is undefined.
--
-- The bias value o for a polygon is
--
-- \[\begin{aligned}
-- o &= \mathrm{dbclamp}( m \times \mathtt{depthBiasSlopeFactor} + r \times \mathtt{depthBiasConstantFactor} ) \\
-- \text{where} &\quad \mathrm{dbclamp}(x) =
-- \begin{cases}
--     x                                 & \mathtt{depthBiasClamp} = 0 \ \text{or}\ \texttt{NaN} \\
--     \min(x, \mathtt{depthBiasClamp})  & \mathtt{depthBiasClamp} > 0 \\
--     \max(x, \mathtt{depthBiasClamp})  & \mathtt{depthBiasClamp} < 0 \\
-- \end{cases}
-- \end{aligned}\]
--
-- m is computed as described above. If the depth buffer uses a fixed-point
-- representation, m is a function of depth values in the range [0,1], and
-- o is applied to depth values in the same range.
--
-- For fixed-point depth buffers, fragment depth values are always limited
-- to the range [0,1] by clamping after depth bias addition is performed.
-- Fragment depth values are clamped even when the depth buffer uses a
-- floating-point representation.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BIAS'
--     dynamic state enabled
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-depthBiasClamp depth bias clamping>
--     feature is not enabled, @depthBiasClamp@ /must/ be @0.0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetDepthBias :: CommandBuffer ->  CFloat ->  CFloat ->  CFloat ->  IO ()
cmdSetDepthBias = \(CommandBuffer commandBuffer' commandTable) -> \depthBiasConstantFactor' -> \depthBiasClamp' -> \depthBiasSlopeFactor' -> vkCmdSetDepthBias commandTable commandBuffer' depthBiasConstantFactor' depthBiasClamp' depthBiasSlopeFactor' *> (pure ())


-- | vkCmdSetDepthBounds - Set the depth bounds test values for a command
-- buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @minDepthBounds@ is the lower bound of the range of depth values
--     used in the depth bounds test.
--
-- -   @maxDepthBounds@ is the upper bound of the range.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BOUNDS'
--     dynamic state enabled
--
-- -   @minDepthBounds@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   @maxDepthBounds@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetDepthBounds :: CommandBuffer ->  CFloat ->  CFloat ->  IO ()
cmdSetDepthBounds = \(CommandBuffer commandBuffer' commandTable) -> \minDepthBounds' -> \maxDepthBounds' -> vkCmdSetDepthBounds commandTable commandBuffer' minDepthBounds' maxDepthBounds' *> (pure ())


-- | vkCmdSetEvent - Set an event object to signaled state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @event@ is the event that will be signaled.
--
-- -   @stageMask@ specifies the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>
--     used to determine when the @event@ is signaled.
--
-- = Description
--
-- When 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' is
-- submitted to a queue, it defines an execution dependency on commands
-- that were submitted before it, and defines an event signal operation
-- which sets the event to the signaled state.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @stageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes only the event signal operation.
--
-- If @event@ is already in the signaled state when
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' is
-- executed on the device, then
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' has no
-- effect, no event signal operation occurs, and no execution dependency is
-- generated.
--
-- == Valid Usage
--
-- -   @stageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @event@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Event.VkEvent'
--     handle
--
-- -   @stageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @stageMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @event@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags'
cmdSetEvent :: CommandBuffer ->  Event ->  PipelineStageFlags ->  IO ()
cmdSetEvent = \(CommandBuffer commandBuffer' commandTable) -> \event' -> \stageMask' -> vkCmdSetEvent commandTable commandBuffer' event' stageMask' *> (pure ())


-- | vkCmdSetLineWidth - Set the dynamic line width state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @lineWidth@ is the width of rasterized line segments.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_LINE_WIDTH'
--     dynamic state enabled
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-wideLines wide lines>
--     feature is not enabled, @lineWidth@ /must/ be @1.0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'
cmdSetLineWidth :: CommandBuffer ->  CFloat ->  IO ()
cmdSetLineWidth = \(CommandBuffer commandBuffer' commandTable) -> \lineWidth' -> vkCmdSetLineWidth commandTable commandBuffer' lineWidth' *> (pure ())


-- | vkCmdSetScissor - Set the dynamic scissor rectangles on a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstScissor@ is the index of the first scissor whose state is
--     updated by the command.
--
-- -   @scissorCount@ is the number of scissors whose rectangles are
--     updated by the command.
--
-- -   @pScissors@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures defining
--     scissor rectangles.
--
-- = Description
--
-- The scissor rectangles taken from element i of @pScissors@ replace the
-- current state for the scissor index @firstScissor@ + i, for i in [0,
-- @scissorCount@).
--
-- Each scissor rectangle is described by a
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structure, with the
-- @offset.x@ and @offset.y@ values determining the upper left corner of
-- the scissor rectangle, and the @extent.width@ and @extent.height@ values
-- determining the size in pixels.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_SCISSOR' dynamic
--     state enabled
--
-- -   @firstScissor@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstScissor@ and @scissorCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstScissor@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @scissorCount@ /must/ be @1@
--
-- -   The @x@ and @y@ members of @offset@ /must/ be greater than or equal
--     to @0@
--
-- -   Evaluation of (@offset.x@ + @extent.width@) /must/ not cause a
--     signed integer addition overflow
--
-- -   Evaluation of (@offset.y@ + @extent.height@) /must/ not cause a
--     signed integer addition overflow
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pScissors@ /must/ be a valid pointer to an array of @scissorCount@
--     'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D' structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   @scissorCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkRect2D'
cmdSetScissor :: CommandBuffer ->  Word32 ->  Vector Rect2D ->  IO ()
cmdSetScissor = \(CommandBuffer commandBuffer' commandTable) -> \firstScissor' -> \scissors' -> withVec withCStructRect2D scissors' (\pScissors' -> vkCmdSetScissor commandTable commandBuffer' firstScissor' (fromIntegral $ Data.Vector.length scissors') pScissors' *> (pure ()))


-- | vkCmdSetStencilCompareMask - Set the stencil compare mask dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'
--     specifying the set of stencil state for which to update the compare
--     mask.
--
-- -   @compareMask@ is the new value to use as the stencil compare mask.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK'
--     dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags'
cmdSetStencilCompareMask :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilCompareMask = \(CommandBuffer commandBuffer' commandTable) -> \faceMask' -> \compareMask' -> vkCmdSetStencilCompareMask commandTable commandBuffer' faceMask' compareMask' *> (pure ())


-- | vkCmdSetStencilReference - Set the stencil reference dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'
--     specifying the set of stencil state for which to update the
--     reference value, as described above for
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask'.
--
-- -   @reference@ is the new value to use as the stencil reference value.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_STENCIL_REFERENCE'
--     dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags'
cmdSetStencilReference :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilReference = \(CommandBuffer commandBuffer' commandTable) -> \faceMask' -> \reference' -> vkCmdSetStencilReference commandTable commandBuffer' faceMask' reference' *> (pure ())


-- | vkCmdSetStencilWriteMask - Set the stencil write mask dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'
--     specifying the set of stencil state for which to update the write
--     mask, as described above for
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetStencilCompareMask'.
--
-- -   @writeMask@ is the new value to use as the stencil write mask.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_STENCIL_WRITE_MASK'
--     dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @faceMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkStencilFaceFlags'
cmdSetStencilWriteMask :: CommandBuffer ->  StencilFaceFlags ->  Word32 ->  IO ()
cmdSetStencilWriteMask = \(CommandBuffer commandBuffer' commandTable) -> \faceMask' -> \writeMask' -> vkCmdSetStencilWriteMask commandTable commandBuffer' faceMask' writeMask' *> (pure ())


-- | vkCmdSetViewport - Set the viewport on a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @firstViewport@ is the index of the first viewport whose parameters
--     are updated by the command.
--
-- -   @viewportCount@ is the number of viewports whose parameters are
--     updated by the command.
--
-- -   @pViewports@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Pipeline.VkViewport' structures specifying
--     viewport parameters.
--
-- = Description
--
-- The viewport parameters taken from element i of @pViewports@ replace the
-- current state for the viewport index @firstViewport@ + i, for i in [0,
-- @viewportCount@).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_VIEWPORT'
--     dynamic state enabled
--
-- -   @firstViewport@ /must/ be less than
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'::@maxViewports@,
--     inclusive
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pViewports@ /must/ be a valid pointer to an array of
--     @viewportCount@ valid 'Graphics.Vulkan.C.Core10.Pipeline.VkViewport'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics
--     operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkViewport'
cmdSetViewport :: CommandBuffer ->  Word32 ->  Vector Viewport ->  IO ()
cmdSetViewport = \(CommandBuffer commandBuffer' commandTable) -> \firstViewport' -> \viewports' -> withVec withCStructViewport viewports' (\pViewports' -> vkCmdSetViewport commandTable commandBuffer' firstViewport' (fromIntegral $ Data.Vector.length viewports') pViewports' *> (pure ()))


-- | vkCmdUpdateBuffer - Update a buffer’s contents from host memory
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @dstBuffer@ is a handle to the buffer to be updated.
--
-- -   @dstOffset@ is the byte offset into the buffer to start updating,
--     and /must/ be a multiple of 4.
--
-- -   @dataSize@ is the number of bytes to update, and /must/ be a
--     multiple of 4.
--
-- -   @pData@ is a pointer to the source data for the buffer update, and
--     /must/ be at least @dataSize@ bytes in size.
--
-- = Description
--
-- @dataSize@ /must/ be less than or equal to 65536 bytes. For larger
-- updates, applications /can/ use buffer to buffer
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies-buffers copies>.
--
-- __Note__
--
-- Buffer updates performed with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer' first
-- copy the data into command buffer memory when the command is recorded
-- (which requires additional storage and may incur an additional
-- allocation), and then copy the data from the command buffer into
-- @dstBuffer@ when the command is executed on a device.
--
-- The additional cost of this functionality compared to
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies-buffers buffer to buffer copies>
-- means it is only recommended for very small amounts of data, and is why
-- it is limited to only 65536 bytes.
--
-- Applications /can/ work around this by issuing multiple
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer'
-- commands to different ranges of the same buffer, but it is strongly
-- recommended that they /should/ not.
--
-- The source data is copied from the user pointer to the command buffer
-- when the command is called.
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer' is
-- only allowed outside of a render pass. This command is treated as
-- “transfer” operation, for the purposes of synchronization barriers. The
-- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
-- /must/ be specified in @usage@ of
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' in order for the
-- buffer to be compatible with
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer'.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @dataSize@ /must/ be less than or equal to the size of @dstBuffer@
--     minus @dstOffset@
--
-- -   @dstBuffer@ /must/ have been created with
--     'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_TRANSFER_DST_BIT'
--     usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single
--     'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- -   @dataSize@ /must/ be less than or equal to @65536@
--
-- -   @dataSize@ /must/ be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @dstBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer' handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize'
cmdUpdateBuffer :: (Storable a) => CommandBuffer ->  Buffer ->  DeviceSize ->  Vector a ->  IO ()
cmdUpdateBuffer = \(CommandBuffer commandBuffer' commandTable) -> \dstBuffer' -> \dstOffset' -> \data' -> withVec (&) data' (\pData' -> vkCmdUpdateBuffer commandTable commandBuffer' dstBuffer' dstOffset' (fromIntegral $ sizeOf (Data.Vector.head data') * Data.Vector.length data') (castPtr pData') *> (pure ()))


-- | vkCmdWaitEvents - Wait for one or more events and insert a set of memory
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @eventCount@ is the length of the @pEvents@ array.
--
-- -   @pEvents@ is an array of event object handles to wait on.
--
-- -   @srcStageMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages source stage mask>.
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages destination stage mask>.
--
-- -   @memoryBarrierCount@ is the length of the @pMemoryBarriers@ array.
--
-- -   @pMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier'
--     structures.
--
-- -   @bufferMemoryBarrierCount@ is the length of the
--     @pBufferMemoryBarriers@ array.
--
-- -   @pBufferMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier'
--     structures.
--
-- -   @imageMemoryBarrierCount@ is the length of the
--     @pImageMemoryBarriers@ array.
--
-- -   @pImageMemoryBarriers@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
--     structures.
--
-- = Description
--
-- When 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents' is
-- submitted to a queue, it defines a memory dependency between prior event
-- signal operations on the same queue or the host, and subsequent
-- commands.
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents' /must/
-- not be used to wait on event signal operations occurring on other
-- queues.
--
-- The first synchronization scope only includes event signal operations
-- that operate on members of @pEvents@, and the operations that
-- happened-before the event signal operations. Event signal operations
-- performed by
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' that
-- occur earlier in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>
-- are included in the first synchronization scope, if the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in their @stageMask@ parameter is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically earlier>
-- than or equal to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in @srcStageMask@. Event signal operations performed by
-- 'Graphics.Vulkan.C.Core10.Event.vkSetEvent' are only included in the
-- first synchronization scope if
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT' is included
-- in @srcStageMask@.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>.
-- The second synchronization scope is limited to operations on the
-- pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- __Note__
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents' is used
-- with 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' to
-- define a memory dependency between two sets of action commands, roughly
-- in the same way as pipeline barriers, but split into two commands such
-- that work between the two /may/ execute unhindered.
--
-- __Note__
--
-- Applications /should/ be careful to avoid race conditions when using
-- events. There is no direct ordering guarantee between a
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' command
-- and a 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
-- command submitted after it, so some other execution dependency /must/ be
-- included between these commands (e.g. a semaphore).
--
-- == Valid Usage
--
-- -   @srcStageMask@ /must/ be the bitwise OR of the @stageMask@ parameter
--     used in previous calls to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' with
--     any of the members of @pEvents@ and
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT' if any
--     of the members of @pEvents@ was set using
--     'Graphics.Vulkan.C.Core10.Event.vkSetEvent'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   If @pEvents@ includes one or more events that will be signaled by
--     'Graphics.Vulkan.C.Core10.Event.vkSetEvent' after @commandBuffer@
--     has been submitted to a queue, then
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
--     /must/ not be called inside a render pass instance
--
-- -   Any pipeline stage included in @srcStageMask@ or @dstStageMask@
--     /must/ be supported by the capabilities of the queue family
--     specified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo'
--     structure that was used to create the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-access-types-supported table of supported access types>.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pEvents@ /must/ be a valid pointer to an array of @eventCount@
--     valid 'Graphics.Vulkan.C.Core10.Event.VkEvent' handles
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   If @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a
--     valid pointer to an array of @memoryBarrierCount@ valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier'
--     structures
--
-- -   If @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier'
--     structures
--
-- -   If @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@
--     /must/ be a valid pointer to an array of @imageMemoryBarrierCount@
--     valid
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier'
--     structures
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support graphics, or
--     compute operations
--
-- -   @eventCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pEvents@ /must/ have
--     been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags'
cmdWaitEvents :: CommandBuffer ->  Vector Event ->  PipelineStageFlags ->  PipelineStageFlags ->  Vector MemoryBarrier ->  Vector BufferMemoryBarrier ->  Vector ImageMemoryBarrier ->  IO ()
cmdWaitEvents = \(CommandBuffer commandBuffer' commandTable) -> \events' -> \srcStageMask' -> \dstStageMask' -> \memoryBarriers' -> \bufferMemoryBarriers' -> \imageMemoryBarriers' -> withVec withCStructImageMemoryBarrier imageMemoryBarriers' (\pImageMemoryBarriers' -> withVec withCStructBufferMemoryBarrier bufferMemoryBarriers' (\pBufferMemoryBarriers' -> withVec withCStructMemoryBarrier memoryBarriers' (\pMemoryBarriers' -> withVec (&) events' (\pEvents' -> vkCmdWaitEvents commandTable commandBuffer' (fromIntegral $ Data.Vector.length events') pEvents' srcStageMask' dstStageMask' (fromIntegral $ Data.Vector.length memoryBarriers') pMemoryBarriers' (fromIntegral $ Data.Vector.length bufferMemoryBarriers') pBufferMemoryBarriers' (fromIntegral $ Data.Vector.length imageMemoryBarriers') pImageMemoryBarriers' *> (pure ())))))


-- | vkCmdWriteTimestamp - Write a device timestamp into a query object
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pipelineStage@ is one of the
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits', specifying
--     a stage of the pipeline.
--
-- -   @queryPool@ is the query pool that will manage the timestamp.
--
-- -   @query@ is the query within the query pool that will contain the
--     timestamp.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'
-- latches the value of the timer when all previous commands have completed
-- executing as far as the specified pipeline stage, and writes the
-- timestamp value to memory. When the timestamp value is written, the
-- availability status of the query is set to available.
--
-- __Note__
--
-- If an implementation is unable to detect completion and latch the timer
-- at any specific stage of the pipeline, it /may/ instead do so at any
-- logically later stage.
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'
-- /can/ then be called to copy the timestamp value from the query pool
-- into buffer memory, with ordering and synchronization behavior
-- equivalent to how other queries operate. Timestamp values /can/ also be
-- retrieved from the query pool using
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults'. As with other
-- queries, the query /must/ be reset using
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool'
-- before requesting the timestamp value be written to it.
--
-- While
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'
-- /can/ be called inside or outside of a render pass instance,
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'
-- /must/ only be called outside of a render pass instance.
--
-- Timestamps /may/ only be meaningfully compared if they are written by
-- commands submitted to the same queue.
--
-- __Note__
--
-- An example of such a comparison is determining the execution time of a
-- sequence of commands.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ of
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_TIMESTAMP'
--
-- -   The query identified by @queryPool@ and @query@ /must/ be
--     /unavailable/
--
-- -   The command pool’s queue family /must/ support a non-zero
--     @timestampValidBits@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer' handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits' value
--
-- -   @queryPool@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle
--
-- -   @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   The 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ support transfer,
--     graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <https://www.kh | <https://www.kh | <https://www.kh | <https://www.kh |
-- > | ronos.org/regis | ronos.org/regis | ronos.org/regis | ronos.org/regis |
-- > | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec | try/vulkan/spec |
-- > | s/1.0-extension | s/1.0-extension | s/1.0-extension | s/1.0-extension |
-- > | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h | s/html/vkspec.h |
-- > | tml#VkCommandBu | tml#vkCmdBeginR | tml#VkQueueFlag | tml#synchroniza |
-- > | fferLevel Comma | enderPass Rende | Bits Supported  | tion-pipeline-s |
-- > | nd Buffer Level | r Pass Scope>   | Queue Types>    | tages-types Pip |
-- > | s>              |                 |                 | eline Type>     |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
cmdWriteTimestamp :: CommandBuffer ->  PipelineStageFlagBits ->  QueryPool ->  Word32 ->  IO ()
cmdWriteTimestamp = \(CommandBuffer commandBuffer' commandTable) -> \pipelineStage' -> \queryPool' -> \query' -> vkCmdWriteTimestamp commandTable commandBuffer' pipelineStage' queryPool' query' *> (pure ())
