{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language LambdaCase #-}

module Graphics.Vulkan.Core10.CommandBufferBuilding
  ( VkIndexType(..)
  , pattern VK_INDEX_TYPE_UINT16
  , pattern VK_INDEX_TYPE_UINT32
  , VkSubpassContents(..)
  , pattern VK_SUBPASS_CONTENTS_INLINE
  , pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS
  , VkStencilFaceFlagBits(..)
  , pattern VK_STENCIL_FACE_FRONT_BIT
  , pattern VK_STENCIL_FACE_BACK_BIT
  , pattern VK_STENCIL_FRONT_AND_BACK
  , vkCmdBindPipeline
  , vkCmdSetViewport
  , vkCmdSetScissor
  , vkCmdSetLineWidth
  , vkCmdSetDepthBias
  , vkCmdSetBlendConstants
  , vkCmdSetDepthBounds
  , vkCmdSetStencilCompareMask
  , vkCmdSetStencilWriteMask
  , vkCmdSetStencilReference
  , vkCmdBindDescriptorSets
  , vkCmdBindIndexBuffer
  , vkCmdBindVertexBuffers
  , vkCmdDraw
  , vkCmdDrawIndexed
  , vkCmdDrawIndirect
  , vkCmdDrawIndexedIndirect
  , vkCmdDispatch
  , vkCmdDispatchIndirect
  , vkCmdCopyBuffer
  , vkCmdCopyImage
  , vkCmdBlitImage
  , vkCmdCopyBufferToImage
  , vkCmdCopyImageToBuffer
  , vkCmdUpdateBuffer
  , vkCmdFillBuffer
  , vkCmdClearColorImage
  , vkCmdClearDepthStencilImage
  , vkCmdClearAttachments
  , vkCmdResolveImage
  , vkCmdSetEvent
  , vkCmdResetEvent
  , vkCmdWaitEvents
  , vkCmdPipelineBarrier
  , vkCmdBeginQuery
  , vkCmdEndQuery
  , vkCmdResetQueryPool
  , vkCmdWriteTimestamp
  , vkCmdCopyQueryPoolResults
  , vkCmdPushConstants
  , vkCmdBeginRenderPass
  , vkCmdNextSubpass
  , vkCmdEndRenderPass
  , vkCmdExecuteCommands
  , VkClearRect(..)
  , VkImageSubresourceLayers(..)
  , VkMemoryBarrier(..)
  , VkBufferMemoryBarrier(..)
  , VkImageMemoryBarrier(..)
  , VkBufferCopy(..)
  , VkImageCopy(..)
  , VkImageBlit(..)
  , VkBufferImageCopy(..)
  , VkImageResolve(..)
  , VkRenderPassBeginInfo(..)
  , VkClearDepthStencilValue(..)
  , VkClearAttachment(..)
  , VkDrawIndirectCommand(..)
  , VkDrawIndexedIndirectCommand(..)
  , VkDispatchIndirectCommand(..)
  , VkClearColorValue(..)
  , VkClearValue(..)
  , VkStencilFaceFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.CommandBuffer
  ( VkQueryControlFlagBits(..)
  , VkQueryControlFlags
  )
import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkDescriptorSet
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkExtent3D(..)
  , VkDeviceSize
  )
import Graphics.Vulkan.Core10.Event
  ( VkEvent
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageLayout(..)
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkImageSubresourceRange(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.Core10.Pass
  ( VkDependencyFlagBits(..)
  , VkPipelineBindPoint(..)
  , VkAccessFlags
  , VkDependencyFlags
  , VkFramebuffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkRect2D(..)
  , VkShaderStageFlagBits(..)
  , VkViewport(..)
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Query
  ( VkQueryResultFlagBits(..)
  , VkQueryPool
  , VkQueryResultFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkCommandBuffer
  , VkPipelineStageFlags
  )
import Graphics.Vulkan.Core10.Sampler
  ( VkFilter(..)
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkOffset3D(..)
  , VkImageAspectFlags
  )


-- ** VkIndexType

-- | VkIndexType - Type of index buffer indices
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableIndexBufferEntryNVX',
-- 'vkCmdBindIndexBuffer'
newtype VkIndexType = VkIndexType Int32
  deriving (Eq, Ord, Storable)

instance Show VkIndexType where
  showsPrec _ VK_INDEX_TYPE_UINT16 = showString "VK_INDEX_TYPE_UINT16"
  showsPrec _ VK_INDEX_TYPE_UINT32 = showString "VK_INDEX_TYPE_UINT32"
  showsPrec p (VkIndexType x) = showParen (p >= 11) (showString "VkIndexType " . showsPrec 11 x)

instance Read VkIndexType where
  readPrec = parens ( choose [ ("VK_INDEX_TYPE_UINT16", pure VK_INDEX_TYPE_UINT16)
                             , ("VK_INDEX_TYPE_UINT32", pure VK_INDEX_TYPE_UINT32)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkIndexType")
                        v <- step readPrec
                        pure (VkIndexType v)
                        )
                    )

-- | @VK_INDEX_TYPE_UINT16@ specifies that indices are 16-bit unsigned
-- integer values.
pattern VK_INDEX_TYPE_UINT16 :: VkIndexType
pattern VK_INDEX_TYPE_UINT16 = VkIndexType 0

-- | @VK_INDEX_TYPE_UINT32@ specifies that indices are 32-bit unsigned
-- integer values.
pattern VK_INDEX_TYPE_UINT32 :: VkIndexType
pattern VK_INDEX_TYPE_UINT32 = VkIndexType 1
-- ** VkSubpassContents

-- | VkSubpassContents - Specify how commands in the first subpass of a
-- render pass are provided
--
-- = See Also
--
-- 'vkCmdBeginRenderPass', 'vkCmdNextSubpass'
newtype VkSubpassContents = VkSubpassContents Int32
  deriving (Eq, Ord, Storable)

instance Show VkSubpassContents where
  showsPrec _ VK_SUBPASS_CONTENTS_INLINE = showString "VK_SUBPASS_CONTENTS_INLINE"
  showsPrec _ VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = showString "VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS"
  showsPrec p (VkSubpassContents x) = showParen (p >= 11) (showString "VkSubpassContents " . showsPrec 11 x)

instance Read VkSubpassContents where
  readPrec = parens ( choose [ ("VK_SUBPASS_CONTENTS_INLINE",                    pure VK_SUBPASS_CONTENTS_INLINE)
                             , ("VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS", pure VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSubpassContents")
                        v <- step readPrec
                        pure (VkSubpassContents v)
                        )
                    )

-- | @VK_SUBPASS_CONTENTS_INLINE@ specifies that the contents of the subpass
-- will be recorded inline in the primary command buffer, and secondary
-- command buffers /must/ not be executed within the subpass.
pattern VK_SUBPASS_CONTENTS_INLINE :: VkSubpassContents
pattern VK_SUBPASS_CONTENTS_INLINE = VkSubpassContents 0

-- | @VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS@ specifies that the
-- contents are recorded in secondary command buffers that will be called
-- from the primary command buffer, and 'vkCmdExecuteCommands' is the only
-- valid command on the command buffer until 'vkCmdNextSubpass' or
-- 'vkCmdEndRenderPass'.
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS :: VkSubpassContents
pattern VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS = VkSubpassContents 1
-- ** VkStencilFaceFlagBits

-- | VkStencilFaceFlagBits - Bitmask specifying sets of stencil state for
-- which to update the compare mask
--
-- = See Also
--
-- 'VkStencilFaceFlags'
newtype VkStencilFaceFlagBits = VkStencilFaceFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkStencilFaceFlagBits where
  showsPrec _ VK_STENCIL_FACE_FRONT_BIT = showString "VK_STENCIL_FACE_FRONT_BIT"
  showsPrec _ VK_STENCIL_FACE_BACK_BIT = showString "VK_STENCIL_FACE_BACK_BIT"
  showsPrec _ VK_STENCIL_FRONT_AND_BACK = showString "VK_STENCIL_FRONT_AND_BACK"
  showsPrec p (VkStencilFaceFlagBits x) = showParen (p >= 11) (showString "VkStencilFaceFlagBits " . showsPrec 11 x)

instance Read VkStencilFaceFlagBits where
  readPrec = parens ( choose [ ("VK_STENCIL_FACE_FRONT_BIT", pure VK_STENCIL_FACE_FRONT_BIT)
                             , ("VK_STENCIL_FACE_BACK_BIT",  pure VK_STENCIL_FACE_BACK_BIT)
                             , ("VK_STENCIL_FRONT_AND_BACK", pure VK_STENCIL_FRONT_AND_BACK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStencilFaceFlagBits")
                        v <- step readPrec
                        pure (VkStencilFaceFlagBits v)
                        )
                    )

-- | @VK_STENCIL_FACE_FRONT_BIT@ specifies that only the front set of stencil
-- state is updated.
pattern VK_STENCIL_FACE_FRONT_BIT :: VkStencilFaceFlagBits
pattern VK_STENCIL_FACE_FRONT_BIT = VkStencilFaceFlagBits 0x00000001

-- | @VK_STENCIL_FACE_BACK_BIT@ specifies that only the back set of stencil
-- state is updated.
pattern VK_STENCIL_FACE_BACK_BIT :: VkStencilFaceFlagBits
pattern VK_STENCIL_FACE_BACK_BIT = VkStencilFaceFlagBits 0x00000002

-- | @VK_STENCIL_FRONT_AND_BACK@ is the combination of
-- @VK_STENCIL_FACE_FRONT_BIT@ and @VK_STENCIL_FACE_BACK_BIT@, and
-- specifies that both sets of stencil state are updated.
pattern VK_STENCIL_FRONT_AND_BACK :: VkStencilFaceFlagBits
pattern VK_STENCIL_FRONT_AND_BACK = VkStencilFaceFlagBits 0x00000003
-- | vkCmdBindPipeline - Bind a pipeline object to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the pipeline will be
--     bound to.
--
-- -   @pipelineBindPoint@ is a
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' value specifying
--     whether to bind to the compute or graphics bind point. Binding one
--     does not disturb the other.
--
-- -   @pipeline@ is the pipeline to be bound.
--
-- = Description
--
-- Once bound, a pipeline binding affects subsequent graphics or compute
-- commands in the command buffer until a different pipeline is bound to
-- the bind point. The pipeline bound to @VK_PIPELINE_BIND_POINT_COMPUTE@
-- controls the behavior of 'vkCmdDispatch' and 'vkCmdDispatchIndirect'.
-- The pipeline bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ controls the
-- behavior of all [drawing
-- commands](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing).
-- No other commands are affected by the pipeline state.
--
-- == Valid Usage
--
-- -   If @pipelineBindPoint@ is @VK_PIPELINE_BIND_POINT_COMPUTE@, the
--     @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support compute operations
--
-- -   If @pipelineBindPoint@ is @VK_PIPELINE_BIND_POINT_GRAPHICS@, the
--     @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   If @pipelineBindPoint@ is @VK_PIPELINE_BIND_POINT_COMPUTE@,
--     @pipeline@ /must/ be a compute pipeline
--
-- -   If @pipelineBindPoint@ is @VK_PIPELINE_BIND_POINT_GRAPHICS@,
--     @pipeline@ /must/ be a graphics pipeline
--
-- -   If the [variable multisample
--     rate](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-variableMultisampleRate)
--     feature is not supported, @pipeline@ is a graphics pipeline, the
--     current subpass has no attachments, and this is not the first call
--     to this function with a graphics pipeline after transitioning to the
--     current subpass, then the sample count specified by this pipeline
--     /must/ match that set in the previous pipeline
--
-- -   If
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT'::@variableSampleLocations@
--     is @VK_FALSE@, and @pipeline@ is a graphics pipeline created with a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     structure having its @sampleLocationsEnable@ member set to @VK_TRUE@
--     but without @VK_DYNAMIC_STATE_SAMPLE_LOCATIONS_EXT@ enabled then the
--     current render pass instance /must/ have been begun by specifying a
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkRenderPassSampleLocationsBeginInfoEXT'
--     structure whose @pPostSubpassSampleLocations@ member contains an
--     element with a @subpassIndex@ matching the current subpass index and
--     the @sampleLocationsInfo@ member of that element /must/ match the
--     @sampleLocationsInfo@ specified in
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'
--     when the pipeline was created
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @pipeline@ /must/ be a valid @VkPipeline@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @pipeline@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindPipeline" vkCmdBindPipeline :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()
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
--     'Graphics.Vulkan.Core10.Pipeline.VkViewport' structures specifying
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
--     @VK_DYNAMIC_STATE_VIEWPORT@ dynamic state enabled
--
-- -   @firstViewport@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxViewports@
--
-- -   The sum of @firstViewport@ and @viewportCount@ /must/ be between @1@
--     and @VkPhysicalDeviceLimits@::@maxViewports@, inclusive
--
-- -   If the [multiple
--     viewports](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiViewport)
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   If the [multiple
--     viewports](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiViewport)
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pViewports@ /must/ be a valid pointer to an array of
--     @viewportCount@ @VkViewport@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   @viewportCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkViewport'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetViewport" vkCmdSetViewport :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()
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
--     'Graphics.Vulkan.Core10.Pipeline.VkRect2D' structures defining
--     scissor rectangles.
--
-- = Description
--
-- The scissor rectangles taken from element i of @pScissors@ replace the
-- current state for the scissor index @firstScissor@ + i, for i in [0,
-- @scissorCount@).
--
-- Each scissor rectangle is described by a
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D' structure, with the
-- @offset.x@ and @offset.y@ values determining the upper left corner of
-- the scissor rectangle, and the @extent.width@ and @extent.height@ values
-- determining the size in pixels.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_SCISSOR@ dynamic state enabled
--
-- -   @firstScissor@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxViewports@
--
-- -   The sum of @firstScissor@ and @scissorCount@ /must/ be between @1@
--     and @VkPhysicalDeviceLimits@::@maxViewports@, inclusive
--
-- -   If the [multiple
--     viewports](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiViewport)
--     feature is not enabled, @firstScissor@ /must/ be @0@
--
-- -   If the [multiple
--     viewports](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiViewport)
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
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pScissors@ /must/ be a valid pointer to an array of @scissorCount@
--     @VkRect2D@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   @scissorCount@ /must/ be greater than @0@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetScissor" vkCmdSetScissor :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()
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
--     @VK_DYNAMIC_STATE_LINE_WIDTH@ dynamic state enabled
--
-- -   If the [wide
--     lines](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-wideLines)
--     feature is not enabled, @lineWidth@ /must/ be @1.0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetLineWidth" vkCmdSetLineWidth :: ("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()
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
-- If @depthBiasEnable@ is @VK_FALSE@, no depth bias is applied and the
-- fragment’s depth values are unchanged.
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
-- differ by $r$, will have distinct depth values.
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
-- If a triangle is rasterized using the
-- @VK_POLYGON_MODE_FILL_RECTANGLE_NV@ polygon mode, then this minimum
-- resolvable difference /may/ not be resolvable for samples outside of the
-- triangle, where the depth is extrapolated.
--
-- If no depth buffer is present, r is undefined.
--
-- The bias value o for a polygon is
--
-- \[o =
-- \begin{cases}
--     m \times depthBiasSlopeFactor +
--          r \times depthBiasConstantFactor  & depthBiasClamp = 0\ or\ NaN \\
--     \min(m \times depthBiasSlopeFactor +
--          r \times depthBiasConstantFactor,
--          depthBiasClamp)                   & depthBiasClamp > 0  \\
--     \max(m \times depthBiasSlopeFactor +
--          r \times depthBiasConstantFactor,
--          depthBiasClamp)                   & depthBiasClamp < 0  \\
-- \end{cases}\]
--
-- m is computed as described above. If the depth buffer uses a fixed-point
-- representation, m is a function of depth values in the range [0,1], and
-- o is applied to depth values in the same range.
--
-- For fixed-point depth buffers, fragment depth values are always limited
-- to the range [0,1] by clamping after depth bias addition is performed.
-- Unless the @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
-- extension is enabled, fragment depth values are clamped even when the
-- depth buffer uses a floating-point representation.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_DEPTH_BIAS@ dynamic state enabled
--
-- -   If the [depth bias
--     clamping](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-depthBiasClamp)
--     feature is not enabled, @depthBiasClamp@ /must/ be @0.0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDepthBias" vkCmdSetDepthBias :: ("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()
-- | vkCmdSetBlendConstants - Set the values of blend constants
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @blendConstants@ is an array of four values specifying the R, G, B,
--     and A components of the blend constant color used in blending,
--     depending on the [blend
--     factor](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#framebuffer-blendfactors).
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_BLEND_CONSTANTS@ dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetBlendConstants" vkCmdSetBlendConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()
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
--     @VK_DYNAMIC_STATE_DEPTH_BOUNDS@ dynamic state enabled
--
-- -   Unless the @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
--     extension is enabled @minDepthBounds@ /must/ be between @0.0@ and
--     @1.0@, inclusive
--
-- -   Unless the @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
--     extension is enabled @maxDepthBounds@ /must/ be between @0.0@ and
--     @1.0@, inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetDepthBounds" vkCmdSetDepthBounds :: ("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()
-- | vkCmdSetStencilCompareMask - Set the stencil compare mask dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of 'VkStencilFaceFlagBits' specifying the
--     set of stencil state for which to update the compare mask.
--
-- -   @compareMask@ is the new value to use as the stencil compare mask.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_STENCIL_COMPARE_MASK@ dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @faceMask@ /must/ be a valid combination of 'VkStencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkStencilFaceFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()
-- | vkCmdSetStencilWriteMask - Set the stencil write mask dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of 'VkStencilFaceFlagBits' specifying the
--     set of stencil state for which to update the write mask, as
--     described above for 'vkCmdSetStencilCompareMask'.
--
-- -   @writeMask@ is the new value to use as the stencil write mask.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_STENCIL_WRITE_MASK@ dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @faceMask@ /must/ be a valid combination of 'VkStencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkStencilFaceFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()
-- | vkCmdSetStencilReference - Set the stencil reference dynamic state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of 'VkStencilFaceFlagBits' specifying the
--     set of stencil state for which to update the reference value, as
--     described above for 'vkCmdSetStencilCompareMask'.
--
-- -   @reference@ is the new value to use as the stencil reference value.
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_STENCIL_REFERENCE@ dynamic state enabled
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @faceMask@ /must/ be a valid combination of 'VkStencilFaceFlagBits'
--     values
--
-- -   @faceMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkStencilFaceFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetStencilReference" vkCmdSetStencilReference :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()
-- | vkCmdBindDescriptorSets - Binds descriptor sets to a command buffer
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer that the descriptor sets will
--     be bound to.
--
-- -   @pipelineBindPoint@ is a
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' indicating whether
--     the descriptors will be used by graphics pipelines or compute
--     pipelines. There is a separate set of bind points for each of
--     graphics and compute, so binding one does not disturb the other.
--
-- -   @layout@ is a @VkPipelineLayout@ object used to program the
--     bindings.
--
-- -   @firstSet@ is the set number of the first descriptor set to be
--     bound.
--
-- -   @descriptorSetCount@ is the number of elements in the
--     @pDescriptorSets@ array.
--
-- -   @pDescriptorSets@ is an array of handles to @VkDescriptorSet@
--     objects describing the descriptor sets to write to.
--
-- -   @dynamicOffsetCount@ is the number of dynamic offsets in the
--     @pDynamicOffsets@ array.
--
-- -   @pDynamicOffsets@ is a pointer to an array of @uint32_t@ values
--     specifying dynamic offsets.
--
-- = Description
--
-- @vkCmdBindDescriptorSets@ causes the sets numbered [@firstSet@..
-- @firstSet@+@descriptorSetCount@-1] to use the bindings stored in
-- @pDescriptorSets@[0..@descriptorSetCount@-1] for subsequent rendering
-- commands (either compute or graphics, according to the
-- @pipelineBindPoint@). Any bindings that were previously applied via
-- these sets are no longer valid.
--
-- Once bound, a descriptor set affects rendering of subsequent graphics or
-- compute commands in the command buffer until a different set is bound to
-- the same set number, or else until the set is disturbed as described in
-- [Pipeline Layout
-- Compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility).
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
-- set. The length of the dynamic uniform and storage buffer bindings is
-- the buffer range as specified in the descriptor set.
--
-- Each of the @pDescriptorSets@ /must/ be compatible with the pipeline
-- layout specified by @layout@. The layout used to program the bindings
-- /must/ also be compatible with the pipeline used in subsequent graphics
-- or compute commands, as defined in the [Pipeline Layout
-- Compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
-- section.
--
-- The descriptor set contents bound by a call to @vkCmdBindDescriptorSets@
-- /may/ be consumed at the following times:
--
-- -   For descriptor bindings created with the
--     @VK_DESCRIPTOR_BINDING_UPDATE_AFTER_BIND_BIT_EXT@ bit set, the
--     contents /may/ be consumed when the command buffer is submitted to a
--     queue, or during shader execution of the resulting draws and
--     dispatches, or any time in between. Otherwise,
--
-- -   during host execution of the command, or during shader execution of
--     the resulting draws and dispatches, or any time in between.
--
-- Thus, the contents of a descriptor set binding /must/ not be altered
-- (overwritten by an update command, or freed) between the first point in
-- time that it /may/ be consumed, and and when the command completes
-- executing on the queue.
--
-- The contents of @pDynamicOffsets@ are consumed immediately during
-- execution of @vkCmdBindDescriptorSets@. Once all pending uses have
-- completed, it is legal to update and reuse a descriptor set.
--
-- == Valid Usage
--
-- -   Each element of @pDescriptorSets@ /must/ have been allocated with a
--     @VkDescriptorSetLayout@ that matches (is the same as, or identically
--     defined as) the @VkDescriptorSetLayout@ at set /n/ in @layout@,
--     where /n/ is the sum of @firstSet@ and the index into
--     @pDescriptorSets@
--
-- -   @dynamicOffsetCount@ /must/ be equal to the total number of dynamic
--     descriptors in @pDescriptorSets@
--
-- -   The sum of @firstSet@ and @descriptorSetCount@ /must/ be less than
--     or equal to @VkPipelineLayoutCreateInfo@::@setLayoutCount@ provided
--     when @layout@ was created
--
-- -   @pipelineBindPoint@ /must/ be supported by the @commandBuffer@’s
--     parent @VkCommandPool@’s queue family
--
-- -   Each element of @pDynamicOffsets@ /must/ satisfy the required
--     alignment for the corresponding descriptor binding’s descriptor type
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pipelineBindPoint@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint' value
--
-- -   @layout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @pDescriptorSets@ /must/ be a valid pointer to an array of
--     @descriptorSetCount@ valid @VkDescriptorSet@ handles
--
-- -   If @dynamicOffsetCount@ is not @0@, @pDynamicOffsets@ /must/ be a
--     valid pointer to an array of @dynamicOffsetCount@ @uint32_t@ values
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   @descriptorSetCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @layout@, and the elements of
--     @pDescriptorSets@ /must/ have been created, allocated, or retrieved
--     from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()
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
-- -   @indexType@ is a 'VkIndexType' value specifying whether indices are
--     treated as 16 bits or 32 bits.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   The sum of @offset@ and the address of the range of @VkDeviceMemory@
--     object that is backing @buffer@, /must/ be a multiple of the type
--     indicated by @indexType@
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDEX_BUFFER_BIT@ flag
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @indexType@ /must/ be a valid 'VkIndexType' value
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@,
-- 'VkIndexType'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()
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
--     @VkPhysicalDeviceLimits@::@maxVertexInputBindings@
--
-- -   The sum of @firstBinding@ and @bindingCount@ /must/ be less than or
--     equal to @VkPhysicalDeviceLimits@::@maxVertexInputBindings@
--
-- -   All elements of @pOffsets@ /must/ be less than the size of the
--     corresponding element in @pBuffers@
--
-- -   All elements of @pBuffers@ /must/ have been created with the
--     @VK_BUFFER_USAGE_VERTEX_BUFFER_BIT@ flag
--
-- -   Each element of @pBuffers@ that is non-sparse /must/ be bound
--     completely and contiguously to a single @VkDeviceMemory@ object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pBuffers@ /must/ be a valid pointer to an array of @bindingCount@
--     valid @VkBuffer@ handles
--
-- -   @pOffsets@ /must/ be a valid pointer to an array of @bindingCount@
--     @VkDeviceSize@ values
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   @bindingCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pBuffers@ /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()
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
-- -   The current render pass /must/ be
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input)
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDraw" vkCmdDraw :: ("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()
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
-- 'vkCmdBindIndexBuffer'::@indexType@ parameter with which the buffer was
-- bound.
--
-- The first vertex index is at an offset of @firstIndex@ * @indexSize@ +
-- @offset@ within the bound index buffer, where @offset@ is the offset
-- specified by @vkCmdBindIndexBuffer@ and @indexSize@ is the byte size of
-- the type specified by @indexType@. Subsequent index values are retrieved
-- from consecutive locations in the index buffer. Indices are first
-- compared to the primitive restart value, then zero extended to 32 bits
-- (if the @indexType@ is @VK_INDEX_TYPE_UINT16@) and have @vertexOffset@
-- added to them, before being supplied as the @vertexIndex@ value.
--
-- The primitives are drawn @instanceCount@ times with @instanceIndex@
-- starting with @firstInstance@ and increasing sequentially for each
-- instance. The assembled primitives execute the bound graphics pipeline.
--
-- == Valid Usage
--
-- -   The current render pass /must/ be
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input)
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     indexSize being based on the type specified by @indexType@, where
--     the index buffer, @indexType@, and @offset@ are specified via
--     @vkCmdBindIndexBuffer@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexed" vkCmdDrawIndexed :: ("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()
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
-- @vkCmdDrawIndirect@ behaves similarly to 'vkCmdDraw' except that the
-- parameters are read by the device from a buffer during execution.
-- @drawCount@ draws are executed by the command, with parameters taken
-- from @buffer@ starting at @offset@ and increasing by @stride@ bytes for
-- each successive draw. The parameters of each draw are encoded in an
-- array of 'VkDrawIndirectCommand' structures. If @drawCount@ is less than
-- or equal to one, @stride@ is ignored.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@(@VkDrawIndirectCommand@)
--
-- -   If the [multi-draw
--     indirect](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiDrawIndirect)
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   If the
--     [drawIndirectFirstInstance](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-drawIndirectFirstInstance)
--     feature is not enabled, all the @firstInstance@ members of the
--     @VkDrawIndirectCommand@ structures accessed by this command /must/
--     be @0@
--
-- -   The current render pass /must/ be
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@('VkDrawIndirectCommand')) /must/ be less than or equal to
--     the size of @buffer@
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ + @sizeof@('VkDrawIndirectCommand')) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   @drawCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndirect" vkCmdDrawIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
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
-- @vkCmdDrawIndexedIndirect@ behaves similarly to 'vkCmdDrawIndexed'
-- except that the parameters are read by the device from a buffer during
-- execution. @drawCount@ draws are executed by the command, with
-- parameters taken from @buffer@ starting at @offset@ and increasing by
-- @stride@ bytes for each successive draw. The parameters of each draw are
-- encoded in an array of 'VkDrawIndexedIndirectCommand' structures. If
-- @drawCount@ is less than or equal to one, @stride@ is ignored.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   If @drawCount@ is greater than @1@, @stride@ /must/ be a multiple of
--     @4@ and /must/ be greater than or equal to
--     @sizeof@(@VkDrawIndexedIndirectCommand@)
--
-- -   If the [multi-draw
--     indirect](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-multiDrawIndirect)
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   If the
--     [drawIndirectFirstInstance](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-drawIndirectFirstInstance)
--     feature is not enabled, all the @firstInstance@ members of the
--     @VkDrawIndexedIndirectCommand@ structures accessed by this command
--     /must/ be @0@
--
-- -   The current render pass /must/ be
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
--     with the @renderPass@ member of the @VkGraphicsPipelineCreateInfo@
--     structure specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   The subpass index of the current render pass /must/ be equal to the
--     @subpass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@.
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   All vertex input bindings accessed via vertex input variables
--     declared in the vertex shader entry point’s interface /must/ have
--     valid buffers bound
--
-- -   A valid graphics pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_GRAPHICS@
--
-- -   If the @VkPipeline@ object bound to
--     @VK_PIPELINE_BIND_POINT_GRAPHICS@ requires any dynamic state, that
--     state /must/ have been set on the current command buffer
--
-- -   If @drawCount@ is equal to @1@, (@offset@ +
--     @sizeof@(@VkDrawIndexedIndirectCommand@)) /must/ be less than or
--     equal to the size of @buffer@
--
-- -   If @drawCount@ is greater than @1@, (@stride@ × (@drawCount@ - 1) +
--     @offset@ + @sizeof@(@VkDrawIndexedIndirectCommand@)) /must/ be less
--     than or equal to the size of @buffer@
--
-- -   @drawCount@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxDrawIndirectCount@
--
-- -   Every input attachment used by the current subpass /must/ be bound
--     to the pipeline via a descriptor set
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Image subresources used as attachments in the current render pass
--     /must/ not be accessed in any way other than as an attachment by
--     this command.
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- -   If the draw is recorded in a render pass instance with multiview
--     enabled, the maximum instance index /must/ be less than or equal to
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties'::@maxMultiviewInstanceIndex@.
--
-- -   If the bound graphics pipeline was created with
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT'::@sampleLocationsEnable@
--     set to @VK_TRUE@ and the current subpass has a depth\/stencil
--     attachment, then that attachment /must/ have been created with the
--     @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@ bit set
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
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
-- groupCountX × groupCountY × groupCountZ local workgroups is assembled.
--
-- == Valid Usage
--
-- -   @groupCountX@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxComputeWorkGroupCount@[0]
--
-- -   @groupCountY@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxComputeWorkGroupCount@[1]
--
-- -   @groupCountZ@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxComputeWorkGroupCount@[2]
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_COMPUTE@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_COMPUTE@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   A valid compute pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_COMPUTE@
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_COMPUTE@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_COMPUTE@, with a
--     @VkPipelineLayout@ that is compatible for push constants with the
--     one used to create the current @VkPipeline@, as described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Compute                                                                                               | Compute                                                                                                                    |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDispatch" vkCmdDispatch :: ("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
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
-- @vkCmdDispatchIndirect@ behaves similarly to 'vkCmdDispatch' except that
-- the parameters are read by the device from a buffer during execution.
-- The parameters of the dispatch are encoded in a
-- 'VkDispatchIndirectCommand' structure taken from @buffer@ starting at
-- @offset@.
--
-- == Valid Usage
--
-- -   If @buffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   For each set /n/ that is statically used by the @VkPipeline@ bound
--     to @VK_PIPELINE_BIND_POINT_COMPUTE@, a descriptor set /must/ have
--     been bound to /n/ at @VK_PIPELINE_BIND_POINT_COMPUTE@, with a
--     @VkPipelineLayout@ that is compatible for set /n/, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   Descriptors in each bound descriptor set, specified via
--     @vkCmdBindDescriptorSets@, /must/ be valid if they are statically
--     used by the bound @VkPipeline@ object, specified via
--     @vkCmdBindPipeline@
--
-- -   A valid compute pipeline /must/ be bound to the current command
--     buffer with @VK_PIPELINE_BIND_POINT_COMPUTE@
--
-- -   @buffer@ /must/ have been created with the
--     @VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT@ bit set
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   The sum of @offset@ and the size of @VkDispatchIndirectCommand@
--     /must/ be less than or equal to the size of @buffer@
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_COMPUTE@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_COMPUTE@, with a
--     @VkPipelineLayout@ that is compatible for push constants with the
--     one used to create the current @VkPipeline@, as described in
--     [{html_spec_relative}#descriptorsets-compatibility](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#descriptorsets-compatibility)
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ uses
--     unnormalized coordinates, it /must/ not be used to sample from any
--     @VkImage@ with a @VkImageView@ of the type @VK_IMAGE_VIEW_TYPE_3D@,
--     @VK_IMAGE_VIEW_TYPE_CUBE@, @VK_IMAGE_VIEW_TYPE_1D_ARRAY@,
--     @VK_IMAGE_VIEW_TYPE_2D_ARRAY@ or @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@, in
--     any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   If any @VkSampler@ object that is accessed from a shader by the
--     @VkPipeline@ bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ uses
--     unnormalized coordinates, it /must/ not be used with any of the
--     SPIR-V @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the [robust buffer
--     access](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-robustBufferAccess)
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ accesses a storage
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   Any @VkImageView@ being sampled with @VK_FILTER_LINEAR@ as a result
--     of this command /must/ be of a format which supports linear
--     filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ be of
--     a format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   Any 'Graphics.Vulkan.Core10.ImageView.VkImageView' being sampled
--     with @VK_FILTER_CUBIC_IMG@ as a result of this command /must/ not
--     have a 'Graphics.Vulkan.Core10.ImageView.VkImageViewType' of
--     @VK_IMAGE_VIEW_TYPE_3D@, @VK_IMAGE_VIEW_TYPE_CUBE@, or
--     @VK_IMAGE_VIEW_TYPE_CUBE_ARRAY@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @buffer@, and @commandBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Compute                                                                                               | Compute                                                                                                                    |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdDispatchIndirect" vkCmdDispatchIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()
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
-- -   @pRegions@ is a pointer to an array of 'VkBufferCopy' structures
--     specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the source buffer to the same
-- region of the destination buffer. @srcBuffer@ and @dstBuffer@ /can/ be
-- the same buffer or alias the same memory, but the result is undefined if
-- the copy regions overlap in memory.
--
-- == Valid Usage
--
-- -   The @size@ member of each element of @pRegions@ /must/ be greater
--     than @0@
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
--     @VK_BUFFER_USAGE_TRANSFER_SRC_BIT@ usage flag
--
-- -   If @srcBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @dstBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     @VkBufferCopy@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstBuffer@, and @srcBuffer@ /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', 'VkBufferCopy',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyBuffer" vkCmdCopyBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()
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
-- -   @pRegions@ is a pointer to an array of 'VkImageCopy' structures
--     specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the source image to the same
-- region of the destination image. @srcImage@ and @dstImage@ /can/ be the
-- same image or alias the same memory.
--
-- The formats of @srcImage@ and @dstImage@ /must/ be compatible. Formats
-- are considered compatible if their element size is the same between both
-- formats. For example, @VK_FORMAT_R8G8B8A8_UNORM@ is compatible with
-- @VK_FORMAT_R32_UINT@ because both texels are 4 bytes in size.
-- Depth\/stencil formats /must/ match exactly.
--
-- If the format of @srcImage@ or @dstImage@ is a [/multi-planar/ image
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
-- regions of each plane to be copied /must/ be specified separately using
-- the @srcSubresource@ and @dstSubresource@ members of the 'VkImageCopy'
-- structure. In this case, the @aspectMask@ of the @srcSubresource@ or
-- @dstSubresource@ that refers to the multi-planar image /must/ be
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@. For the purposes of @vkCmdCopyImage@,
-- each plane of a multi-planar image is treated as having the format
-- listed in
-- [{html_spec_relative}#features-formats-compatible-planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Graphics.Vulkan.Core10.Core.VkFormat'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- __Note__
--
-- For example, the @VK_IMAGE_ASPECT_PLANE_1_BIT@ plane of a
-- @VK_FORMAT_G8_B8R8_2PLANE_420_UNORM@ image is compatible with an image
-- of format @VK_FORMAT_R8G8_UNORM@ and (less usefully) with the
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@ plane of an image of format
-- @VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16@, as each texel is
-- 2 bytes in size.
--
-- @vkCmdCopyImage@ allows copying between /size-compatible/ compressed and
-- uncompressed internal formats. Formats are size-compatible if the
-- element size of the uncompressed format is equal to the element size
-- (compressed texel block size) of the compressed format. Such a copy does
-- not perform on-the-fly compression or decompression. When copying from
-- an uncompressed format to a compressed format, each texel of
-- uncompressed data of the source image is copied as a raw value to the
-- corresponding compressed texel block of the destination image. When
-- copying from a compressed format to an uncompressed format, each
-- compressed texel block of the source image is copied as a raw value to
-- the corresponding texel of uncompressed data in the destination image.
-- Thus, for example, it is legal to copy between a 128-bit uncompressed
-- format and a compressed format which has a 128-bit sized compressed
-- texel block representing 4×4 texels (using 8 bits per texel), or between
-- a 64-bit uncompressed format and a compressed format which has a 64-bit
-- sized compressed texel block representing 4×4 texels (using 4 bits per
-- texel).
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
-- “@_422@” image formats that are not
-- [/multi-planar/](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
-- are treated as having a 2×1 compressed texel block for the purposes of
-- these rules.
--
-- @vkCmdCopyImage@ /can/ be used to copy image data between multisample
-- images, but both images /must/ have the same number of samples.
--
-- == Valid Usage
--
-- -   The source region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcImage@ if the @srcImage@’s
--     'Graphics.Vulkan.Core10.Core.VkFormat' is not a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     and /must/ be a region that is contained within the plane being
--     copied if the @srcImage@’s 'Graphics.Vulkan.Core10.Core.VkFormat' is
--     a multi-planar format
--
-- -   The destination region specified by each element of @pRegions@
--     /must/ be a region that is contained within @dstImage@ if the
--     @dstImage@’s 'Graphics.Vulkan.Core10.Core.VkFormat' is not a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     and /must/ be a region that is contained within the plane being
--     copied to if the @dstImage@’s 'Graphics.Vulkan.Core10.Core.VkFormat'
--     is a multi-planar format
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_TRANSFER_SRC_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--
-- -   @srcImage@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage flag
--
-- -   If @srcImage@ is non-sparse then the image or /disjoint/ plane to be
--     copied /must/ be bound completely and contiguously to a single
--     @VkDeviceMemory@ object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_GENERAL@, or @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@
--
-- -   @dstImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_TRANSFER_DST_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--
-- -   @dstImage@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstImage@ is non-sparse then the image or /disjoint/ plane that
--     is the destination of the copy /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_GENERAL@, or @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@
--
-- -   If the 'Graphics.Vulkan.Core10.Core.VkFormat' of each of @srcImage@
--     and @dstImage@ is not a [/multi-planar
--     format/](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     the 'Graphics.Vulkan.Core10.Core.VkFormat' of each of @srcImage@ and
--     @dstImage@ /must/ be compatible, as defined
--     [below](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies-images-format-compatibility)
--
-- -   In a copy to or from a plane of a [multi-planar
--     image](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     the 'Graphics.Vulkan.Core10.Core.VkFormat' of the image and plane
--     /must/ be compatible according to [the description of compatible
--     planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
--     for the plane being copied
--
-- -   When a copy is performed to or from an image with a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     the @aspectMask@ of the @srcSubresource@ and\/or @dstSubresource@
--     that refers to the multi-planar image /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@ (with @VK_IMAGE_ASPECT_PLANE_2_BIT@
--     valid only for a 'Graphics.Vulkan.Core10.Core.VkFormat' with three
--     planes)
--
-- -   The sample count of @srcImage@ and @dstImage@ /must/ match
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcOffset@ and and @extent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- -   The @dstOffset@ and and @extent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcImage@ /must/ be a valid @VkImage@ handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @dstImage@ /must/ be a valid @VkImage@ handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid @VkImageCopy@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcImage@ /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage', 'VkImageCopy',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyImage" vkCmdCopyImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()
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
-- -   @pRegions@ is a pointer to an array of 'VkImageBlit' structures
--     specifying the regions to blit.
--
-- -   @filter@ is a 'Graphics.Vulkan.Core10.Sampler.VkFilter' specifying
--     the filter to apply if the blits require scaling.
--
-- = Description
--
-- @vkCmdBlitImage@ /must/ not be used for multisampled source or
-- destination images. Use 'vkCmdResolveImage' for this purpose.
--
-- As the sizes of the source and destination extents /can/ differ in any
-- dimension, texels in the source extent are scaled and filtered to the
-- destination extent. Scaling occurs via the following operations:
--
-- -   For each destination texel, the integer coordinate of that texel is
--     converted to an unnormalized texture coordinate, using the effective
--     inverse of the equations described in [unnormalized to integer
--     conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures-unnormalized-to-integer):
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
-- in [Image Operations
-- chapter](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#textures),
-- with the filter mode equal to that of @filter@, a mipmap mode of
-- @VK_SAMPLER_MIPMAP_MODE_NEAREST@ and an address mode of
-- @VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE@. Implementations /must/ clamp at
-- the edge of the source image, and /may/ additionally clamp to the edge
-- of the source region.
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
-- @srcOffsets@[1].@z@. If the @filter@ parameter is @VK_FILTER_LINEAR@
-- then the value sampled from the source image is taken by doing linear
-- filtering using the interpolated __z__ coordinate. If @filter@ parameter
-- is @VK_FILTER_NEAREST@ then value sampled from the source image is taken
-- from the single nearest slice (with undefined rounding mode).
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
-- -   @srcImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_BLIT_SRC_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   @srcImage@ /must/ not use a format listed in
--     [{html_spec_relative}#features-formats-requiring-sampler-ycbcr-conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--
-- -   @srcImage@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@,
--     @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   @dstImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_BLIT_DST_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   @dstImage@ /must/ not use a format listed in
--     [{html_spec_relative}#features-formats-requiring-sampler-ycbcr-conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--
-- -   @dstImage@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@,
--     @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   The sample count of @srcImage@ and @dstImage@ /must/ both be equal
--     to @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If either of @srcImage@ or @dstImage@ was created with a signed
--     integer 'Graphics.Vulkan.Core10.Core.VkFormat', the other /must/
--     also have been created with a signed integer
--     'Graphics.Vulkan.Core10.Core.VkFormat'
--
-- -   If either of @srcImage@ or @dstImage@ was created with an unsigned
--     integer 'Graphics.Vulkan.Core10.Core.VkFormat', the other /must/
--     also have been created with an unsigned integer
--     'Graphics.Vulkan.Core10.Core.VkFormat'
--
-- -   If either of @srcImage@ or @dstImage@ was created with a
--     depth\/stencil format, the other /must/ have exactly the same format
--
-- -   If @srcImage@ was created with a depth\/stencil format, @filter@
--     /must/ be @VK_FILTER_NEAREST@
--
-- -   @srcImage@ /must/ have been created with a @samples@ value of
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   @dstImage@ /must/ have been created with a @samples@ value of
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If @filter@ is @VK_FILTER_LINEAR@, @srcImage@ /must/ be of a format
--     which supports linear filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   If @filter@ is @VK_FILTER_CUBIC_IMG@, @srcImage@ /must/ be of a
--     format which supports cubic filtering, as specified by the
--     @VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ (for a linear image) or
--     @VkFormatProperties@::@optimalTilingFeatures@(for an optimally tiled
--     image) returned by @vkGetPhysicalDeviceFormatProperties@
--
-- -   If @filter@ is @VK_FILTER_CUBIC_IMG@, @srcImage@ /must/ have a
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType' of
--     @VK_IMAGE_TYPE_3D@
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcImage@ /must/ be a valid @VkImage@ handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @dstImage@ /must/ be a valid @VkImage@ handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid @VkImageBlit@ structures
--
-- -   @filter@ /must/ be a valid 'Graphics.Vulkan.Core10.Sampler.VkFilter'
--     value
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcImage@ /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage', 'VkImageBlit',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBlitImage" vkCmdBlitImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()
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
-- -   @pRegions@ is a pointer to an array of 'VkBufferImageCopy'
--     structures specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source buffer to the specified region of the destination image.
--
-- If the format of @dstImage@ is a [multi-planar image
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)),
-- regions of each plane to be a target of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'VkBufferImageCopy'
-- structure. In this case, the @aspectMask@ of @imageSubresource@ /must/
-- be @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@. For the purposes of
-- @vkCmdCopyBufferToImage@, each plane of a multi-planar image is treated
-- as having the format listed in
-- [{html_spec_relative}#features-formats-compatible-planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Graphics.Vulkan.Core10.Core.VkFormat'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- == Valid Usage
--
-- -   The buffer region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcBuffer@
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @dstImage@ if the @dstImage@’s
--     'Graphics.Vulkan.Core10.Core.VkFormat' is not a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     and /must/ be a region that is contained within the plane being
--     copied to if the @dstImage@’s 'Graphics.Vulkan.Core10.Core.VkFormat'
--     is a multi-planar format
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_SRC_BIT@ usage flag
--
-- -   @dstImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_TRANSFER_DST_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--
-- -   If @srcBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstImage@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstImage@ /must/ have a sample count equal to
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_GENERAL@, or @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@
--
-- -   The @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@ of each element of @pRegions@ /must/
--     be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @imageOffset@ and and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @dstImage@ /must/ be a valid @VkImage@ handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid @VkBufferImageCopy@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcBuffer@ /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', 'VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
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
-- -   @pRegions@ is a pointer to an array of 'VkBufferImageCopy'
--     structures specifying the regions to copy.
--
-- = Description
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source image to the specified region of the destination buffer.
--
-- If the 'Graphics.Vulkan.Core10.Core.VkFormat' of @srcImage@ is a
-- [multi-planar image
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
-- regions of each plane to be a source of a copy /must/ be specified
-- separately using the @pRegions@ member of the 'VkBufferImageCopy'
-- structure. In this case, the @aspectMask@ of @imageSubresource@ /must/
-- be @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
-- @VK_IMAGE_ASPECT_PLANE_2_BIT@. For the purposes of
-- @vkCmdCopyBufferToImage@, each plane of a multi-planar image is treated
-- as having the format listed in
-- [{html_spec_relative}#features-formats-compatible-planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
-- for the plane identified by the @aspectMask@ of the corresponding
-- subresource. This applies both to 'Graphics.Vulkan.Core10.Core.VkFormat'
-- and to coordinates used in the copy, which correspond to texels in the
-- /plane/ rather than how these texels map to coordinates in the image as
-- a whole.
--
-- == Valid Usage
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @srcImage@ if the @srcImage@’s
--     'Graphics.Vulkan.Core10.Core.VkFormat' is not a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     and /must/ be a region that is contained within the plane being
--     copied if the @srcImage@’s 'Graphics.Vulkan.Core10.Core.VkFormat' is
--     a multi-planar format
--
-- -   The buffer region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @dstBuffer@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_TRANSFER_SRC_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
--
-- -   @srcImage@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @srcImage@ /must/ have a sample count equal to
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@,
--     @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   @dstBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   The @imageSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @imageSubresource.baseArrayLayer@ +
--     @imageSubresource.layerCount@ of each element of @pRegions@ /must/
--     be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @imageOffset@ and and @imageExtent@ members of each element of
--     @pRegions@ /must/ respect the image transfer granularity
--     requirements of @commandBuffer@’s command pool’s queue family, as
--     described in
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkQueueFamilyProperties'
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcImage@ /must/ be a valid @VkImage@ handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @dstBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid @VkBufferImageCopy@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstBuffer@, and @srcImage@ /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', 'VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
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
-- [copies](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies-buffers).
--
-- __Note__
--
-- Buffer updates performed with @vkCmdUpdateBuffer@ first copy the data
-- into command buffer memory when the command is recorded (which requires
-- additional storage and may incur an additional allocation), and then
-- copy the data from the command buffer into @dstBuffer@ when the command
-- is executed on a device.
--
-- The additional cost of this functionality compared to [buffer to buffer
-- copies](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#copies-buffers)
-- means it is only recommended for very small amounts of data, and is why
-- it is limited to only 65536 bytes.
--
-- Applications /can/ work around this by issuing multiple
-- @vkCmdUpdateBuffer@ commands to different ranges of the same buffer, but
-- it is strongly recommended that they /should/ not.
--
-- The source data is copied from the user pointer to the command buffer
-- when the command is called.
--
-- @vkCmdUpdateBuffer@ is only allowed outside of a render pass. This
-- command is treated as “transfer” operation, for the purposes of
-- synchronization barriers. The @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ /must/
-- be specified in @usage@ of
-- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateInfo' in order for the
-- buffer to be compatible with @vkCmdUpdateBuffer@.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @dataSize@ /must/ be less than or equal to the size of @dstBuffer@
--     minus @dstOffset@
--
-- -   @dstBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- -   @dataSize@ /must/ be less than or equal to @65536@
--
-- -   @dataSize@ /must/ be a multiple of @4@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @dstBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdUpdateBuffer" vkCmdUpdateBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()
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
--     multiple of 4, or @VK_WHOLE_SIZE@ to fill the range from @offset@ to
--     the end of the buffer. If @VK_WHOLE_SIZE@ is used and the remaining
--     size of the buffer is not a multiple of 4, then the nearest smaller
--     multiple is used.
--
-- -   @data@ is the 4-byte word written repeatedly to the buffer to fill
--     @size@ bytes of data. The data word is written to memory according
--     to the host endianness.
--
-- = Description
--
-- @vkCmdFillBuffer@ is treated as “transfer” operation for the purposes of
-- synchronization barriers. The @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ /must/
-- be specified in @usage@ of @VkBufferCreateInfo@ in order for the buffer
-- to be compatible with @vkCmdFillBuffer@.
--
-- == Valid Usage
--
-- -   @dstOffset@ /must/ be less than the size of @dstBuffer@
--
-- -   @dstOffset@ /must/ be a multiple of @4@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be greater
--     than @0@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be less
--     than or equal to the size of @dstBuffer@ minus @dstOffset@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be a
--     multiple of @4@
--
-- -   @dstBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @dstBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @dstBuffer@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdFillBuffer" vkCmdFillBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()
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
--     @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@, @VK_IMAGE_LAYOUT_GENERAL@ or
--     @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@.
--
-- -   @pColor@ is a pointer to a 'VkClearColorValue' structure that
--     contains the values the image subresource ranges will be cleared to
--     (see
--     [{html_spec_relative}#clears-values](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears-values)
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ points to an array of
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     structures that describe a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in [Image
--     Views](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views).
--     The @aspectMask@ of all image subresource ranges /must/ only include
--     @VK_IMAGE_ASPECT_COLOR_BIT@.
--
-- = Description
--
-- Each specified range in @pRanges@ is cleared to the value specified by
-- @pColor@.
--
-- == Valid Usage
--
-- -   @image@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_TRANSFER_DST_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   @image@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   @image@ /must/ not use a format listed in
--     [{html_spec_relative}#features-formats-requiring-sampler-ycbcr-conversion](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @imageLayout@ /must/ specify the layout of the image subresource
--     ranges of @image@ specified in @pRanges@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @imageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_GENERAL@, or @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@
--
-- -   The
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     element of @pRanges@, if the @levelCount@ member is not
--     @VK_REMAINING_MIP_LEVELS@, then @baseMipLevel@ + @levelCount@ /must/
--     be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   The
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     element of @pRanges@, if the @layerCount@ member is not
--     @VK_REMAINING_ARRAY_LAYERS@, then @baseArrayLayer@ + @layerCount@
--     /must/ be less than the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   @image@ /must/ not have a compressed or depth\/stencil format
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @imageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @pColor@ /must/ be a valid pointer to a valid @VkClearColorValue@
--     union
--
-- -   @pRanges@ /must/ be a valid pointer to an array of @rangeCount@
--     valid @VkImageSubresourceRange@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @rangeCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @image@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkClearColorValue', 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdClearColorImage" vkCmdClearColorImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
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
--     ranges to be cleared, and /must/ be @VK_IMAGE_LAYOUT_GENERAL@ or
--     @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@.
--
-- -   @pDepthStencil@ is a pointer to a 'VkClearDepthStencilValue'
--     structure that contains the values the depth and stencil image
--     subresource ranges will be cleared to (see
--     [{html_spec_relative}#clears-values](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears-values)
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ points to an array of
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     structures that describe a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in [Image
--     Views](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views).
--     The @aspectMask@ of each image subresource range in @pRanges@ /can/
--     include @VK_IMAGE_ASPECT_DEPTH_BIT@ if the image format has a depth
--     component, and @VK_IMAGE_ASPECT_STENCIL_BIT@ if the image format has
--     a stencil component. @pDepthStencil@ is a pointer to a
--     @VkClearDepthStencilValue@ structure that contains the values the
--     image subresource ranges will be cleared to (see
--     [{html_spec_relative}#clears-values](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears-values)
--     below).
--
-- == Valid Usage
--
-- -   @image@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_TRANSFER_DST_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   @image@ /must/ have been created with
--     @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @image@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @imageLayout@ /must/ specify the layout of the image subresource
--     ranges of @image@ specified in @pRanges@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @imageLayout@ /must/ be either of
--     @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   The
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'::@baseMipLevel@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     element of @pRanges@, if the @levelCount@ member is not
--     @VK_REMAINING_MIP_LEVELS@, then @baseMipLevel@ + @levelCount@ /must/
--     be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   The
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'::@baseArrayLayer@
--     members of the elements of the @pRanges@ array /must/ each be less
--     than the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   For each 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     element of @pRanges@, if the @layerCount@ member is not
--     @VK_REMAINING_ARRAY_LAYERS@, then @baseArrayLayer@ + @layerCount@
--     /must/ be less than the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   @image@ /must/ have a depth\/stencil format
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @imageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @pDepthStencil@ /must/ be a valid pointer to a valid
--     @VkClearDepthStencilValue@ structure
--
-- -   @pRanges@ /must/ be a valid pointer to an array of @rangeCount@
--     valid @VkImageSubresourceRange@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @rangeCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @image@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkClearDepthStencilValue',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
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
-- -   @pAttachments@ is a pointer to an array of 'VkClearAttachment'
--     structures defining the attachments to clear and the clear values to
--     use.
--
-- -   @rectCount@ is the number of entries in the @pRects@ array.
--
-- -   @pRects@ points to an array of 'VkClearRect' structures defining
--     regions within each selected attachment to clear.
--
-- = Description
--
-- @vkCmdClearAttachments@ /can/ clear multiple regions of each attachment
-- used in the current subpass of a render pass instance. This command
-- /must/ be called only inside a render pass instance, and implicitly
-- selects the images to clear based on the current framebuffer attachments
-- and the command parameters.
--
-- == Valid Usage
--
-- -   If the @aspectMask@ member of any element of @pAttachments@ contains
--     @VK_IMAGE_ASPECT_COLOR_BIT@, the @colorAttachment@ member of that
--     element /must/ refer to a valid color attachment in the current
--     subpass
--
-- -   The rectangular region specified by each element of @pRects@ /must/
--     be contained within the render area of the current render pass
--     instance
--
-- -   The layers specified by each element of @pRects@ /must/ be contained
--     within every attachment that @pAttachments@ refers to
--
-- -   If the render pass instance this is recorded in uses multiview, then
--     @baseArrayLayer@ /must/ be zero and @layerCount@ /must/ be one.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pAttachments@ /must/ be a valid pointer to an array of
--     @attachmentCount@ valid @VkClearAttachment@ structures
--
-- -   @pRects@ /must/ be a valid pointer to an array of @rectCount@
--     @VkClearRect@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
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
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkClearAttachment', 'VkClearRect',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdClearAttachments" vkCmdClearAttachments :: ("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()
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
-- -   @pRegions@ is a pointer to an array of 'VkImageResolve' structures
--     specifying the regions to resolve.
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
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @srcImage@ /must/ have a sample count equal to any valid sample
--     count value other than @VK_SAMPLE_COUNT_1_BIT@
--
-- -   If @dstImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @dstImage@ /must/ have a sample count equal to
--     @VK_SAMPLE_COUNT_1_BIT@
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@,
--     @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR@,
--     @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   If @dstImage@ was created with @tiling@ equal to
--     @VK_IMAGE_TILING_LINEAR@, @dstImage@ /must/ have been created with a
--     @format@ that supports being a color attachment, as specified by the
--     @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@ flag in
--     @VkFormatProperties@::@linearTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   If @dstImage@ was created with @tiling@ equal to
--     @VK_IMAGE_TILING_OPTIMAL@, @dstImage@ /must/ have been created with
--     a @format@ that supports being a color attachment, as specified by
--     the @VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT@ flag in
--     @VkFormatProperties@::@optimalTilingFeatures@ returned by
--     @vkGetPhysicalDeviceFormatProperties@
--
-- -   @srcImage@ and @dstImage@ /must/ have been created with the same
--     image format
--
-- -   The @srcSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.mipLevel@ member of each element of @pRegions@
--     /must/ be less than the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- -   The @srcSubresource.baseArrayLayer@ + @srcSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @srcImage@ was
--     created
--
-- -   The @dstSubresource.baseArrayLayer@ + @dstSubresource.layerCount@ of
--     each element of @pRegions@ /must/ be less than or equal to the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @dstImage@ was
--     created
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcImage@ /must/ be a valid @VkImage@ handle
--
-- -   @srcImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @dstImage@ /must/ be a valid @VkImage@ handle
--
-- -   @dstImageLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @pRegions@ /must/ be a valid pointer to an array of @regionCount@
--     valid @VkImageResolve@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @regionCount@ /must/ be greater than @0@
--
-- -   Each of @commandBuffer@, @dstImage@, and @srcImage@ /must/ have been
--     created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            |                                                                                                       |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout', 'VkImageResolve'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdResolveImage" vkCmdResolveImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()
-- | vkCmdSetEvent - Set an event object to signaled state
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @event@ is the event that will be signaled.
--
-- -   @stageMask@ specifies the [source stage
--     mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages)
--     used to determine when the @event@ is signaled.
--
-- = Description
--
-- When 'vkCmdSetEvent' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event signal operation which sets the event to the signaled state.
--
-- The first [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes all commands that occur earlier in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order).
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the [source stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @stageMask@.
--
-- The second [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes only the event signal operation.
--
-- If @event@ is already in the signaled state when 'vkCmdSetEvent' is
-- executed on the device, then 'vkCmdSetEvent' has no effect, no event
-- signal operation occurs, and no execution dependency is generated.
--
-- == Valid Usage
--
-- -   @stageMask@ /must/ not include @VK_PIPELINE_STAGE_HOST_BIT@
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   @commandBuffer@’s current device mask /must/ include exactly one
--     physical device.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @event@ /must/ be a valid @VkEvent@ handle
--
-- -   @stageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @stageMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @event@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdSetEvent" vkCmdSetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
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
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the [source stage
--     mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages)
--     used to determine when the @event@ is unsignaled.
--
-- = Description
--
-- When 'vkCmdResetEvent' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event unsignal operation which resets the event to the unsignaled state.
--
-- The first [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes all commands that occur earlier in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order).
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the [source stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @stageMask@.
--
-- The second [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes only the event unsignal operation.
--
-- If @event@ is already in the unsignaled state when 'vkCmdResetEvent' is
-- executed on the device, then 'vkCmdResetEvent' has no effect, no event
-- unsignal operation occurs, and no execution dependency is generated.
--
-- == Valid Usage
--
-- -   @stageMask@ /must/ not include @VK_PIPELINE_STAGE_HOST_BIT@
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   When this command executes, @event@ /must/ not be waited on by a
--     @vkCmdWaitEvents@ command that is currently executing
--
-- -   @commandBuffer@’s current device mask /must/ include exactly one
--     physical device.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @event@ /must/ be a valid @VkEvent@ handle
--
-- -   @stageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @stageMask@ /must/ not be @0@
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @event@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdResetEvent" vkCmdResetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
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
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the [source stage
--     mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages).
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the [destination stage
--     mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages).
--
-- -   @memoryBarrierCount@ is the length of the @pMemoryBarriers@ array.
--
-- -   @pMemoryBarriers@ is a pointer to an array of 'VkMemoryBarrier'
--     structures.
--
-- -   @bufferMemoryBarrierCount@ is the length of the
--     @pBufferMemoryBarriers@ array.
--
-- -   @pBufferMemoryBarriers@ is a pointer to an array of
--     'VkBufferMemoryBarrier' structures.
--
-- -   @imageMemoryBarrierCount@ is the length of the
--     @pImageMemoryBarriers@ array.
--
-- -   @pImageMemoryBarriers@ is a pointer to an array of
--     'VkImageMemoryBarrier' structures.
--
-- = Description
--
-- When @vkCmdWaitEvents@ is submitted to a queue, it defines a memory
-- dependency between prior event signal operations on the same queue or
-- the host, and subsequent commands. @vkCmdWaitEvents@ /must/ not be used
-- to wait on event signal operations occuring on other queues.
--
-- The first synchronization scope only includes event signal operations
-- that operate on members of @pEvents@, and the operations that
-- happened-before the event signal operations. Event signal operations
-- performed by 'vkCmdSetEvent' that occur earlier in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order)
-- are included in the first synchronization scope, if the [logically
-- latest](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-order)
-- pipeline stage in their @stageMask@ parameter is [logically
-- earlier](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-order)
-- than or equal to the [logically
-- latest](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-order)
-- pipeline stage in @srcStageMask@. Event signal operations performed by
-- 'Graphics.Vulkan.Core10.Event.vkSetEvent' are only included in the first
-- synchronization scope if @VK_PIPELINE_STAGE_HOST_BIT@ is included in
-- @srcStageMask@.
--
-- The second [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes all commands that occur later in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order).
-- The second synchronization scope is limited to operations on the
-- pipeline stages determined by the [destination stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @dstStageMask@.
--
-- The first [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access in the pipeline stages determined by the [source
-- stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of [memory
-- barriers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-memory-barriers).
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access in the pipeline stages determined by the
-- [destination stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of [memory
-- barriers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-memory-barriers).
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- __Note__
--
-- 'vkCmdWaitEvents' is used with 'vkCmdSetEvent' to define a memory
-- dependency between two sets of action commands, roughly in the same way
-- as pipeline barriers, but split into two commands such that work between
-- the two /may/ execute unhindered.
--
-- __Note__
--
-- Applications /should/ be careful to avoid race conditions when using
-- events. There is no direct ordering guarantee between a
-- 'vkCmdResetEvent' command and a 'vkCmdWaitEvents' command submitted
-- after it, so some other execution dependency /must/ be included between
-- these commands (e.g. a semaphore).
--
-- == Valid Usage
--
-- -   @srcStageMask@ /must/ be the bitwise OR of the @stageMask@ parameter
--     used in previous calls to @vkCmdSetEvent@ with any of the members of
--     @pEvents@ and @VK_PIPELINE_STAGE_HOST_BIT@ if any of the members of
--     @pEvents@ was set using @vkSetEvent@
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If @pEvents@ includes one or more events that will be signaled by
--     @vkSetEvent@ after @commandBuffer@ has been submitted to a queue,
--     then @vkCmdWaitEvents@ /must/ not be called inside a render pass
--     instance
--
-- -   Any pipeline stage included in @srcStageMask@ or @dstStageMask@
--     /must/ be supported by the capabilities of the queue family
--     specified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.Core10.CommandPool.VkCommandPoolCreateInfo'
--     structure that was used to create the @VkCommandPool@ that
--     @commandBuffer@ was allocated from, as specified in the [table of
--     supported pipeline
--     stages](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-supported).
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the [table of
--     supported access
--     types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-types-supported).
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the [table of
--     supported access
--     types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-types-supported).
--
-- -   @commandBuffer@’s current device mask /must/ include exactly one
--     physical device.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pEvents@ /must/ be a valid pointer to an array of @eventCount@
--     valid @VkEvent@ handles
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   If @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a
--     valid pointer to an array of @memoryBarrierCount@ valid
--     @VkMemoryBarrier@ structures
--
-- -   If @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid @VkBufferMemoryBarrier@ structures
--
-- -   If @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@
--     /must/ be a valid pointer to an array of @imageMemoryBarrierCount@
--     valid @VkImageMemoryBarrier@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   @eventCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pEvents@ /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkBufferMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Event.VkEvent', 'VkImageMemoryBarrier',
-- 'VkMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWaitEvents" vkCmdWaitEvents :: ("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
-- | vkCmdPipelineBarrier - Insert a memory dependency
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @srcStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the [source stage
--     mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks).
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the [destination stage
--     mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks).
--
-- -   @dependencyFlags@ is a bitmask of
--     'Graphics.Vulkan.Core10.Pass.VkDependencyFlagBits' specifying how
--     execution and memory dependencies are formed.
--
-- -   @memoryBarrierCount@ is the length of the @pMemoryBarriers@ array.
--
-- -   @pMemoryBarriers@ is a pointer to an array of 'VkMemoryBarrier'
--     structures.
--
-- -   @bufferMemoryBarrierCount@ is the length of the
--     @pBufferMemoryBarriers@ array.
--
-- -   @pBufferMemoryBarriers@ is a pointer to an array of
--     'VkBufferMemoryBarrier' structures.
--
-- -   @imageMemoryBarrierCount@ is the length of the
--     @pImageMemoryBarriers@ array.
--
-- -   @pImageMemoryBarriers@ is a pointer to an array of
--     'VkImageMemoryBarrier' structures.
--
-- = Description
--
-- When 'vkCmdPipelineBarrier' is submitted to a queue, it defines a memory
-- dependency between commands that were submitted before it, and those
-- submitted after it.
--
-- If 'vkCmdPipelineBarrier' was recorded outside a render pass instance,
-- the first [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes all commands that occur earlier in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order).
-- If 'vkCmdPipelineBarrier' was recorded inside a render pass instance,
-- the first synchronization scope includes only commands that occur
-- earlier in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order)
-- within the same subpass. In either case, the first synchronization scope
-- is limited to operations on the pipeline stages determined by the
-- [source stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @srcStageMask@.
--
-- If 'vkCmdPipelineBarrier' was recorded outside a render pass instance,
-- the second [synchronization
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-scopes)
-- includes all commands that occur later in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order).
-- If 'vkCmdPipelineBarrier' was recorded inside a render pass instance,
-- the second synchronization scope includes only commands that occur later
-- in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order)
-- within the same subpass. In either case, the second synchronization
-- scope is limited to operations on the pipeline stages determined by the
-- [destination stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @dstStageMask@.
--
-- The first [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access in the pipeline stages determined by the [source
-- stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of [memory
-- barriers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-memory-barriers).
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access in the pipeline stages determined by the
-- [destination stage
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-masks)
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of [memory
-- barriers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-memory-barriers).
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- If @dependencyFlags@ includes @VK_DEPENDENCY_BY_REGION_BIT@, then any
-- dependency between
-- [framebuffer-space](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-framebuffer-regions)
-- pipeline stages is
-- [framebuffer-local](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-framebuffer-regions)
-- - otherwise it is
-- [framebuffer-global](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-framebuffer-regions).
--
-- == Valid Usage
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-geometryShader)
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If the [tessellation
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-tessellationShader)
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the render pass /must/ have been created with a
--     @VkSubpassDependency@ instance in @pDependencies@ that expresses a
--     dependency from the current subpass to itself.
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     @srcStageMask@ /must/ contain a subset of the bit values in the
--     @srcStageMask@ member of that instance of @VkSubpassDependency@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     @dstStageMask@ /must/ contain a subset of the bit values in the
--     @dstStageMask@ member of that instance of @VkSubpassDependency@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the @srcAccessMask@ of any element of @pMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ contain a subset of the bit values the
--     @srcAccessMask@ member of that instance of @VkSubpassDependency@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the @dstAccessMask@ of any element of @pMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ contain a subset of the bit values the
--     @dstAccessMask@ member of that instance of @VkSubpassDependency@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     @dependencyFlags@ /must/ be equal to the @dependencyFlags@ member of
--     that instance of @VkSubpassDependency@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     @bufferMemoryBarrierCount@ /must/ be @0@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the @image@ member of any element of @pImageMemoryBarriers@ /must/
--     be equal to one of the elements of @pAttachments@ that the current
--     @framebuffer@ was created with, that is also referred to by one of
--     the elements of the @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@ members of the @VkSubpassDescription@
--     instance that the current subpass was created with
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the @oldLayout@ and @newLayout@ members of any element of
--     @pImageMemoryBarriers@ /must/ be equal to the @layout@ member of an
--     element of the @pColorAttachments@, @pResolveAttachments@ or
--     @pDepthStencilAttachment@ members of the @VkSubpassDescription@
--     instance that the current subpass was created with, that refers to
--     the same @image@
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the @oldLayout@ and @newLayout@ members of an element of
--     @pImageMemoryBarriers@ /must/ be equal
--
-- -   If @vkCmdPipelineBarrier@ is called within a render pass instance,
--     the @srcQueueFamilyIndex@ and @dstQueueFamilyIndex@ members of any
--     element of @pImageMemoryBarriers@ /must/ be
--     @VK_QUEUE_FAMILY_IGNORED@
--
-- -   Any pipeline stage included in @srcStageMask@ or @dstStageMask@
--     /must/ be supported by the capabilities of the queue family
--     specified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.Core10.CommandPool.VkCommandPoolCreateInfo'
--     structure that was used to create the @VkCommandPool@ that
--     @commandBuffer@ was allocated from, as specified in the [table of
--     supported pipeline
--     stages](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-supported).
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ and
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the [table of
--     supported access
--     types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-types-supported).
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ and
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the [table of
--     supported access
--     types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-types-supported).
--
-- -   If @vkCmdPipelineBarrier@ is called outside of a render pass
--     instance, @dependencyFlags@ /must/ not include
--     @VK_DEPENDENCY_VIEW_LOCAL_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @srcStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @srcStageMask@ /must/ not be @0@
--
-- -   @dstStageMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' values
--
-- -   @dstStageMask@ /must/ not be @0@
--
-- -   @dependencyFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkDependencyFlagBits' values
--
-- -   If @memoryBarrierCount@ is not @0@, @pMemoryBarriers@ /must/ be a
--     valid pointer to an array of @memoryBarrierCount@ valid
--     @VkMemoryBarrier@ structures
--
-- -   If @bufferMemoryBarrierCount@ is not @0@, @pBufferMemoryBarriers@
--     /must/ be a valid pointer to an array of @bufferMemoryBarrierCount@
--     valid @VkBufferMemoryBarrier@ structures
--
-- -   If @imageMemoryBarrierCount@ is not @0@, @pImageMemoryBarriers@
--     /must/ be a valid pointer to an array of @imageMemoryBarrierCount@
--     valid @VkImageMemoryBarrier@ structures
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Transfer                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'VkBufferMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pass.VkDependencyFlags', 'VkImageMemoryBarrier',
-- 'VkMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPipelineBarrier" vkCmdPipelineBarrier :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
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
--     'Graphics.Vulkan.Core10.CommandBuffer.VkQueryControlFlagBits'
--     specifying constraints on the types of queries that /can/ be
--     performed.
--
-- = Description
--
-- If the @queryType@ of the pool is @VK_QUERY_TYPE_OCCLUSION@ and @flags@
-- contains @VK_QUERY_CONTROL_PRECISE_BIT@, an implementation /must/ return
-- a result that matches the actual number of samples passed. This is
-- described in more detail in [Occlusion
-- Queries](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-occlusion).
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
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active)
--     within @commandBuffer@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If the [precise occlusion
--     queries](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-occlusionQueryPrecise)
--     feature is not enabled, or the @queryType@ used to create
--     @queryPool@ was not @VK_QUERY_TYPE_OCCLUSION@, @flags@ /must/ not
--     contain @VK_QUERY_CONTROL_PRECISE_BIT@
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If the @queryType@ used to create @queryPool@ was
--     @VK_QUERY_TYPE_OCCLUSION@, the @VkCommandPool@ that @commandBuffer@
--     was allocated from /must/ support graphics operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     @VK_QUERY_TYPE_PIPELINE_STATISTICS@ and any of the
--     @pipelineStatistics@ indicate graphics operations, the
--     @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   If the @queryType@ used to create @queryPool@ was
--     @VK_QUERY_TYPE_PIPELINE_STATISTICS@ and any of the
--     @pipelineStatistics@ indicate compute operations, the
--     @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support compute operations
--
-- -   If @vkCmdBeginQuery@ is called within a render pass instance, the
--     sum of @query@ and the number of bits set in the current subpass’s
--     view mask /must/ be less than or equal to the number of queries in
--     @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.CommandBuffer.VkQueryControlFlagBits' values
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkQueryControlFlags',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginQuery" vkCmdBeginQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()
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
-- 'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults' and
-- 'vkCmdCopyQueryPoolResults', and this is when the query’s status is set
-- to available.
--
-- Once a query is ended the query /must/ finish in finite time, unless the
-- state of the query is changed using other commands, e.g. by issuing a
-- reset of the query.
--
-- == Valid Usage
--
-- -   All queries used by the command /must/ be
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active)
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- -   If @vkCmdEndQuery@ is called within a render pass instance, the sum
--     of @query@ and the number of bits set in the current subpass’s view
--     mask /must/ be less than or equal to the number of queries in
--     @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndQuery" vkCmdEndQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
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
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdResetQueryPool" vkCmdResetQueryPool :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
-- | vkCmdWriteTimestamp - Write a device timestamp into a query object
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @pipelineStage@ is one of the
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits', specifying a
--     stage of the pipeline.
--
-- -   @queryPool@ is the query pool that will manage the timestamp.
--
-- -   @query@ is the query within the query pool that will contain the
--     timestamp.
--
-- = Description
--
-- @vkCmdWriteTimestamp@ latches the value of the timer when all previous
-- commands have completed executing as far as the specified pipeline
-- stage, and writes the timestamp value to memory. When the timestamp
-- value is written, the availability status of the query is set to
-- available.
--
-- __Note__
--
-- If an implementation is unable to detect completion and latch the timer
-- at any specific stage of the pipeline, it /may/ instead do so at any
-- logically later stage.
--
-- 'vkCmdCopyQueryPoolResults' /can/ then be called to copy the timestamp
-- value from the query pool into buffer memory, with ordering and
-- synchronization behavior equivalent to how other queries operate.
-- Timestamp values /can/ also be retrieved from the query pool using
-- 'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults'. As with other
-- queries, the query /must/ be reset using 'vkCmdResetQueryPool' before
-- requesting the timestamp value be written to it.
--
-- While @vkCmdWriteTimestamp@ /can/ be called inside or outside of a
-- render pass instance, 'vkCmdCopyQueryPoolResults' /must/ only be called
-- outside of a render pass instance.
--
-- Timestamps /may/ only be meaningfully compared if they are written by
-- commands submitted to the same queue.
--
-- __Note__
--
-- An example of such a comparison is determining the execution time of a
-- sequence of commands.
--
-- If @vkCmdWriteTimestamp@ is called while executing a render pass
-- instance that has multiview enabled, the timestamp uses N consecutive
-- query indices in the query pool (starting at @query@) where N is the
-- number of bits set in the view mask of the subpass the command is
-- executed in. The resulting query values are determined by an
-- implementation-dependent choice of one of the following behaviors:
--
-- -   The first query is a timestamp value and (if more than one bit is
--     set in the view mask) zero is written to the remaining queries. If
--     two timestamps are written in the same subpass, the sum of the
--     execution time of all views between those commands is the difference
--     between the first query written by each command.
--
-- -   All N queries are timestamp values. If two timestamps are written in
--     the same subpass, the sum of the execution time of all views between
--     those commands is the sum of the difference between corresponding
--     queries written by each command. The difference between
--     corresponding queries /may/ be the execution time of a single view.
--
-- In either case, the application /can/ sum the differences between all N
-- queries to determine the total execution time.
--
-- == Valid Usage
--
-- -   @queryPool@ /must/ have been created with a @queryType@ of
--     @VK_QUERY_TYPE_TIMESTAMP@
--
-- -   The query identified by @queryPool@ and @query@ /must/ be
--     /unavailable/
--
-- -   The command pool’s queue family /must/ support a non-zero
--     @timestampValidBits@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If @vkCmdWriteTimestamp@ is called within a render pass instance,
--     the sum of @query@ and the number of bits set in the current
--     subpass’s view mask /must/ be less than or equal to the number of
--     queries in @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' value
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   Both of @commandBuffer@, and @queryPool@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Transfer                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdWriteTimestamp" vkCmdWriteTimestamp :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
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
-- -   @dstBuffer@ is a @VkBuffer@ object that will receive the results of
--     the copy command.
--
-- -   @dstOffset@ is an offset into @dstBuffer@.
--
-- -   @stride@ is the stride in bytes between results for individual
--     queries within @dstBuffer@. The required size of the backing memory
--     for @dstBuffer@ is determined as described above for
--     'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults'.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.Core10.Query.VkQueryResultFlagBits' specifying how
--     and when results are returned.
--
-- = Description
--
-- @vkCmdCopyQueryPoolResults@ is guaranteed to see the effect of previous
-- uses of @vkCmdResetQueryPool@ in the same queue, without any additional
-- synchronization. Thus, the results will always reflect the most recent
-- use of the query.
--
-- @flags@ has the same possible values described above for the @flags@
-- parameter of 'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults', but
-- the different style of execution causes some subtle behavioral
-- differences. Because @vkCmdCopyQueryPoolResults@ executes in order with
-- respect to other query commands, there is less ambiguity about which use
-- of a query is being requested.
--
-- If no bits are set in @flags@, results for all requested queries in the
-- available state are written as 32-bit unsigned integer values, and
-- nothing is written for queries in the unavailable state.
--
-- If @VK_QUERY_RESULT_64_BIT@ is set, the results are written as an array
-- of 64-bit unsigned integer values as described for
-- 'Graphics.Vulkan.Core10.Query.vkGetQueryPoolResults'.
--
-- If @VK_QUERY_RESULT_WAIT_BIT@ is set, the implementation will wait for
-- each query’s status to be in the available state before retrieving the
-- numerical results for that query. This is guaranteed to reflect the most
-- recent use of the query on the same queue, assuming that the query is
-- not being simultaneously used by other queues. If the query does not
-- become available in a finite amount of time (e.g. due to not issuing a
-- query since the last reset), a @VK_ERROR_DEVICE_LOST@ error /may/ occur.
--
-- Similarly, if @VK_QUERY_RESULT_WITH_AVAILABILITY_BIT@ is set and
-- @VK_QUERY_RESULT_WAIT_BIT@ is not set, the availability is guaranteed to
-- reflect the most recent use of the query on the same queue, assuming
-- that the query is not being simultaneously used by other queues. As with
-- @vkGetQueryPoolResults@, implementations /must/ guarantee that if they
-- return a non-zero availability value, then the numerical results are
-- valid.
--
-- If @VK_QUERY_RESULT_PARTIAL_BIT@ is set, @VK_QUERY_RESULT_WAIT_BIT@ is
-- not set, and the query’s status is unavailable, an intermediate result
-- value between zero and the final result value is written for that query.
--
-- @VK_QUERY_RESULT_PARTIAL_BIT@ /must/ not be used if the pool’s
-- @queryType@ is @VK_QUERY_TYPE_TIMESTAMP@.
--
-- @vkCmdCopyQueryPoolResults@ is considered to be a transfer operation,
-- and its writes to buffer memory /must/ be synchronized using
-- @VK_PIPELINE_STAGE_TRANSFER_BIT@ and @VK_ACCESS_TRANSFER_WRITE_BIT@
-- before using the results.
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
-- -   If @VK_QUERY_RESULT_64_BIT@ is not set in @flags@ then @dstOffset@
--     and @stride@ /must/ be multiples of @4@
--
-- -   If @VK_QUERY_RESULT_64_BIT@ is set in @flags@ then @dstOffset@ and
--     @stride@ /must/ be multiples of @8@
--
-- -   @dstBuffer@ /must/ have enough storage, from @dstOffset@, to contain
--     the result of each query, as described
--     [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-memorylayout)
--
-- -   @dstBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_DST_BIT@ usage flag
--
-- -   If @dstBuffer@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   If the @queryType@ used to create @queryPool@ was
--     @VK_QUERY_TYPE_TIMESTAMP@, @flags@ /must/ not contain
--     @VK_QUERY_RESULT_PARTIAL_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @dstBuffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Query.VkQueryResultFlagBits' values
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   Each of @commandBuffer@, @dstBuffer@, and @queryPool@ /must/ have
--     been created, allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              | Transfer                                                                                                                   |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.Core10.Query.VkQueryResultFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdCopyQueryPoolResults" vkCmdCopyQueryPoolResults :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()
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
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' specifying
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
-- == Valid Usage
--
-- -   For each byte in the range specified by @offset@ and @size@ and for
--     each shader stage in @stageFlags@, there /must/ be a push constant
--     range in @layout@ that includes that byte and that stage
--
-- -   For each byte in the range specified by @offset@ and @size@ and for
--     each push constant range that overlaps that byte, @stageFlags@
--     /must/ include all stages in that push constant range’s
--     'Graphics.Vulkan.Core10.PipelineLayout.VkPushConstantRange'::@stageFlags@
--
-- -   @offset@ /must/ be a multiple of @4@
--
-- -   @size@ /must/ be a multiple of @4@
--
-- -   @offset@ /must/ be less than
--     @VkPhysicalDeviceLimits@::@maxPushConstantsSize@
--
-- -   @size@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxPushConstantsSize@ minus @offset@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @layout@ /must/ be a valid @VkPipelineLayout@ handle
--
-- -   @stageFlags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pipeline.VkShaderStageFlagBits' values
--
-- -   @stageFlags@ /must/ not be @0@
--
-- -   @pValues@ /must/ be a valid pointer to an array of @size@ bytes
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics, or compute operations
--
-- -   @size@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and @layout@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Graphics                                                                                              |                                                                                                                            |
-- | Secondary                                                                                                   |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdPushConstants" vkCmdPushConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()
-- | vkCmdBeginRenderPass - Begin a new render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @pRenderPassBegin@ is a pointer to a 'VkRenderPassBeginInfo'
--     structure (defined below) which specifies the render pass to begin
--     an instance of, and the framebuffer the instance uses.
--
-- -   @contents@ is a 'VkSubpassContents' value specifying how the
--     commands in the first subpass will be provided.
--
-- = Description
--
-- After beginning a render pass instance, the command buffer is ready to
-- record the commands for the first subpass of that render pass.
--
-- == Valid Usage
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     @VkAttachmentDescription@ structures or the @layout@ member of the
--     @VkAttachmentReference@ structures specified when creating the
--     render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is @VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL@
--     then the corresponding attachment image subresource of the
--     framebuffer specified in the @framebuffer@ member of
--     @pRenderPassBegin@ /must/ have been created with
--     @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ set
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     @VkAttachmentDescription@ structures or the @layout@ member of the
--     @VkAttachmentReference@ structures specified when creating the
--     render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is
--     @VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL@,
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL@, or
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ then the
--     corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@
--     set
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     @VkAttachmentDescription@ structures or the @layout@ member of the
--     @VkAttachmentReference@ structures specified when creating the
--     render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@
--     then the corresponding attachment image subresource of the
--     framebuffer specified in the @framebuffer@ member of
--     @pRenderPassBegin@ /must/ have been created with
--     @VK_IMAGE_USAGE_SAMPLED_BIT@ or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ set
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     @VkAttachmentDescription@ structures or the @layout@ member of the
--     @VkAttachmentReference@ structures specified when creating the
--     render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ then
--     the corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ set
--
-- -   If any of the @initialLayout@ or @finalLayout@ member of the
--     @VkAttachmentDescription@ structures or the @layout@ member of the
--     @VkAttachmentReference@ structures specified when creating the
--     render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ then
--     the corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@ /must/
--     have been created with @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ set
--
-- -   If any of the @initialLayout@ members of the
--     @VkAttachmentDescription@ structures specified when creating the
--     render pass specified in the @renderPass@ member of
--     @pRenderPassBegin@ is not @VK_IMAGE_LAYOUT_UNDEFINED@, then each
--     such @initialLayout@ /must/ be equal to the current layout of the
--     corresponding attachment image subresource of the framebuffer
--     specified in the @framebuffer@ member of @pRenderPassBegin@
--
-- -   The @srcStageMask@ and @dstStageMask@ members of any element of the
--     @pDependencies@ member of
--     'Graphics.Vulkan.Core10.Pass.VkRenderPassCreateInfo' used to create
--     @renderPass@ /must/ be supported by the capabilities of the queue
--     family identified by the @queueFamilyIndex@ member of the
--     'Graphics.Vulkan.Core10.CommandPool.VkCommandPoolCreateInfo' used to
--     create the command pool which @commandBuffer@ was allocated from.
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pRenderPassBegin@ /must/ be a valid pointer to a valid
--     @VkRenderPassBeginInfo@ structure
--
-- -   @contents@ /must/ be a valid 'VkSubpassContents' value
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called outside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a primary @VkCommandBuffer@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Outside                                                                                                    | Graphics                                                                                              | Graphics                                                                                                                   |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkRenderPassBeginInfo',
-- 'VkSubpassContents'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdBeginRenderPass" vkCmdBeginRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()
-- | vkCmdNextSubpass - Transition to the next subpass of a render pass
--
-- = Parameters
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @contents@ specifies how the commands in the next subpass will be
--     provided, in the same fashion as the corresponding parameter of
--     'vkCmdBeginRenderPass'.
--
-- = Description
--
-- The subpass index for a render pass begins at zero when
-- @vkCmdBeginRenderPass@ is recorded, and increments each time
-- @vkCmdNextSubpass@ is recorded.
--
-- Moving to the next subpass automatically performs any multisample
-- resolve operations in the subpass being ended. End-of-subpass
-- multisample resolves are treated as color attachment writes for the
-- purposes of synchronization. That is, they are considered to execute in
-- the @VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT@ pipeline stage and
-- their writes are synchronized with
-- @VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT@. Synchronization between
-- rendering within a subpass and any resolve operations at the end of the
-- subpass occurs automatically, without need for explicit dependencies or
-- pipeline barriers. However, if the resolve attachment is also used in a
-- different subpass, an explicit dependency is needed.
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
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @contents@ /must/ be a valid 'VkSubpassContents' value
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a primary @VkCommandBuffer@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkSubpassContents'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdNextSubpass" vkCmdNextSubpass :: ("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()
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
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics operations
--
-- -   This command /must/ only be called inside of a render pass instance
--
-- -   @commandBuffer@ /must/ be a primary @VkCommandBuffer@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Inside                                                                                                     | Graphics                                                                                              | Graphics                                                                                                                   |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdEndRenderPass" vkCmdEndRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
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
-- @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, and it was recorded
-- into any other primary command buffer which is currently in the
-- [executable or recording
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle),
-- that primary command buffer becomes
-- [invalid](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ have been allocated with a @level@ of
--     @VK_COMMAND_BUFFER_LEVEL_PRIMARY@
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated with a
--     @level@ of @VK_COMMAND_BUFFER_LEVEL_SECONDARY@
--
-- -   Each element of @pCommandBuffers@ /must/ be in the [pending or
--     executable
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, and it was
--     recorded into any other primary command buffer, that primary command
--     buffer /must/ not be in the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, it /must/ not
--     be in the [pending
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle).
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, it /must/ not
--     have already been recorded to @commandBuffer@.
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, it /must/ not
--     appear more than once in @pCommandBuffers@.
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated from a
--     @VkCommandPool@ that was created for the same queue family as the
--     @VkCommandPool@ from which @commandBuffer@ was allocated
--
-- -   If @vkCmdExecuteCommands@ is being called within a render pass
--     instance, that render pass instance /must/ have been begun with the
--     @contents@ parameter of @vkCmdBeginRenderPass@ set to
--     @VK_SUBPASS_CONTENTS_SECONDARY_COMMAND_BUFFERS@
--
-- -   If @vkCmdExecuteCommands@ is being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ have been
--     recorded with the @VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT@
--
-- -   If @vkCmdExecuteCommands@ is being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ have been
--     recorded with @VkCommandBufferInheritanceInfo@::@subpass@ set to the
--     index of the subpass which the given command buffer will be executed
--     in
--
-- -   If @vkCmdExecuteCommands@ is being called within a render pass
--     instance, the render passes specified in the
--     pname::pBeginInfo::@pInheritanceInfo@::@renderPass@ members of the
--     'Graphics.Vulkan.Core10.CommandBuffer.vkBeginCommandBuffer' commands
--     used to begin recording each element of @pCommandBuffers@ /must/ be
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
--     with the current render pass.
--
-- -   If @vkCmdExecuteCommands@ is being called within a render pass
--     instance, and any element of @pCommandBuffers@ was recorded with
--     @VkCommandBufferInheritanceInfo@::@framebuffer@ not equal to
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', that
--     @VkFramebuffer@ /must/ match the @VkFramebuffer@ used in the current
--     render pass instance
--
-- -   If @vkCmdExecuteCommands@ is not being called within a render pass
--     instance, each element of @pCommandBuffers@ /must/ not have been
--     recorded with the @VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT@
--
-- -   If the [inherited
--     queries](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-inheritedQueries)
--     feature is not enabled, @commandBuffer@ /must/ not have any queries
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active)
--
-- -   If @commandBuffer@ has a @VK_QUERY_TYPE_OCCLUSION@ query
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active),
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with @VkCommandBufferInheritanceInfo@::@occlusionQueryEnable@ set to
--     @VK_TRUE@
--
-- -   If @commandBuffer@ has a @VK_QUERY_TYPE_OCCLUSION@ query
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active),
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with @VkCommandBufferInheritanceInfo@::@queryFlags@ having all bits
--     set that are set for the query
--
-- -   If @commandBuffer@ has a @VK_QUERY_TYPE_PIPELINE_STATISTICS@ query
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active),
--     then each element of @pCommandBuffers@ /must/ have been recorded
--     with @VkCommandBufferInheritanceInfo@::@pipelineStatistics@ having
--     all bits set that are set in the @VkQueryPool@ the query uses
--
-- -   Each element of @pCommandBuffers@ /must/ not begin any query types
--     that are
--     [active](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-active)
--     in @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@ valid @VkCommandBuffer@ handles
--
-- -   @commandBuffer@ /must/ be in the [recording
--     state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle)
--
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support transfer, graphics, or compute operations
--
-- -   @commandBuffer@ /must/ be a primary @VkCommandBuffer@
--
-- -   @commandBufferCount@ /must/ be greater than @0@
--
-- -   Both of @commandBuffer@, and the elements of @pCommandBuffers@
--     /must/ have been created, allocated, or retrieved from the same
--     @VkDevice@
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the @VkCommandPool@ that @commandBuffer@ was
--     allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
-- | [Command Buffer                                                                                             | [Render Pass                                                                                               | [Supported Queue                                                                                      | [Pipeline                                                                                                                  |
-- | Levels](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkCommandBufferLevel) | Scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vkCmdBeginRenderPass) | Types](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#VkQueueFlagBits) | Type](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-pipeline-stages-types) |
-- +=============================================================================================================+============================================================================================================+=======================================================================================================+============================================================================================================================+
-- | Primary                                                                                                     | Both                                                                                                       | Transfer                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Graphics                                                                                              |                                                                                                                            |
-- |                                                                                                             |                                                                                                            | Compute                                                                                               |                                                                                                                            |
-- +-------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCmdExecuteCommands" vkCmdExecuteCommands :: ("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
-- | VkClearRect - Structure specifying a clear rectangle
--
-- = Description
--
-- The layers [@baseArrayLayer@, @baseArrayLayer@ + @layerCount@) counting
-- from the base layer of the attachment image view are cleared.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D', 'vkCmdClearAttachments'
data VkClearRect = VkClearRect
  { -- | @rect@ is the two-dimensional region to be cleared.
  vkRect :: VkRect2D
  , -- | @baseArrayLayer@ is the first layer to be cleared.
  vkBaseArrayLayer :: Word32
  , -- | @layerCount@ is the number of layers to clear.
  vkLayerCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkClearRect where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearRect <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRect (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 16) (vkBaseArrayLayer (poked :: VkClearRect))
                *> poke (ptr `plusPtr` 20) (vkLayerCount (poked :: VkClearRect))
-- | VkImageSubresourceLayers - Structure specifying a image subresource
-- layers
--
-- == Valid Usage
--
-- -   If @aspectMask@ contains @VK_IMAGE_ASPECT_COLOR_BIT@, it /must/ not
--     contain either of @VK_IMAGE_ASPECT_DEPTH_BIT@ or
--     @VK_IMAGE_ASPECT_STENCIL_BIT@
--
-- -   @aspectMask@ /must/ not contain @VK_IMAGE_ASPECT_METADATA_BIT@
--
-- -   @layerCount@ /must/ be greater than 0
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'VkImageBlit', 'VkImageCopy', 'VkImageResolve'
data VkImageSubresourceLayers = VkImageSubresourceLayers
  { -- | @aspectMask@ is a combination of
  -- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits',
  -- selecting the color, depth and\/or stencil aspects to be copied.
  vkAspectMask :: VkImageAspectFlags
  , -- | @mipLevel@ is the mipmap level to copy from.
  vkMipLevel :: Word32
  , -- | @baseArrayLayer@ and @layerCount@ are the starting layer and number of
  -- layers to copy.
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "layerCount"
  vkLayerCount :: Word32
  }
  deriving (Eq, Show)

instance Storable VkImageSubresourceLayers where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkImageSubresourceLayers <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 4) (vkMipLevel (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 8) (vkBaseArrayLayer (poked :: VkImageSubresourceLayers))
                *> poke (ptr `plusPtr` 12) (vkLayerCount (poked :: VkImageSubresourceLayers))
-- | VkMemoryBarrier - Structure specifying a global memory barrier
--
-- = Description
--
-- The first [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access types in the [source access
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks)
-- specified by @srcAccessMask@.
--
-- The second [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access types in the [destination access
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks)
-- specified by @dstAccessMask@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_MEMORY_BARRIER@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @srcAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' values
--
-- -   @dstAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdPipelineBarrier',
-- 'vkCmdWaitEvents'
data VkMemoryBarrier = VkMemoryBarrier
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' specifying a [source
  -- access
  -- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
  vkSrcAccessMask :: VkAccessFlags
  , -- | @dstAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' specifying a [destination
  -- access
  -- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
  vkDstAccessMask :: VkAccessFlags
  }
  deriving (Eq, Show)

instance Storable VkMemoryBarrier where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryBarrier <$> peek (ptr `plusPtr` 0)
                             <*> peek (ptr `plusPtr` 8)
                             <*> peek (ptr `plusPtr` 16)
                             <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkMemoryBarrier))
-- | VkBufferMemoryBarrier - Structure specifying a buffer memory barrier
--
-- = Description
--
-- The first [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access to memory through the specified buffer range, via
-- access types in the [source access
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks)
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@, memory writes performed by that access type
-- are also made visible, as that access type is not performed through a
-- resource.
--
-- The second [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access to memory through the specified buffer range, via
-- access types in the [destination access
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@ or @VK_ACCESS_HOST_READ_BIT@, available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a [queue family release
-- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-release)
-- for the specified buffer range, and the second access scope includes no
-- access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a [queue family acquire
-- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-acquire)
-- for the specified buffer range, and the first access scope includes no
-- access, as if @srcAccessMask@ was @0@.
--
-- == Valid Usage
--
-- -   @offset@ /must/ be less than the size of @buffer@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be greater
--     than @0@
--
-- -   If @size@ is not equal to @VK_WHOLE_SIZE@, @size@ /must/ be less
--     than or equal to than the size of @buffer@ minus @offset@
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_CONCURRENT@, at least one of @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ /must/ be @VK_QUEUE_FAMILY_IGNORED@
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_CONCURRENT@, and one of @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ is @VK_QUEUE_FAMILY_IGNORED@, the other /must/
--     be @VK_QUEUE_FAMILY_IGNORED@ or a special queue family reserved for
--     external memory ownership transfers, as described in
--     [{html_spec_relative}#synchronization-queue-transfers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@ and @srcQueueFamilyIndex@ is
--     @VK_QUEUE_FAMILY_IGNORED@, @dstQueueFamilyIndex@ /must/ also be
--     @VK_QUEUE_FAMILY_IGNORED@
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@ and @srcQueueFamilyIndex@ is not
--     @VK_QUEUE_FAMILY_IGNORED@, it /must/ be a valid queue family or a
--     special queue family reserved for external memory transfers, as
--     described in
--     [{html_spec_relative}#synchronization-queue-transfers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@ and @dstQueueFamilyIndex@ is not
--     @VK_QUEUE_FAMILY_IGNORED@, it /must/ be a valid queue family or a
--     special queue family reserved for external memory transfers, as
--     described in
--     [{html_spec_relative}#synchronization-queue-transfers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@, and @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ are not @VK_QUEUE_FAMILY_IGNORED@, at least
--     one of them /must/ be the same as the family of the queue that will
--     execute this barrier
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @srcAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' values
--
-- -   @dstAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' values
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdPipelineBarrier',
-- 'vkCmdWaitEvents'
data VkBufferMemoryBarrier = VkBufferMemoryBarrier
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' specifying a [source
  -- access
  -- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
  vkSrcAccessMask :: VkAccessFlags
  , -- | @dstAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' specifying a [destination
  -- access
  -- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
  vkDstAccessMask :: VkAccessFlags
  , -- | @srcQueueFamilyIndex@ is the source queue family for a [queue family
  -- ownership
  -- transfer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
  vkSrcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a [queue
  -- family ownership
  -- transfer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
  vkDstQueueFamilyIndex :: Word32
  , -- | @buffer@ is a handle to the buffer whose backing memory is affected by
  -- the barrier.
  vkBuffer :: VkBuffer
  , -- | @offset@ is an offset in bytes into the backing memory for @buffer@;
  -- this is relative to the base offset as bound to the buffer (see
  -- 'Graphics.Vulkan.Core10.MemoryManagement.vkBindBufferMemory').
  vkOffset :: VkDeviceSize
  , -- | @size@ is a size in bytes of the affected area of backing memory for
  -- @buffer@, or @VK_WHOLE_SIZE@ to use the range from @offset@ to the end
  -- of the buffer.
  vkSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBufferMemoryBarrier where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
                                   <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkSrcQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkDstQueueFamilyIndex (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkBuffer (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkOffset (poked :: VkBufferMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSize (poked :: VkBufferMemoryBarrier))
-- | VkImageMemoryBarrier - Structure specifying the parameters of an image
-- memory barrier
--
-- = Description
--
-- The first [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access to memory through the specified image subresource
-- range, via access types in the [source access
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks)
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@, memory writes performed by that access type
-- are also made visible, as that access type is not performed through a
-- resource.
--
-- The second [access
-- scope](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-dependencies-access-scopes)
-- is limited to access to memory through the specified image subresource
-- range, via access types in the [destination access
-- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks)
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@ or @VK_ACCESS_HOST_READ_BIT@, available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a [queue family release
-- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-release)
-- for the specified image subresource range, and the second access scope
-- includes no access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a [queue family acquire
-- operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers-acquire)
-- for the specified image subresource range, and the first access scope
-- includes no access, as if @srcAccessMask@ was @0@.
--
-- If @oldLayout@ is not equal to @newLayout@, then the memory barrier
-- defines an [image layout
-- transition](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-image-layout-transitions)
-- for the specified image subresource range.
--
-- Layout transitions that are performed via image memory barriers execute
-- in their entirety in [submission
-- order](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-submission-order),
-- relative to other image layout transitions submitted to the same queue,
-- including those performed by [render
-- passes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass).
-- In effect there is an implicit execution dependency from each such
-- layout transition to all layout transitions previously submitted to the
-- same queue.
--
-- The image layout of each image subresource of a depth\/stencil image
-- created with @VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT@
-- is dependent on the last sample locations used to render to the image
-- subresource as a depth\/stencil attachment, thus when the @image@ member
-- of an @VkImageMemoryBarrier@ is an image created with this flag the
-- application /can/ chain a
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT'
-- structure to the @pNext@ chain of @VkImageMemoryBarrier@ to specify the
-- sample locations to use during the image layout transition.
--
-- If the @VkSampleLocationsInfoEXT@ structure in the @pNext@ chain of
-- @VkImageMemoryBarrier@ does not match the sample location state last
-- used to render to the image subresource range specified by
-- @subresourceRange@ or if no @VkSampleLocationsInfoEXT@ structure is in
-- the @pNext@ chain of @VkImageMemoryBarrier@ then the contents of the
-- given image subresource range becomes undefined as if @oldLayout@ would
-- equal @VK_IMAGE_LAYOUT_UNDEFINED@.
--
-- If @image@ has a multi-planar format and the image is /disjoint/, then
-- including @VK_IMAGE_ASPECT_COLOR_BIT@ in the @aspectMask@ member of
-- @subresourceRange@ is equivalent to including
-- @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, and (for
-- three-plane formats only) @VK_IMAGE_ASPECT_PLANE_2_BIT@.
--
-- == Valid Usage
--
-- -   @oldLayout@ /must/ be @VK_IMAGE_LAYOUT_UNDEFINED@ or the current
--     layout of the image subresources affected by the barrier
--
-- -   @newLayout@ /must/ not be @VK_IMAGE_LAYOUT_UNDEFINED@ or
--     @VK_IMAGE_LAYOUT_PREINITIALIZED@
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_CONCURRENT@, at least one of @srcQueueFamilyIndex@
--     and @dstQueueFamilyIndex@ /must/ be @VK_QUEUE_FAMILY_IGNORED@
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_CONCURRENT@, and one of @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ is @VK_QUEUE_FAMILY_IGNORED@, the other /must/
--     be @VK_QUEUE_FAMILY_IGNORED@ or a special queue family reserved for
--     external memory transfers, as described in
--     [{html_spec_relative}#synchronization-queue-transfers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@ and @srcQueueFamilyIndex@ is
--     @VK_QUEUE_FAMILY_IGNORED@, @dstQueueFamilyIndex@ /must/ also be
--     @VK_QUEUE_FAMILY_IGNORED@.
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@ and @srcQueueFamilyIndex@ is not
--     @VK_QUEUE_FAMILY_IGNORED@, it /must/ be a valid queue family or a
--     special queue family reserved for external memory transfers, as
--     described in
--     [{html_spec_relative}#synchronization-queue-transfers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@ and @dstQueueFamilyIndex@ is not
--     @VK_QUEUE_FAMILY_IGNORED@, it /must/ be a valid queue family or a
--     special queue family reserved for external memory transfers, as
--     described in
--     [{html_spec_relative}#synchronization-queue-transfers](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@, and @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ are not @VK_QUEUE_FAMILY_IGNORED@, at least
--     one of them /must/ be the same as the family of the queue that will
--     execute this barrier
--
-- -   @subresourceRange.baseMipLevel@ /must/ be less than the @mipLevels@
--     specified in 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when
--     @image@ was created
--
-- -   If @subresourceRange.levelCount@ is not @VK_REMAINING_MIP_LEVELS@,
--     @subresourceRange.baseMipLevel@ + @subresourceRange.levelCount@
--     /must/ be less than or equal to the @mipLevels@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   @subresourceRange.baseArrayLayer@ /must/ be less than the
--     @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @subresourceRange.layerCount@ is not @VK_REMAINING_ARRAY_LAYERS@,
--     @subresourceRange.baseArrayLayer@ + @subresourceRange.layerCount@
--     /must/ be less than or equal to the @arrayLayers@ specified in
--     'Graphics.Vulkan.Core10.Image.VkImageCreateInfo' when @image@ was
--     created
--
-- -   If @image@ has a depth\/stencil format with both depth and stencil
--     components, then the @aspectMask@ member of @subresourceRange@
--     /must/ include both @VK_IMAGE_ASPECT_DEPTH_BIT@ and
--     @VK_IMAGE_ASPECT_STENCIL_BIT@
--
-- -   If @image@ has a single-plane color format or is not /disjoint/,
--     then the @aspectMask@ member of @subresourceRange@ /must/ be
--     @VK_IMAGE_ASPECT_COLOR_BIT@
--
-- -   If @image@ has a multi-planar format and the image is /disjoint/,
--     then the @aspectMask@ member of @subresourceRange@ /must/ include
--     either at least one of @VK_IMAGE_ASPECT_PLANE_0_BIT@,
--     @VK_IMAGE_ASPECT_PLANE_1_BIT@, and @VK_IMAGE_ASPECT_PLANE_2_BIT@; or
--     /must/ include @VK_IMAGE_ASPECT_COLOR_BIT@
--
-- -   If @image@ has a multi-planar format with only two planes, then the
--     @aspectMask@ member of @subresourceRange@ /must/ not include
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL@ then @image@ /must/ have
--     been created with @VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL@ then @image@
--     /must/ have been created with
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL@ then @image@
--     /must/ have been created with
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL@ then
--     @image@ /must/ have been created with
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL@ then
--     @image@ /must/ have been created with
--     @VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL@ then @image@ /must/ have
--     been created with @VK_IMAGE_USAGE_SAMPLED_BIT@ or
--     @VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ set
--
-- -   If either @oldLayout@ or @newLayout@ is
--     @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ then @image@ /must/ have been
--     created with @VK_IMAGE_USAGE_TRANSFER_DST_BIT@ set
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER@
--
-- -   @pNext@ /must/ be @NULL@ or a pointer to a valid instance of
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT'
--
-- -   @srcAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' values
--
-- -   @dstAccessMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' values
--
-- -   @oldLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @newLayout@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Image.VkImageLayout' value
--
-- -   @image@ /must/ be a valid @VkImage@ handle
--
-- -   @subresourceRange@ /must/ be a valid @VkImageSubresourceRange@
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdPipelineBarrier',
-- 'vkCmdWaitEvents'
data VkImageMemoryBarrier = VkImageMemoryBarrier
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @srcAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' specifying a [source
  -- access
  -- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
  vkSrcAccessMask :: VkAccessFlags
  , -- | @dstAccessMask@ is a bitmask of
  -- 'Graphics.Vulkan.Core10.Pass.VkAccessFlagBits' specifying a [destination
  -- access
  -- mask](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-access-masks).
  vkDstAccessMask :: VkAccessFlags
  , -- | @oldLayout@ is the old layout in an [image layout
  -- transition](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-image-layout-transitions).
  vkOldLayout :: VkImageLayout
  , -- | @newLayout@ is the new layout in an [image layout
  -- transition](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-image-layout-transitions).
  vkNewLayout :: VkImageLayout
  , -- | @srcQueueFamilyIndex@ is the source queue family for a [queue family
  -- ownership
  -- transfer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
  vkSrcQueueFamilyIndex :: Word32
  , -- | @dstQueueFamilyIndex@ is the destination queue family for a [queue
  -- family ownership
  -- transfer](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#synchronization-queue-transfers).
  vkDstQueueFamilyIndex :: Word32
  , -- | @image@ is a handle to the image affected by this barrier.
  vkImage :: VkImage
  , -- | @subresourceRange@ describes the [image subresource
  -- range](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#resources-image-views)
  -- within @image@ that is affected by this barrier.
  vkSubresourceRange :: VkImageSubresourceRange
  }
  deriving (Eq, Show)

instance Storable VkImageMemoryBarrier where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek ptr = VkImageMemoryBarrier <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 20)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 28)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 36)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 16) (vkSrcAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 20) (vkDstAccessMask (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 24) (vkOldLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 28) (vkNewLayout (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 32) (vkSrcQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 36) (vkDstQueueFamilyIndex (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 40) (vkImage (poked :: VkImageMemoryBarrier))
                *> poke (ptr `plusPtr` 48) (vkSubresourceRange (poked :: VkImageMemoryBarrier))
-- | VkBufferCopy - Structure specifying a buffer copy operation
--
-- = See Also
--
-- @VkDeviceSize@, 'vkCmdCopyBuffer'
data VkBufferCopy = VkBufferCopy
  { -- | @srcOffset@ is the starting offset in bytes from the start of
  -- @srcBuffer@.
  vkSrcOffset :: VkDeviceSize
  , -- | @dstOffset@ is the starting offset in bytes from the start of
  -- @dstBuffer@.
  vkDstOffset :: VkDeviceSize
  , -- | @size@ is the number of bytes to copy.
  vkSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBufferCopy where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkBufferCopy <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 8) (vkDstOffset (poked :: VkBufferCopy))
                *> poke (ptr `plusPtr` 16) (vkSize (poked :: VkBufferCopy))
-- | VkImageCopy - Structure specifying an image copy operation
--
-- = Description
--
-- For @VK_IMAGE_TYPE_3D@ images, copies are performed slice by slice
-- starting with the @z@ member of the @srcOffset@ or @dstOffset@, and
-- copying @depth@ slices. For images with multiple layers, copies are
-- performed layer by layer starting with the @baseArrayLayer@ member of
-- the @srcSubresource@ or @dstSubresource@ and copying @layerCount@
-- layers. Image data /can/ be copied between images with different image
-- types. If one image is @VK_IMAGE_TYPE_3D@ and the other image is
-- @VK_IMAGE_TYPE_2D@ with multiple layers, then each slice is copied to or
-- from a different layer.
--
-- Copies involving a [multi-planar image
-- format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
-- specify the region to be copied in terms of the /plane/ to be copied,
-- not the coordinates of the multi-planar image. This means that copies
-- accessing the R\/B planes of “@_422@” format images /must/ fit the
-- copied region within half the @width@ of the parent image, and that
-- copies accessing the R\/B planes of “@_420@” format images /must/ fit
-- the copied region within half the @width@ and @height@ of the parent
-- image.
--
-- == Valid Usage
--
-- -   If neither the calling command’s @srcImage@ nor the calling
--     command’s @dstImage@ has a [multi-planar image
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     then the @aspectMask@ member of @srcSubresource@ and
--     @dstSubresource@ /must/ match
--
-- -   If the calling command’s @srcImage@ has a
--     'Graphics.Vulkan.Core10.Core.VkFormat' with [two
--     planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     then the @srcSubresource@ @aspectMask@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@ or @VK_IMAGE_ASPECT_PLANE_1_BIT@
--
-- -   If the calling command’s @srcImage@ has a
--     'Graphics.Vulkan.Core10.Core.VkFormat' with [three
--     planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     then the @srcSubresource@ @aspectMask@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- -   If the calling command’s @dstImage@ has a
--     'Graphics.Vulkan.Core10.Core.VkFormat' with [two
--     planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     then the @dstSubresource@ @aspectMask@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@ or @VK_IMAGE_ASPECT_PLANE_1_BIT@
--
-- -   If the calling command’s @dstImage@ has a
--     'Graphics.Vulkan.Core10.Core.VkFormat' with [three
--     planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     then the @dstSubresource@ @aspectMask@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@
--
-- -   If the calling command’s @srcImage@ has a [multi-planar image
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     and the @dstImage@ does not have a multi-planar image format, the
--     @dstSubresource@ @aspectMask@ /must/ be @VK_IMAGE_ASPECT_COLOR_BIT@
--
-- -   If the calling command’s @dstImage@ has a [multi-planar image
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--     and the @srcImage@ does not have a multi-planar image format, the
--     @srcSubresource@ @aspectMask@ /must/ be @VK_IMAGE_ASPECT_COLOR_BIT@
--
-- -   The number of slices of the @extent@ (for 3D) or layers of the
--     @srcSubresource@ (for non-3D) /must/ match the number of slices of
--     the @extent@ (for 3D) or layers of the @dstSubresource@ (for non-3D)
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType'
--     @VK_IMAGE_TYPE_3D@, the @baseArrayLayer@ and @layerCount@ members of
--     the corresponding subresource /must/ be @0@ and @1@, respectively
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
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @srcOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @srcOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @dstOffset.z@ /must/ be @0@ and @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_2D@,
--     then @srcOffset.z@ /must/ be @0@.
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_2D@,
--     then @dstOffset.z@ /must/ be @0@.
--
-- -   If both @srcImage@ and @dstImage@ are of type @VK_IMAGE_TYPE_2D@
--     then then @extent.depth@ /must/ be @1@.
--
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_2D@,
--     and the @dstImage@ is of type @VK_IMAGE_TYPE_3D@, then
--     @extent.depth@ /must/ equal to the @layerCount@ member of
--     @srcSubresource@.
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_2D@,
--     and the @srcImage@ is of type @VK_IMAGE_TYPE_3D@, then
--     @extent.depth@ /must/ equal to the @layerCount@ member of
--     @dstSubresource@.
--
-- -   @dstOffset.x@ and (@extent.width@ + @dstOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource width
--
-- -   @dstOffset.y@ and (@extent.height@ + @dstOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @dstOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource depth
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, all members of @srcOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @srcOffset.x@) /must/ equal the source image subresource width
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @srcOffset.y@) /must/ equal the source image subresource height
--
-- -   If the calling command’s @srcImage@ is a compressed image, or a
--     /single-plane/, “@_422@” image format, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @srcOffset.z@) /must/ equal the source image subresource depth
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, all members of @dstOffset@
--     /must/ be a multiple of the corresponding dimensions of the
--     compressed texel block
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, @extent.width@ /must/ be a
--     multiple of the compressed texel block width or (@extent.width@ +
--     @dstOffset.x@) /must/ equal the destination image subresource width
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, @extent.height@ /must/ be a
--     multiple of the compressed texel block height or (@extent.height@ +
--     @dstOffset.y@) /must/ equal the destination image subresource height
--
-- -   If the calling command’s @dstImage@ is a compressed format image, or
--     a /single-plane/, “@_422@” image format, @extent.depth@ /must/ be a
--     multiple of the compressed texel block depth or (@extent.depth@ +
--     @dstOffset.z@) /must/ equal the destination image subresource depth
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- -   @dstSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdCopyImage'
data VkImageCopy = VkImageCopy
  { -- | @srcSubresource@ and @dstSubresource@ are 'VkImageSubresourceLayers'
  -- structures specifying the image subresources of the images used for the
  -- source and destination image data, respectively.
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
  -- in texels of the sub-regions of the source and destination image data.
  vkSrcOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageCopy" "dstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "dstOffset"
  vkDstOffset :: VkOffset3D
  , -- | @extent@ is the size in texels of the image to copy in @width@, @height@
  -- and @depth@.
  vkExtent :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkImageCopy where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkImageCopy <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 28)
                         <*> peek (ptr `plusPtr` 44)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageCopy))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageCopy))
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
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType'
--     @VK_IMAGE_TYPE_3D@, the @baseArrayLayer@ and @layerCount@ members of
--     both @srcSubresource@ and @dstSubresource@ /must/ be @0@ and @1@,
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
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @srcOffset@[0].y /must/ be @0@ and @srcOffset@[1].y /must/ be
--     @1@.
--
-- -   @srcOffset@[0].@z@ and @srcOffset@[1].@z@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the source image
--     subresource depth
--
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_1D@ or
--     @VK_IMAGE_TYPE_2D@, then @srcOffset@[0].z /must/ be @0@ and
--     @srcOffset@[1].z /must/ be @1@.
--
-- -   @dstOffset@[0].@x@ and @dstOffset@[1].@x@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the destination image
--     subresource width
--
-- -   @dstOffset@[0].@y@ and @dstOffset@[1].@y@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the destination image
--     subresource height
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @dstOffset@[0].y /must/ be @0@ and @dstOffset@[1].y /must/ be
--     @1@.
--
-- -   @dstOffset@[0].@z@ and @dstOffset@[1].@z@ /must/ both be greater
--     than or equal to @0@ and less than or equal to the destination image
--     subresource depth
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_1D@ or
--     @VK_IMAGE_TYPE_2D@, then @dstOffset@[0].z /must/ be @0@ and
--     @dstOffset@[1].z /must/ be @1@.
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- -   @dstSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- = See Also
--
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdBlitImage'
data VkImageBlit = VkImageBlit
  { -- | @srcSubresource@ is the subresource to blit from.
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- | @srcOffsets@ is an array of two
  -- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D'
  -- structures specifying the bounds of the source region within
  -- @srcSubresource@.
  vkSrcOffsets :: Vector 2 VkOffset3D
  , -- | @dstSubresource@ is the subresource to blit into.
  vkDstSubresource :: VkImageSubresourceLayers
  , -- | @dstOffsets@ is an array of two
  -- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D'
  -- structures specifying the bounds of the destination region within
  -- @dstSubresource@.
  vkDstOffsets :: Vector 2 VkOffset3D
  }
  deriving (Eq, Show)

instance Storable VkImageBlit where
  sizeOf ~_ = 80
  alignment ~_ = 4
  peek ptr = VkImageBlit <$> peek (ptr `plusPtr` 0)
                         <*> peek (ptr `plusPtr` 16)
                         <*> peek (ptr `plusPtr` 40)
                         <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 16) (vkSrcOffsets (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 40) (vkDstSubresource (poked :: VkImageBlit))
                *> poke (ptr `plusPtr` 56) (vkDstOffsets (poked :: VkImageBlit))
-- | VkBufferImageCopy - Structure specifying a buffer image copy operation
--
-- = Description
--
-- When copying to or from a depth or stencil aspect, the data in buffer
-- memory uses a layout that is a (mostly) tightly packed representation of
-- the depth or stencil data. Specifically:
--
-- -   data copied to or from the stencil aspect of any depth\/stencil
--     format is tightly packed with one @VK_FORMAT_S8_UINT@ value per
--     texel.
--
-- -   data copied to or from the depth aspect of a @VK_FORMAT_D16_UNORM@
--     or @VK_FORMAT_D16_UNORM_S8_UINT@ format is tightly packed with one
--     @VK_FORMAT_D16_UNORM@ value per texel.
--
-- -   data copied to or from the depth aspect of a @VK_FORMAT_D32_SFLOAT@
--     or @VK_FORMAT_D32_SFLOAT_S8_UINT@ format is tightly packed with one
--     @VK_FORMAT_D32_SFLOAT@ value per texel.
--
-- -   data copied to or from the depth aspect of a
--     @VK_FORMAT_X8_D24_UNORM_PACK32@ or @VK_FORMAT_D24_UNORM_S8_UINT@
--     format is packed with one 32-bit word per texel with the D24 value
--     in the LSBs of the word, and undefined values in the eight MSBs.
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
-- queues that do not support graphics. When copying to a depth aspect, the
-- data in buffer memory /must/ be in the the range [0,1] or undefined
-- results occur.
--
-- Copies are done layer by layer starting with image layer
-- @baseArrayLayer@ member of @imageSubresource@. @layerCount@ layers are
-- copied from the source image or to the destination image.
--
-- == Valid Usage
--
-- -   If the calling command’s @VkImage@ parameter’s format is not a
--     depth\/stencil format or a [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     then @bufferOffset@ /must/ be a multiple of the format’s element
--     size
--
-- -   If the calling command’s @VkImage@ parameter’s format is a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     then @bufferOffset@ /must/ be a multiple of the element size of the
--     compatible format for the format and the @aspectMask@ of the
--     @imageSubresource@ as defined in
--     [{html_spec_relative}#features-formats-compatible-planes](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-compatible-planes)
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
--     image subresource width where this refers to the width of the
--     /plane/ of the image involved in the copy in the case of a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--
-- -   @imageOffset.y@ and (imageExtent.height + @imageOffset.y@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource height where this refers to the height of the
--     /plane/ of the image involved in the copy in the case of a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion)
--
-- -   If the calling command’s @srcImage@ ('vkCmdCopyImageToBuffer') or
--     @dstImage@ ('vkCmdCopyBufferToImage') is of type @VK_IMAGE_TYPE_1D@,
--     then @imageOffset.y@ /must/ be @0@ and @imageExtent.height@ /must/
--     be @1@.
--
-- -   @imageOffset.z@ and (imageExtent.depth + @imageOffset.z@) /must/
--     both be greater than or equal to @0@ and less than or equal to the
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ ('vkCmdCopyImageToBuffer') or
--     @dstImage@ ('vkCmdCopyBufferToImage') is of type @VK_IMAGE_TYPE_1D@
--     or @VK_IMAGE_TYPE_2D@, then @imageOffset.z@ /must/ be @0@ and
--     @imageExtent.depth@ /must/ be @1@
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, @bufferRowLength@ /must/
--     be a multiple of the compressed texel block width
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, @bufferImageHeight@
--     /must/ be a multiple of the compressed texel block height
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, all members of
--     @imageOffset@ /must/ be a multiple of the corresponding dimensions
--     of the compressed texel block
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, @bufferOffset@ /must/ be
--     a multiple of the compressed texel block size in bytes
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, @imageExtent.width@
--     /must/ be a multiple of the compressed texel block width or
--     (@imageExtent.width@ + @imageOffset.x@) /must/ equal the image
--     subresource width
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, @imageExtent.height@
--     /must/ be a multiple of the compressed texel block height or
--     (@imageExtent.height@ + @imageOffset.y@) /must/ equal the image
--     subresource height
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     or a /single-plane/, “@_422@” image format, @imageExtent.depth@
--     /must/ be a multiple of the compressed texel block depth or
--     (@imageExtent.depth@ + @imageOffset.z@) /must/ equal the image
--     subresource depth
--
-- -   The @aspectMask@ member of @imageSubresource@ /must/ specify aspects
--     present in the calling command’s @VkImage@ parameter
--
-- -   If the calling command’s @VkImage@ parameter’s format is a
--     [multi-planar
--     format](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-formats-requiring-sampler-ycbcr-conversion),
--     then the @aspectMask@ member of @imageSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_PLANE_0_BIT@, @VK_IMAGE_ASPECT_PLANE_1_BIT@, or
--     @VK_IMAGE_ASPECT_PLANE_2_BIT@ (with @VK_IMAGE_ASPECT_PLANE_2_BIT@
--     valid only for image formats with three planes)
--
-- -   The @aspectMask@ member of @imageSubresource@ /must/ only have a
--     single bit set
--
-- -   If the calling command’s @VkImage@ parameter is of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType'
--     @VK_IMAGE_TYPE_3D@, the @baseArrayLayer@ and @layerCount@ members of
--     @imageSubresource@ /must/ be @0@ and @1@, respectively
--
-- -   When copying to the depth aspect of an image subresource, the data
--     in the source buffer /must/ be in the range [0,1]
--
-- == Valid Usage (Implicit)
--
-- -   @imageSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- = See Also
--
-- @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdCopyBufferToImage', 'vkCmdCopyImageToBuffer'
data VkBufferImageCopy = VkBufferImageCopy
  { -- | @bufferOffset@ is the offset in bytes from the start of the buffer
  -- object where the image data is copied from or to.
  vkBufferOffset :: VkDeviceSize
  , -- | @bufferRowLength@ and @bufferImageHeight@ specify the data in buffer
  -- memory as a subregion of a larger two- or three-dimensional image, and
  -- control the addressing calculations of data in buffer memory. If either
  -- of these values is zero, that aspect of the buffer memory is considered
  -- to be tightly packed according to the @imageExtent@.
  vkBufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "bufferImageHeight"
  vkBufferImageHeight :: Word32
  , -- | @imageSubresource@ is a 'VkImageSubresourceLayers' used to specify the
  -- specific image subresources of the image used for the source or
  -- destination image data.
  vkImageSubresource :: VkImageSubresourceLayers
  , -- | @imageOffset@ selects the initial @x@, @y@, @z@ offsets in texels of the
  -- sub-region of the source or destination image data.
  vkImageOffset :: VkOffset3D
  , -- | @imageExtent@ is the size in texels of the image to copy in @width@,
  -- @height@ and @depth@.
  vkImageExtent :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkBufferImageCopy where
  sizeOf ~_ = 56
  alignment ~_ = 8
  peek ptr = VkBufferImageCopy <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 12)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkBufferOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 8) (vkBufferRowLength (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 12) (vkBufferImageHeight (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 16) (vkImageSubresource (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 32) (vkImageOffset (poked :: VkBufferImageCopy))
                *> poke (ptr `plusPtr` 44) (vkImageExtent (poked :: VkBufferImageCopy))
-- | VkImageResolve - Structure specifying an image resolve operation
--
-- == Valid Usage
--
-- -   The @aspectMask@ member of @srcSubresource@ and @dstSubresource@
--     /must/ only contain @VK_IMAGE_ASPECT_COLOR_BIT@
--
-- -   The @layerCount@ member of @srcSubresource@ and @dstSubresource@
--     /must/ match
--
-- -   If either of the calling command’s @srcImage@ or @dstImage@
--     parameters are of
--     'Graphics.Vulkan.Core10.DeviceInitialization.VkImageType'
--     @VK_IMAGE_TYPE_3D@, the @baseArrayLayer@ and @layerCount@ members of
--     both @srcSubresource@ and @dstSubresource@ /must/ be @0@ and @1@,
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
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @srcOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @srcOffset.z@ and (@extent.depth@ + @srcOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the source
--     image subresource depth
--
-- -   If the calling command’s @srcImage@ is of type @VK_IMAGE_TYPE_1D@ or
--     @VK_IMAGE_TYPE_2D@, then @srcOffset.z@ /must/ be @0@ and
--     @extent.depth@ /must/ be @1@.
--
-- -   @dstOffset.x@ and (@extent.width@ + @dstOffset.x@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource width
--
-- -   @dstOffset.y@ and (@extent.height@ + @dstOffset.y@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource height
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_1D@,
--     then @dstOffset.y@ /must/ be @0@ and @extent.height@ /must/ be @1@.
--
-- -   @dstOffset.z@ and (@extent.depth@ + @dstOffset.z@) /must/ both be
--     greater than or equal to @0@ and less than or equal to the
--     destination image subresource depth
--
-- -   If the calling command’s @dstImage@ is of type @VK_IMAGE_TYPE_1D@ or
--     @VK_IMAGE_TYPE_2D@, then @dstOffset.z@ /must/ be @0@ and
--     @extent.depth@ /must/ be @1@.
--
-- == Valid Usage (Implicit)
--
-- -   @srcSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- -   @dstSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdResolveImage'
data VkImageResolve = VkImageResolve
  { -- | @srcSubresource@ and @dstSubresource@ are 'VkImageSubresourceLayers'
  -- structures specifying the image subresources of the images used for the
  -- source and destination image data, respectively. Resolve of
  -- depth\/stencil images is not supported.
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- | @srcOffset@ and @dstOffset@ select the initial @x@, @y@, and @z@ offsets
  -- in texels of the sub-regions of the source and destination image data.
  vkSrcOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageResolve" "dstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "dstOffset"
  vkDstOffset :: VkOffset3D
  , -- | @extent@ is the size in texels of the source image to resolve in
  -- @width@, @height@ and @depth@.
  vkExtent :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkImageResolve where
  sizeOf ~_ = 68
  alignment ~_ = 4
  peek ptr = VkImageResolve <$> peek (ptr `plusPtr` 0)
                            <*> peek (ptr `plusPtr` 16)
                            <*> peek (ptr `plusPtr` 28)
                            <*> peek (ptr `plusPtr` 44)
                            <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSrcSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 16) (vkSrcOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 28) (vkDstSubresource (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 44) (vkDstOffset (poked :: VkImageResolve))
                *> poke (ptr `plusPtr` 56) (vkExtent (poked :: VkImageResolve))
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
-- area, otherwise the pixels outside of the render area become undefined
-- and shader side effects /may/ occur for fragments outside the render
-- area. The render area /must/ be contained within the framebuffer
-- dimensions.
--
-- When multiview is enabled, the resolve operation at the end of a subpass
-- applies to all views in the view mask.
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
--     @VK_ATTACHMENT_LOAD_OP_CLEAR@
--
-- -   If @clearValueCount@ is not @0@, @pClearValues@ /must/ be a valid
--     pointer to an array of @clearValueCount@ valid @VkClearValue@ unions
--
-- -   @renderPass@ /must/ be
--     [compatible](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#renderpass-compatibility)
--     with the @renderPass@ member of the @VkFramebufferCreateInfo@
--     structure specified when creating @framebuffer@.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO@
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo'
--     or
--     'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkRenderPassSampleLocationsBeginInfoEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @renderPass@ /must/ be a valid @VkRenderPass@ handle
--
-- -   @framebuffer@ /must/ be a valid @VkFramebuffer@ handle
--
-- -   Both of @framebuffer@, and @renderPass@ /must/ have been created,
--     allocated, or retrieved from the same @VkDevice@
--
-- = See Also
--
-- 'VkClearValue', 'Graphics.Vulkan.Core10.Pass.VkFramebuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdBeginRenderPass'
data VkRenderPassBeginInfo = VkRenderPassBeginInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @renderPass@ is the render pass to begin an instance of.
  vkRenderPass :: VkRenderPass
  , -- | @framebuffer@ is the framebuffer containing the attachments that are
  -- used with the render pass.
  vkFramebuffer :: VkFramebuffer
  , -- | @renderArea@ is the render area that is affected by the render pass
  -- instance, and is described in more detail below.
  vkRenderArea :: VkRect2D
  , -- | @clearValueCount@ is the number of elements in @pClearValues@.
  vkClearValueCount :: Word32
  , -- | @pClearValues@ is an array of 'VkClearValue' structures that contains
  -- clear values for each attachment, if the attachment uses a @loadOp@
  -- value of @VK_ATTACHMENT_LOAD_OP_CLEAR@ or if the attachment has a
  -- depth\/stencil format and uses a @stencilLoadOp@ value of
  -- @VK_ATTACHMENT_LOAD_OP_CLEAR@. The array is indexed by attachment
  -- number. Only elements corresponding to cleared attachments are used.
  -- Other elements of @pClearValues@ are ignored.
  vkPClearValues :: Ptr VkClearValue
  }
  deriving (Eq, Show)

instance Storable VkRenderPassBeginInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkRenderPassBeginInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 48)
                                   <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 16) (vkRenderPass (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 24) (vkFramebuffer (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 32) (vkRenderArea (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 48) (vkClearValueCount (poked :: VkRenderPassBeginInfo))
                *> poke (ptr `plusPtr` 56) (vkPClearValues (poked :: VkRenderPassBeginInfo))
-- | VkClearDepthStencilValue - Structure specifying a clear depth stencil
-- value
--
-- == Valid Usage
--
-- -   Unless the @{html_spec_relative}#VK_EXT_depth_range_unrestricted@
--     extension is enabled @depth@ /must/ be between @0.0@ and @1.0@,
--     inclusive
--
-- = See Also
--
-- 'VkClearValue', 'vkCmdClearDepthStencilImage'
data VkClearDepthStencilValue = VkClearDepthStencilValue
  { -- | @depth@ is the clear value for the depth aspect of the depth\/stencil
  -- attachment. It is a floating-point value which is automatically
  -- converted to the attachment’s format.
  vkDepth :: CFloat
  , -- | @stencil@ is the clear value for the stencil aspect of the
  -- depth\/stencil attachment. It is a 32-bit integer value which is
  -- converted to the attachment’s format by taking the appropriate number of
  -- LSBs.
  vkStencil :: Word32
  }
  deriving (Eq, Show)

instance Storable VkClearDepthStencilValue where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkClearDepthStencilValue <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkDepth (poked :: VkClearDepthStencilValue))
                *> poke (ptr `plusPtr` 4) (vkStencil (poked :: VkClearDepthStencilValue))
-- | VkClearAttachment - Structure specifying a clear attachment
--
-- = Description
--
-- No memory barriers are needed between @vkCmdClearAttachments@ and
-- preceding or subsequent draw or attachment clear commands in the same
-- subpass.
--
-- The @vkCmdClearAttachments@ command is not affected by the bound
-- pipeline state.
--
-- Attachments /can/ also be cleared at the beginning of a render pass
-- instance by setting @loadOp@ (or @stencilLoadOp@) of
-- 'Graphics.Vulkan.Core10.Pass.VkAttachmentDescription' to
-- @VK_ATTACHMENT_LOAD_OP_CLEAR@, as described for
-- 'Graphics.Vulkan.Core10.Pass.vkCreateRenderPass'.
--
-- == Valid Usage
--
-- -   If @aspectMask@ includes @VK_IMAGE_ASPECT_COLOR_BIT@, it /must/ not
--     include @VK_IMAGE_ASPECT_DEPTH_BIT@ or @VK_IMAGE_ASPECT_STENCIL_BIT@
--
-- -   @aspectMask@ /must/ not include @VK_IMAGE_ASPECT_METADATA_BIT@
--
-- -   @clearValue@ /must/ be a valid @VkClearValue@ union
--
-- == Valid Usage (Implicit)
--
-- -   @aspectMask@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlagBits'
--     values
--
-- -   @aspectMask@ /must/ not be @0@
--
-- = See Also
--
-- 'VkClearValue',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'vkCmdClearAttachments'
data VkClearAttachment = VkClearAttachment
  { -- | @aspectMask@ is a mask selecting the color, depth and\/or stencil
  -- aspects of the attachment to be cleared. @aspectMask@ /can/ include
  -- @VK_IMAGE_ASPECT_COLOR_BIT@ for color attachments,
  -- @VK_IMAGE_ASPECT_DEPTH_BIT@ for depth\/stencil attachments with a depth
  -- component, and @VK_IMAGE_ASPECT_STENCIL_BIT@ for depth\/stencil
  -- attachments with a stencil component. If the subpass’s depth\/stencil
  -- attachment is @VK_ATTACHMENT_UNUSED@, then the clear has no effect.
  vkAspectMask :: VkImageAspectFlags
  , -- | @colorAttachment@ is only meaningful if @VK_IMAGE_ASPECT_COLOR_BIT@ is
  -- set in @aspectMask@, in which case it is an index to the
  -- @pColorAttachments@ array in the
  -- 'Graphics.Vulkan.Core10.Pass.VkSubpassDescription' structure of the
  -- current subpass which selects the color attachment to clear. If
  -- @colorAttachment@ is @VK_ATTACHMENT_UNUSED@ then the clear has no
  -- effect.
  vkColorAttachment :: Word32
  , -- | @clearValue@ is the color or depth\/stencil value to clear the
  -- attachment to, as described in [Clear
  -- Values](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#clears-values)
  -- below.
  vkClearValue :: VkClearValue
  }
  deriving (Eq, Show)

instance Storable VkClearAttachment where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkClearAttachment <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 4)
                               <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkAspectMask (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 4) (vkColorAttachment (poked :: VkClearAttachment))
                *> poke (ptr `plusPtr` 8) (vkClearValue (poked :: VkClearAttachment))
-- | VkDrawIndirectCommand - Structure specifying a draw indirect command
--
-- = Description
--
-- The members of @VkDrawIndirectCommand@ have the same meaning as the
-- similarly named parameters of 'vkCmdDraw'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input)
--
-- -   If the
--     [drawIndirectFirstInstance](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-drawIndirectFirstInstance)
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- 'vkCmdDrawIndirect'
data VkDrawIndirectCommand = VkDrawIndirectCommand
  { -- | @vertexCount@ is the number of vertices to draw.
  vkVertexCount :: Word32
  , -- | @instanceCount@ is the number of instances to draw.
  vkInstanceCount :: Word32
  , -- | @firstVertex@ is the index of the first vertex to draw.
  vkFirstVertex :: Word32
  , -- | @firstInstance@ is the instance ID of the first instance to draw.
  vkFirstInstance :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDrawIndirectCommand where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkDrawIndirectCommand <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 4)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkVertexCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstVertex (poked :: VkDrawIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkFirstInstance (poked :: VkDrawIndirectCommand))
-- | VkDrawIndexedIndirectCommand - Structure specifying a draw indexed
-- indirect command
--
-- = Description
--
-- The members of @VkDrawIndexedIndirectCommand@ have the same meaning as
-- the similarly named parameters of 'vkCmdDrawIndexed'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     [{html_spec_relative}#fxvertex-input](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fxvertex-input)
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     @indexSize@ being based on the type specified by @indexType@, where
--     the index buffer, @indexType@, and @offset@ are specified via
--     @vkCmdBindIndexBuffer@
--
-- -   If the
--     [drawIndirectFirstInstance](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-drawIndirectFirstInstance)
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
--
-- 'vkCmdDrawIndexedIndirect'
data VkDrawIndexedIndirectCommand = VkDrawIndexedIndirectCommand
  { -- | @indexCount@ is the number of vertices to draw.
  vkIndexCount :: Word32
  , -- | @instanceCount@ is the number of instances to draw.
  vkInstanceCount :: Word32
  , -- | @firstIndex@ is the base index within the index buffer.
  vkFirstIndex :: Word32
  , -- | @vertexOffset@ is the value added to the vertex index before indexing
  -- into the vertex buffer.
  vkVertexOffset :: Int32
  , -- | @firstInstance@ is the instance ID of the first instance to draw.
  vkFirstInstance :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDrawIndexedIndirectCommand where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkDrawIndexedIndirectCommand <$> peek (ptr `plusPtr` 0)
                                          <*> peek (ptr `plusPtr` 4)
                                          <*> peek (ptr `plusPtr` 8)
                                          <*> peek (ptr `plusPtr` 12)
                                          <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkIndexCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkInstanceCount (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkFirstIndex (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 12) (vkVertexOffset (poked :: VkDrawIndexedIndirectCommand))
                *> poke (ptr `plusPtr` 16) (vkFirstInstance (poked :: VkDrawIndexedIndirectCommand))
-- | VkDispatchIndirectCommand - Structure specifying a dispatch indirect
-- command
--
-- = Description
--
-- The members of @VkDispatchIndirectCommand@ have the same meaning as the
-- corresponding parameters of 'vkCmdDispatch'.
--
-- == Valid Usage
--
-- -   @x@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxComputeWorkGroupCount@[0]
--
-- -   @y@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxComputeWorkGroupCount@[1]
--
-- -   @z@ /must/ be less than or equal to
--     @VkPhysicalDeviceLimits@::@maxComputeWorkGroupCount@[2]
--
-- = See Also
--
-- 'vkCmdDispatchIndirect'
data VkDispatchIndirectCommand = VkDispatchIndirectCommand
  { -- | @x@ is the number of local workgroups to dispatch in the X dimension.
  vkX :: Word32
  , -- | @y@ is the number of local workgroups to dispatch in the Y dimension.
  vkY :: Word32
  , -- | @z@ is the number of local workgroups to dispatch in the Z dimension.
  vkZ :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDispatchIndirectCommand where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkDispatchIndirectCommand <$> peek (ptr `plusPtr` 0)
                                       <*> peek (ptr `plusPtr` 4)
                                       <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkX (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 4) (vkY (poked :: VkDispatchIndirectCommand))
                *> poke (ptr `plusPtr` 8) (vkZ (poked :: VkDispatchIndirectCommand))
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
-- 'VkClearValue', 'vkCmdClearColorImage'
data VkClearColorValue
  = -- No documentation found for Nested "VkClearColorValue" "VkFloat32"
  VkFloat32 (Vector 4 CFloat)
  | -- No documentation found for Nested "VkClearColorValue" "VkInt32"
  VkInt32 (Vector 4 Int32)
  | -- No documentation found for Nested "VkClearColorValue" "VkUint32"
  VkUint32 (Vector 4 Word32)
  deriving (Eq, Show)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearColorValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek _   = error "peek @VkClearColorValue"
  poke ptr = \case
    VkFloat32 e -> poke (castPtr ptr) e
    VkInt32 e -> poke (castPtr ptr) e
    VkUint32 e -> poke (castPtr ptr) e
-- | VkClearValue - Structure specifying a clear value
--
-- = Description
--
-- This union is used where part of the API requires either color or
-- depth\/stencil clear values, depending on the attachment, and defines
-- the initial clear values in the 'VkRenderPassBeginInfo' structure.
--
-- == Valid Usage
--
-- -   @depthStencil@ /must/ be a valid @VkClearDepthStencilValue@
--     structure
--
-- = See Also
--
-- 'VkClearAttachment', 'VkClearColorValue', 'VkClearDepthStencilValue',
-- 'VkRenderPassBeginInfo'
data VkClearValue
  = -- No documentation found for Nested "VkClearValue" "VkColor"
  VkColor VkClearColorValue
  | -- No documentation found for Nested "VkClearValue" "VkDepthStencil"
  VkDepthStencil VkClearDepthStencilValue
  deriving (Eq, Show)

-- | _Note_: peek is undefined as we wouldn't know which constructor to use
instance Storable VkClearValue where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek _   = error "peek @VkClearValue"
  poke ptr = \case
    VkColor e -> poke (castPtr ptr) e
    VkDepthStencil e -> poke (castPtr ptr) e
-- | VkStencilFaceFlags - Bitmask of VkStencilFaceFlagBits
--
-- = Description
--
-- @VkStencilFaceFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkStencilFaceFlagBits'.
--
-- = See Also
--
-- 'VkStencilFaceFlagBits', 'vkCmdSetStencilCompareMask',
-- 'vkCmdSetStencilReference', 'vkCmdSetStencilWriteMask'
type VkStencilFaceFlags = VkStencilFaceFlagBits
