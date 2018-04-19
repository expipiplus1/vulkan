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
  ( castPtr
  , plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import GHC.Read
  ( expectP
  , choose
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
  ( VkImage
  , VkBuffer
  )
import Graphics.Vulkan.Core10.Pass
  ( VkFramebuffer
  , VkAccessFlags
  , VkDependencyFlagBits(..)
  , VkDependencyFlags
  , VkPipelineBindPoint(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkRenderPass
  , VkShaderStageFlagBits(..)
  , VkPipelineLayout
  , VkRect2D(..)
  , VkViewport(..)
  , VkPipeline
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Query
  ( VkQueryResultFlagBits(..)
  , VkQueryResultFlags
  , VkQueryPool
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkPipelineStageFlags
  , VkCommandBuffer
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
-- #_see_also#
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
-- #_see_also#
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
-- #_see_also#
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
-- #_parameters#
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
-- #_description#
--
-- Once bound, a pipeline binding affects subsequent graphics or compute
-- commands in the command buffer until a different pipeline is bound to
-- the bind point. The pipeline bound to @VK_PIPELINE_BIND_POINT_COMPUTE@
-- controls the behavior of 'vkCmdDispatch' and 'vkCmdDispatchIndirect'.
-- The pipeline bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ controls the
-- behavior of all <{html_spec_relative}#drawing drawing commands>. No
-- other commands are affected by the pipeline state.
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
-- -   If the
--     <{html_spec_relative}#features-features-variableMultisampleRate variable multisample rate>
--     feature is not supported, @pipeline@ is a graphics pipeline, the
--     current subpass has no attachments, and this is not the first call
--     to this function with a graphics pipeline after transitioning to the
--     current subpass, then the sample count specified by this pipeline
--     /must/ match that set in the previous pipeline
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipeline',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint'
foreign import ccall "vkCmdBindPipeline" vkCmdBindPipeline :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()
-- | vkCmdSetViewport - Set the viewport on a command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   If the
--     <{html_spec_relative}#features-features-multiViewport multiple viewports>
--     feature is not enabled, @firstViewport@ /must/ be @0@
--
-- -   If the
--     <{html_spec_relative}#features-features-multiViewport multiple viewports>
--     feature is not enabled, @viewportCount@ /must/ be @1@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pViewports@ /must/ be a valid pointer to an array of
--     @viewportCount@ @VkViewport@ structures
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkViewport'
foreign import ccall "vkCmdSetViewport" vkCmdSetViewport :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()
-- | vkCmdSetScissor - Set the dynamic scissor rectangles on a command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   If the
--     <{html_spec_relative}#features-features-multiViewport multiple viewports>
--     feature is not enabled, @firstScissor@ /must/ be @0@
--
-- -   If the
--     <{html_spec_relative}#features-features-multiViewport multiple viewports>
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D'
foreign import ccall "vkCmdSetScissor" vkCmdSetScissor :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()
-- | vkCmdSetLineWidth - Set the dynamic line width state
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @lineWidth@ is the width of rasterized line segments.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_LINE_WIDTH@ dynamic state enabled
--
-- -   If the <{html_spec_relative}#features-features-wideLines wide lines>
--     feature is not enabled, @lineWidth@ /must/ be @1.0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdSetLineWidth" vkCmdSetLineWidth :: ("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()
-- | vkCmdSetDepthBias - Set the depth bias dynamic state
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- <<data:image/png;base64,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 $$m = \\sqrt{ \\left({{\\partial z_f} \\over {\\partial x_f}}\\right)^2 + \\left({{\\partial z_f} \\over {\\partial y_f}}\\right)^2}$$>>
--
-- where (xf, yf, zf) is a point on the triangle. m /may/ be approximated
-- as
--
-- <<data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAjwAAAB+CAYAAAAgAMvUAAAABmJLR0QA/wD/AP+gvaeTAAAgAElEQVR4nO2dd9gcVdXAf28q6RAgkAKhBCWRAAICIlKiUYxYQUFAEaQpikHg+/wQJAiGJkUpxkJTKaKGKipFUVCQQAgoVRICiRATCBCSkP5+f5wZ9+6du7Mzu9P3/J5nn8zcvXfu3ex5d87c00BRlKpwOPA74MfA5iH9fgs85L1GtTjXjUC38ZrS4nWUfCnD91iGNSrxiPqdHkLtt8p/HdXqpL1aHagoSqH4KnCpcb4/sDvwkqPv7sDQLBalKIrSBiOA3ay237d6sR7trUVRlAKwMXCu1TYcuAHo6eg/yDhekdaiFEVRioQqPIpSfo4ABjja9wC+abUNA3ob58vSWpSiKEqRUIVHUcrPwSHvnQHsZJxvbxy/BbydyooURVEKhvrwKEq5GQrsEPJ+b+DnwC6IcvMR473/pLiupOiPfL5NEQVtHvAc4uioKGmgMldRVOFRlHLzXprv1I4Dfg1cBxxjtM9Pa1EJMAo4CzgI6Ge99yJwNXABsDzjdSnVRWWu4qhJS1HKzbiI/SYhCs9Ao+3p5JeTCO8BZgFfJHjjARiNhLE+CozPbFVKlVGZ6wBU4VGUcvNO43gNYrL6MtG2359KZUXt0QOJLtvQ8d4TSI6hO5HPui1wD7BFVotTKonKXIegJi1FKTcjjeNbqeWoGAuc0GTsA6msqD02Aba22l4FjgWmG237AHchUWe3I47ZqzNYn1I9VOY6BN3hUZRys7FxfI9xfCrid9CIxcjTa9FYAMwwzu9BHEinW/3uAy73jrcDvpL6ypSqojLXIajCoyjlZiPj+BnjeBnhP8g3AetSWVF7dAN7AZ/2/p0IvNyg7y+M428j0TWKEheVuQ5BFR5FKTfmD+4C6707Ef8Dm27gR6mtqH1WADcD9zfpN4uaSWEoUk4jCYZRX+cnqdc7Elqfkjx5ypzKW0aowqMo5aaPcbzU8f4JwF+tth8iP9xlZy31T+JhCRjjsHNC1zFZAvwrhesq2ZKGzKm8ZYQ6LStKuXmaWhitK2vySmA/JOPy7oh/wtnZLC0TTCVvr4Suad6AbiCe6W9j4EOO9rPRxHVVIWmZU3nLCFV4FKXcvDdCn6XAKWkvJCfM37ANkerKjfwvouLfgOYDh8QYNxT4k6N9CpKwTqkGScucyltGqElLUZQyM9A63y6Ba/o3oJkxxgxBQpa3t9rPA85MYE1KcUha5lTeMkIVHkVRyswg63wjZ6/obAxs5h0/GnHMQOB3BH0xfkCwWr1SfpKUOZW3DFGTlqIoRWMD4FOIz9FIoAt4CfgjcAuwyuvXBQywxg5uc27zJhLlBtQPuIOgafEnwOQ216JkR14yp/KWIarwKIpSFPoiT6j/5x3bHAvMRiJjHkF8J3pafWxzQ1weALb0jv/dpG9fJLv13lb7z5C1qtNo8clb5lTeMkQVHkVRisAQZJvefHJdgjhf3oqk+t8ZmArcC+yBOG3auCLV4rAUd3i/TW+kAv1Eq/0m4Ej05lMGiiBzKm8ZogqPoih50we4G6lY7fMq8EHgcaPtDuAvSBmAq4ErHNd6M6U1mvREwoftpHO3AociuVqUYlMmmVN5Swh1WlYUJW+mUn/jAfgS9TcenyXAaV5/V+mMJckuLUAPxIRwgNX+e+CzSEXtNOjpXf83wFxgOVI+ZA4SrTMZMbco0SiLzOUlb6AypyiKAsCN1Kexn9LidbZBnlDNa93RZMx6SEJFVzr9PVtcRxS6gCsdc97rrSktRiMOrf58rwB/RnYdVhvtX2jh2kl9j2mS9BrLInN5yRukK3MQ/Ts9meDnb9S3KbrDoyhKnkwm+Dt0YZMxK2icNv+ptlfUmMsQfwmTvwIf99aUBkMRk8pOyI3maOSpem9kx2E3ak/5UcOaO52yyFwe8gYVljlVeBRFyZMDrfMFwH0Rxr3maJsHLG53QQ24kKA5YwYwCdnmT4vzgM294/OBn1LvoDoTKS+yHHgmxXVUiTLIXF7yBhWWOXVaVpTqcTww1mpbBvxvDmsJY1ukUrTJ3USLOHE9rLn8L5Lgu8A3rLZZwIdJ139jQ2omg26k6KuLjyLmD3VebU4ZZC4veYOKy5wqPIpSPb6K/LDbfIf0nw7jYCtlED29viv3yZ/bWEsjTgdOtdqeQsKDX09hPpOJSDQRwJM0ztMyL+V1VImiy1ye8gYVlzk1aSlKtehBLZGZzdZZLiQCGzranos4dgPrvBv4ZXvLCXASoiSa/Av4ABLCnDbvM47/nsF8nUCRZS5veYOKy5wqPIpSLTbDnTEWiqfw9He0vRJh3HrU6g/5PEiyT53HA9+z2uYCExCfjyyIW3ZAaU5RZa4I8gYVlzlVeBSlWoQpNWMyW0U0XFv0UbLOjiP423Vl+8v5L0cBl1pt85Gbz/wI43dDHGM/EXPefkj0ix9+a2YAvoJgeO7nY15fKabM5SVv0GEypz48ilItwpSaoik8cxxtKyOMs9PrPwtc2/5yADgM+BHikOmzADErvBDxGpci4btPINlwo7IN8lQPciPyk7qtazD3wzGurQhFk7k85Q06TOZU4VGUalEmhedh5OnadAaNUojxk9b5qSQTLXIgcA31T/J+uYGofh69ge2940dizv8Ete/oCOAq7/hxJCeK0j5Fkrm85Q06TObUpKUo1SJMqSmaD89q4DqrrdkadwV2N87vBqYnsJaPA9dTXwn7deTJ/skY13kXNR+qdnwgKu1LkSNFkbmiyRt0gMypwqMo1SJM4dmMWshpUTgHSWDmMyGkbxdwrnH+MnB4Amv4MFJ1urfRtgTYD8l/EoekbhqVv/nkSN4yV0R5S/pahURNWopSLbYKea+H936RsqO+CBwL/Nw7PwK5IS109D0F2Nc7XoxknY0SYRPG3sDNBCPb5iCZbl3FIsPwzQBraD0pXU9gB+O8kjefHMlT5ooob6AypyhKyRiOu7ih+do/obmSLuh4HLVokRnUmxkGIynu/bmeQ6JmkuAOmv+ftfKK+6RuMt64zmoapxlIgk4sHuqTh8wVUd4gW5mDnIqH6g6PolSHKE7JRfPj8ZmGPKFehPhLPIf4MqwB3onkT3kLiUiZSnIZo3du3qUlkjJn/ZNoUURKfPKQuSLKG3SIzKnCoyjVIYrCU7RILZMHkTwg45BIlc2Rm87tSPr/u4C3E55zeMLXS4LK+1IUiKxlrojyBh0ic6rwKEp1cO3erKU+EqTICo/PU96rU+mIm0/BUJmrUVmZ0ygtRakOtjKzCsmzEdZHKRbqPKpkTcfInCo8ilIdbGVmLlJ80GQ09Ts+SrEYS63e0xqCCquiJE3HyJwqPIpSHWyT1myCqfR7I0qPUkxM08KTwIq8FqJ0DB0jc6rwKEo12AhY32qbgyg9NmrWKi4d4UuhFIqOkTlVeBSlGrgcll07PKAKT5HpmJuPUhg6RuZU4VGUauBSYmbj3uEpai6eTqcHHeI8qhSGjpI5VXgUpRq4FJ45wDwkWqtZXyV/xgIDvONKO48qhaGjZE7z8ChKNWik8KxDagdt06Svkg+HAad5xwOM9i7gMeN8IbBXVotSKk3HypwqPIpSDWwlZgG1itCzqVd4tkJ+3LozWJcSzgSkjIFNT6vd5YulKK3QsTKnJi1FqQaukHTXMcB6wMh0l6NE5EhE+Wz2mpTXApXK0bEypwqPopSfwcDGVtucBsc+atZSFKWjUIVHUcpPowgt13HYGEVRlMqSlg9PH2APYDzy9LkEyeD4INEqz24CvA/YEskMuwgpWf8w6neQNUOAfZAbZB/gVWAWEr64LsL4bZBqxCOA1Ygj3EMESx60w4bAdt4ahwD9gDeB15GCgE8gRTSrSiOHZddx2BhFUZSOZRiiYIS9bjX6DwTORG6Krr6vAafQeGfp3cBtSHica/xLwKeT+nAdxo40/y6nGf0HAxcDyxr0nQscS+Pvcjfg/pC5ngA+0uJn6QL2Bi4BnonwuZYCvwR2jzHHURGua752scZPiTjuNNrnVMd19zDe7+94/1dtznmjdb0pbV5PyYcyfI9lWKMSj6jf6ckEf7sa9W1KM5OW/SPuwk9UtAvwOPBt5InbxVDgfOCnVnsX8iEeAT5G4+KGmwG/AT4fYV1K62yB7KZNplZUzmY0oiBNB/pa7x0P/BXYM2SO8cCdwP/GXNsk4HngPuDrBKMNVhKsBTMA+Cyyw3gN9aGYVcCVSNDc1VmORG2Z6A6PoigdRTOT1ubU2//7A8OtPo8CHwRuoXYjeQt4A6nv089x3SOQnaFbEeXmWuBQ771uZIdoNbLD5FrjZcDNyJO7Eo2VBH05tiCoXG4A3E3thuibodbDrch+Avk+jvbOP++d+ywDFnvXHegYfy5irvxthM8AsCsSVm0yHVGiH0LMWCCyugOyI3gMsmMFcDhi/trbW1sjngeu8477AJ9x9LkLMbeC7F6aPGGMHwB80jtegSjtPv8IWUNUbOVlGUEFZw6wqXGu2ZYVRSk6LheWlt1amik806g3cxwN/Ngx+c3ILs1ZiPLi31h7Iv4fP6Q+DwiIOeRW4AeIsvMC8B1EcXrD6zMIuUFdgNxwfQYjN7KfNVl/u3yN+rTbWXIf8IsEr/c0wRvjfILhyZd7/R5DzC13ISZGgFHAV5BdGXN38ChE4ViEfNcgcnAxsusHIh87A1OBidacPwB+RzSfIJO1iOz80vHecmRH50Hv+rciJlO8dVyN7Po04j7v5fN94ASrzzpEwXP9AU73XgBXGe2nUK8QJoH9vbp8dmZTb+YahPjK/SfhtSiKoiTFmohtqTCNelvaEuRJeA5BhcZkDEG/nDeAL3jH06lXaGxOJGjHu6iNzxGVOxzzZvUyFc20mG/N+Zj3722EK8NfI7jenwG/9o6PCxnbC9lBssdPiLjmKcaYqRHHgIRtv2bN+b4Y4/siztr2uic3GfcZo+9tMeaLSj9E8TLXdIuj3xkE176Ho19U1K+iGpTheyzDGpV4RP1Ojyf4uxXXDeK/xA1L39k6H4SYKvYlPOrmeWCG1TYEuAL4I3JTsP0uTK5ztNmmNaV9dkSe+A8lXIu+nKB57ABk1+16wpW1Ncguh83Hoi8TkJt8HKV3EbLTY3JyjPErgYOpZS/2OZfGu4CbU9sRfRlJ+JU0Y5DdMxNXGLpGaimKUjYS3eGJo/D0RhxNbY5HavU04yVHWzdyc20WMrwQueGYZLat1WGcj/hghbGO4G5Ff+TGe0aEOWYhUV4mUU2Hv0d2/I5CfL3icI91PoHGDvIuniG4o9MXUfJsX7WeiElyfWqmr7jrjUIzh2UfzcWjKErZWB2xLRJx8vBsRzAaZyZu/wkXfRxtFxN0rmyEvdY3I45rhzPJxrTkIooSmQY3Reznqqr7GLKbF4XHEadpn6g334e8Vyu8bJ0PRpT4WTGu8RPgQ8CBRts44ELEv8nnW8D7vePzkJ3MNGiWdNAn7x2eKURThpXkOJPONP9MQWUtb5KSvUR3eOIoPLY5C8T5MqrH9DDrfC1i0orCUIJP4kkmrmuEbYarOi8ifj1RcPX7W4y57B2/ITHGtorryWA48RQeEOf9XRGTlc+Xkd2n25BEi6d77X9HUjWkRbOkgz4LkOgtMyRfI7UURSkyrt/sXBSeFYiTalS2tc7vJfrujj0W4NkYcyvReCZGX1dIdzvjXSHrUeiF7NK8G7mBD0F8y1ymKldOoUY5o8J4AzHF3mfNcyWSe+g6b11LgM+RrvnVVnjWETQX+syh3iytJi1FUYpMYXZ4HqK5r4fPKGSXxuSuGHO7fIceizFeiYadSyYMl+YdZ7ztkxXXgX5zxJfnEIK7h3EIiw4M4wEkDcMUo20jxMzrK1bHIekW0sTepZkPrGrQ11Z4hiJBB6+7uyuKouRKLk7LvYHtrba/xJjHZQ5rZ/y/0fwhaRCWiC+L8VE5AamRNZn2lJ12ORspn2HiKzvXAjekPH8fJPu4ict/J+w93eVRFKWo5GLScjksPxxjHlthWUUtIV0r4x919lLaJW7iv6THR2Eq8H9W21okr8NNiD/OItxFakcB8xJci5/48HFkp8Tk9gTnacSWBE13Lv8dH5fCszXZ+KpNoTMdaJXsmYLKWlXIxaTl2qGJo3TY4/9B4213mz6IwtXq3O1wErXsvFlzL5INWKkxiaCy8zpShPTv2S8HEAXqEiQqweSHSD2xqH5qrRA1Qssn70gtRVGUOBRC4XmZeD/k7ezQjCcY0p6VwrMv8NGM5rJZiio8Nt9ztB1JfsoOwAgk87TNxsDPkRD2lmu/NCFqhJaPmrQURSkTiZq0ovrwtKOwjERq9rQ63rW7NDPGeKUajAfGWm2zcZdRyIoeiFKzkXf+ivX+B4H/SXF+V1h52A7PXIJJPlXhURSlqLiUm5YTD0ZReHoRdFhuV2FpZ/zLBG8sSvV5j6PNdhjOmm9SqwF2pXdsl544C8nZkwZxTVqrCeZP0lw8iqIUlcydll0Oy4/EmMPlsPyPNsZn6bD8LcQ/Iw+iJgDsFOxdQojvHxM39D2M3aj57TyDRI4t9/79qdGvNxKttSPR0zhExVZ43qB5iPlsYLRxvimSA2lpgutSFEVJgsx9ePJ2WLZz8GSp8MSJJFPSxeUHEyePFARzQbXKYKR+Vi8kn9BB1HZ2rgQmem0+WyElSg5NaH6Q6KwtrLaw3R2fOQQr02+NyrqiKMUj8zw8eTosb0fQYVn9dzoTV96luOaYnZJYCBKBtZV3fBLBumLHEsx2fAhweELzg+zS9LbawhyWfdRxWVGUspC5SasdhWUEsmXe6vh2d5eU6vCgo20CUkYiqqnosATWcTiivADcClzu6PMmUlLifur/xi5D6o0lUQcursOyj0spUj8eJS5fJig33yTdMipK5xFnh6cHUsD544gV6QakzM9/aabwtOuw7HqibkfhWUCw4rXSGTyDZFceZ7QNQQpznhJh/MFImoF22AZRWkB8rI4M6fsQUrH5u0bbQOSPcA+im3UbETck3Ud3eJQk+BzwfuN8OXByTmtRqkucHZ7LEEXc56NIctaz/YZmJq13Eaw11I7CUiaHZaV4nO5oOxl5suwKGXcwcFWbc/vOxwOpZVhe3GTMucAfrbadgXPaXAvEj9AK66MKjxKXAdb5S7msQqk6UXd4RiC1C22+DfTzT+wdno8BBxjnmzsu8CXgQO94FXCM8d4xyNOrz57W2FXAj43zGdSbBK6gvqK1vbu0FXCNcX4bMN2xRsXNNOoVWLscwr7U///eQi3PzQbAxcZ7rirjk6nJBkh697nGtU0flh0d46+xzo+lvsjodOQz2IJ9DvLEeTVS8mQxYuraAVFM9vH6XQ0cYY09gno5vYpanbfvI7tIIH8LvgK+ENndORJ5Ajnauqb5WV07OScCw433/oTU3opDqzs8fiSX+d2rwqPEZYR1rg+jShpEzcOzFe6H3t7Ib/ezrovfgETDRH3ZQv5YzPFmOv6RMcd2Izc5JTpLiff/O8UYOyrm2G5gF2P8cS2MH+j4DD2BC5C6XVGv8zbw9Yif4ShjrgUR+q9wrDHuZ53muEYznrSusYpgXa1GzLDGriOYeqIZN1rXmBJzvFIMWvke1ycow8emtD5QWasiUb/TDQnKmusBbRiiHNl9l2IEPtkmLZeTcBhmPp6+iAms1fFx57bHK53BWsRnZ0+kQKedOdhkCZIJeSyyW1MVuhDbtMmLhP9fmNhmrS5qUWeK0gzbF24NutOupENUk9ZCgq4C3cA3MHbZbZPWO9pY2EqCIeRxuI1wPwylfVw7JlGZT3vfzzRa28loxN8Qb/xBwHsRBWADZLtzEfAC4jhsbn/G/Qx2hGFUkv6sNiMx7NIeUfx3fBoVEX265RUpnYQd7fhr5G9OUZImTpTW6YiVaX/kd/9GxF3gv8RN3KYoReMt4K68F5ExrTosh/VVPx4lCtsBnzTOVwPfyWktSvWJm4dnOiG7jarwKEr5aNVhOaxv0XLx9EeczjdFlNp5wHPINrWSD72RTOKmK8RZlH9nUGWtuGSeeFBRlGLRatLBsL5F2eEZhdxEDyJotnsRibS7gGCRViVdeiBRlGYh3NuAqbmsJhlU1oqPH1RhKtmplpZQFKVYtLvDM5/6cP9G18ya9wCzgC8SvAGBlNOYgkSH2jX2lPToi6SnOMRo+wOS3yqqo3zRUFkrD7aCowqPonQQ7So865AnWJPR5Lvj2wNJi+HK7/QEkr/rTuTHblvgHoLFU5V0WImYeHymIY6hb+eznLZRWSsXtoLjMnNFQk1ailI+bJPWQiTfRBxmUx+V2QtReuKYxpJkE4Kf61Ukv4vphLgP4qQ+DElLsBNt/AAqkTkZkbGZiCmrzKislYvTqM8Tpt+BonQImxBMrvW3Fq5zqeM6H44xPulkcF1Ilmz/encTzObrc7HR7+ttztvplCGpn8pa9chF7tSkpSjlwuWwHMecFTYmTz+ebmAv4NPevxNpXCj4F8bxt6kvR6MozVBZ61BU4VGUctFuDp6wMXk7Lq8Abgbub9JvFrVt7aGIP0m7TCJeOZCPWeOPiTjucpQikKes+fiFiKPK3L9DrnV+xGu8O8H1lw714VGUcuFSSg4B9o55ncGOtqLl4mnEWuSJfLR3fjBwU5vX3KV5lzrsOoJRx2uRzXKRhqz57ES8TYcw2YkifyuBf8aYr3KowqMo5cKl8Ixp0J7EtYuK6aS9VwLXew24zjt+BxK2bLIMCc0Gyctim0DmGePHA9t7x08jjr4+f05grUq2JC1rPr2oycx2SPJDkweoj6a8PeRaT1GTyaHAR7zjRdQy0c9DHX4VRSkRprNl0q8VRH/izNvZ9Rlr/kZOp60wAlFq7P+f90YYuz3wutf/aWCjBNeVBnl/j1HIe41pyprPwQTl7bQWr3WlcY1DE1ld8qjTsqIoTUnT7NQXyT5bBuxCuNsleO2XcfvanN1k3JbA74H1kafpDyHhzkq5SVPWfB5xtNm7jFHYHTjCO/4ztR0kBVV4FKVMrI9sV6dJWfx4BlnnSe+knAsssdomAPs26L8JEt48HFFyJiJKj1J+0pY1gOeRnUGTuH5lPYErkLD7NcDxCayrUqgPj6KUh20ymGMM8KcM5nGxAfAp5Cl1JPLD/RLwR8R/ZpXXrwsYYI11OWG3w2vAhcCZVvvZwPustiHIzs7WSPHJ/YBnE16PkixFkjWfmcAHjPMRiAL9SsTxX6EWhfV94MnklqYoipItnyM9/x3/dV7EtSRpg+8LnIH4EDVa1/PUnnhHOt4/uY35GzEIcfq055pk9FkPMR34PlD7pLCONMnbPyYKnSBrILuK9lx2+oNGbAK8QS183d6VKhrqw6MoSihZRFFlHak1BNlRmkItffwS4HTEAXgE8qO/HLgXeBewleM6adR1egs4x9F+FvLk3xP54d4LCV8+CLgvhXUoyVBkWQN32HlUP54LkM8HcBIiu4qiKKXlGtLf4ZkVcS1JPKH1IRh1tohgeC6IGeFZr/8XHes+rIX5o7Ae4otjz3cAcJV3vM5bUxnplB2eMsjalo657oww7v1G/3tTWlvSlEHuFEXJkQdIX+GJ+mSYxA/W9xzzfzyk/2e8Pq7Q/LBx7XK0Y75lxvFJKc6dNmW48XSSrL1qzbWwSf9eSIX3bsTvaGyKa0sSNWkpihJKFuamgcCmGcyzDXCi1fZbwitx3478qLu2+RcntC4XVwP/str8mkpTEefmrOgJfBb4DTAXMb8sQ2qj3QVMJp08MWWmTLJmm7U2BjYP6X8CkugSpNDp02ksyqP0sqdRWopSDgYijokmi3H7mMThswR/1McAC9q8bjMmE3zgaqY4rEAUj3c53nsqiUU1YA3i6Hq91f42UsMoK0YD05GSBCDf0QxE+doRMYlMROTiZxmuq+iUSdYeQfI3mbwHiSCzGU5tZ2Q+8J30lqWypyhKduxIcGv9gQSu+13HdQ+PMK7dLen/WONfQRyBm+FHRJkv180gaQYgDq723GneZEyGImUGfNPFUdT/f+2ElA3oxn2TbkQnmLTKJGufcszZ6KHmBqPPZ1JcUxqypyYtRVEa4koI+EIC13VdI23T2bbAMKvtbuSHrxmu36zH215ROL2QH2hXqO+JiNkhbc6jZto4H/gp9f9fMxFzxnKkFIIilE3WXBmXXQkI90XKUQDcA/wqtRVVSPZU4VGUcuBSQsqq8LgcK2c62lzYaf4h3YKcXYgPz/7euZ0EbiDwrRTnB9gQ+IJ33A38sEG/jyL/t2tTXk+ZKJOsgUQE2o7KO1vnvamVPlkFfDXF9VRK9tSHR1HKQZUUng0dbc9FHLuBdd4N/LK95YRyCbUw5LMRZ9a/W32OAy4iPXPHRCSsGiR77r8b9NNSFkHKJGs+j1Krdu6vYwySEBHgG9QUuQtJN6t33rK3A8GivY/g3glriu7wKEo5SEvheYngU1na9bT6O9qipM9fD9jManuQ9H5sz0CiYEBqFJ2OhCnfYvXrS7o+CGYpC1vZUsIpi6yZhJm1RiFyCPK326ygbbvkLXsTkV0l87V/6IgQVOFRlHLgUkLmJHDdNQR/xDcg3SKldpFEgKURxo0j+Jt1ZfvLcXI8NSXmeurNBt9Ckg2afAHxF0kD06ThysarNKYMsmYTpvBcQq2214mI30yaVEr2VOFRlOLTF6npY7KaxtvLccnarOVS1FZGGDfROn8WuLb95QT4HHCpd3wnErVmOmk+BfzcGtOT5J62+yGKqB/BYm7pX0EwiufzCc1bRYouay5mONp2QcLVD/DO/4CEiSdNpWVPfXgUpfhsTfDhxGWKapUXkKgPkzGI+SYNHkaesk2nUJeDqM0nrfNTSd5J8iPIja0LuB84ELkB2JyBKEZ9jLYDkBtTS/4FBtsgid1AbkB+Mrd1uJXTtL6nKlBkWWvEK95ruNG2E3CZd7wS+FpKc1da9nSHR1GKT1r+O2HXStOPZzVwXcz5dgV2N87vJvkn3D2AX8iijFYAAAQJSURBVCNRMI8hhSQbFYp8EfiRo31qAut4AvnOxwCnGe2PG+3mK02n1bJTVFlrhq00D0KUEZBCoXbm76SotOypwqMoxScPhSftSK1zqPc/mBDStws41zh/mWjJEeMwHik30B+J4tkPeLPJmLMJ+oNMJLhb1g6V8qHIiaLJWhQa7RLOJRmlOgqVkz01aSlK8UnLYTnsWmkrPC8Cx1LzhTkCuTG5iiWeQk2JWAxMIlqkTRgfAg4xzvcD1veO51ErGXEd8oRvcgW16J/FBE0kPwb+6h13A1+h8U5RMyp308mBvGWtFRp915NpXZbiorKnKErm/IGgs+BBCV5/U8f1/9NkTFKp4Y+j5iQ5g3rlbjCiePhzPIdEzyTBTwh+ZtfLdl4dGXGc/2rHsbwnsjPhX8tVyLJdOqG0hE9estYKwwjK0m8znD9t2Yv6nZ5M8P+hUd+m6A6PohSftE1aC5Cnxn5G2zBk5yJKCG87TEP8Ay5C/CaeQxKcrQHeieykvIVETU1FqjMngZ29thH2k23UcT7tODCPo/adrEH8K5TWyUvWWmEhUhB0lHe+glpOqCyopOypwqMoxaYXtTo2JkkqPCC+AXYa/jHArITncfEgEv46Dvgg8nn7I1mNZwJ3kfw2/k7Nuzi5jWiFJ5PAVK7+SbRwaiWcPGStVf5JTeE5D5id4dyVlD1VeBSl2GxJ8O90KbAo4XleID+Fx+cp76UI6kORHkWXta2p+RK9QL0jdRZUUvY0SktRik1aVdJt8nBcVsKp5E1HicQlSMJREFPWioznr6TsqcKjKMUmbf+dsGumXVNLaUxPpHCiT2VuOkpTJlGrF3U7cEfG81dW9lThUZRik6fCozs8+TGWWuh7ZZxGlab0QXZ3QHyJvp7DGiore+rDoyjFRhWezsQ0KTxJ9iYNJXm2BbZA8vs0KsnwP9QyKp9DOn/rzais7OkOj6IUmzwVnpHAeinMpTSnkj4UHc65wO+Q0hAuxlMr5/AsteSXWVNZ2VOFR1GKSw/kidAmySzLPm8iT54mXcBWKcylNKeyN50Oxv9ORxNMbTAM+A3iqLwW+BL5hYJXVvZU4VGU4rIZtUgNk7kpzadmrWLQg4o6jXYww6jl1BmNZPreE8kH9WXkO/ZNWadSK02SNZWWPfXhUZTi4lI2FpFe9uMXCGYSVoUne8YCA7zjSjmNdjD239WXvJfNReRnyoKKy54qPIpSXLLy3wm7tio82XAYNf+NAUZ7F/CYcb4Q2CurRSmJMQxx/m3kE/cisrNzfWYrqtExsqcKj6IUl6ySDoZdu5HC8xD1vx9FzlpbBiYg9ZxselrtSftvleF7LMMam3EtUjBzV8QvbmMkBP1VJJv5DKQwZh7kIXu5fKf/D+Woq8HLbv+YAAAAAElFTkSuQmCC $$m = \\max\\left( \\left| { {\\partial z_f} \\over {\\partial x_f} } \\right|, \\left| { {\\partial z_f} \\over {\\partial y_f} } \\right| \\right).$$>>
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
-- []
--     r = 2e-n
--
-- If no depth buffer is present, r is undefined.
--
-- The bias value o for a polygon is
--
-- <<data:image/png;base64,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 $$o = \\begin{cases} m \\times depthBiasSlopeFactor + r \\times depthBiasConstantFactor & depthBiasClamp = 0\\ or\\ NaN \\\\ \\min(m \\times depthBiasSlopeFactor + r \\times depthBiasConstantFactor, depthBiasClamp) & depthBiasClamp > 0 \\\\ \\max(m \\times depthBiasSlopeFactor + r \\times depthBiasConstantFactor, depthBiasClamp) & depthBiasClamp \< 0 \\\\ \\end{cases}$$>>
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
--     @VK_DYNAMIC_STATE_DEPTH_BIAS@ dynamic state enabled
--
-- -   If the
--     <{html_spec_relative}#features-features-depthBiasClamp depth bias clamping>
--     feature is not enabled, @depthBiasClamp@ /must/ be @0.0@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdSetDepthBias" vkCmdSetDepthBias :: ("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()
-- | vkCmdSetBlendConstants - Set the values of blend constants
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @blendConstants@ is an array of four values specifying the R, G, B,
--     and A components of the blend constant color used in blending,
--     depending on the
--     <{html_spec_relative}#framebuffer-blendfactors blend factor>.
--
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdSetBlendConstants" vkCmdSetBlendConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()
-- | vkCmdSetDepthBounds - Set the depth bounds test values for a command
-- buffer
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @minDepthBounds@ is the lower bound of the range of depth values
--     used in the depth bounds test.
--
-- -   @maxDepthBounds@ is the upper bound of the range.
--
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   The bound graphics pipeline /must/ have been created with the
--     @VK_DYNAMIC_STATE_DEPTH_BOUNDS@ dynamic state enabled
--
-- -   @minDepthBounds@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- -   @maxDepthBounds@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdSetDepthBounds" vkCmdSetDepthBounds :: ("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()
-- | vkCmdSetStencilCompareMask - Set the stencil compare mask dynamic state
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @faceMask@ is a bitmask of 'VkStencilFaceFlagBits' specifying the
--     set of stencil state for which to update the compare mask.
--
-- -   @compareMask@ is the new value to use as the stencil compare mask.
--
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkStencilFaceFlags'
foreign import ccall "vkCmdSetStencilCompareMask" vkCmdSetStencilCompareMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()
-- | vkCmdSetStencilWriteMask - Set the stencil write mask dynamic state
--
-- = Parameters
-- #_parameters#
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
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkStencilFaceFlags'
foreign import ccall "vkCmdSetStencilWriteMask" vkCmdSetStencilWriteMask :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()
-- | vkCmdSetStencilReference - Set the stencil reference dynamic state
--
-- = Parameters
-- #_parameters#
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
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkStencilFaceFlags'
foreign import ccall "vkCmdSetStencilReference" vkCmdSetStencilReference :: ("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()
-- | vkCmdBindDescriptorSets - Binds descriptor sets to a command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- <{html_spec_relative}#descriptorsets-compatibility Pipeline Layout Compatibility>.
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
-- or compute commands, as defined in the
-- <{html_spec_relative}#descriptorsets-compatibility Pipeline Layout Compatibility>
-- section.
--
-- The descriptor set contents bound by a call to @vkCmdBindDescriptorSets@
-- /may/ be consumed at the following times:
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSet',
-- 'Graphics.Vulkan.Core10.Pass.VkPipelineBindPoint',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout'
foreign import ccall "vkCmdBindDescriptorSets" vkCmdBindDescriptorSets :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()
-- | vkCmdBindIndexBuffer - Bind an index buffer to a command buffer
--
-- = Parameters
-- #_parameters#
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
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@,
-- 'VkIndexType'
foreign import ccall "vkCmdBindIndexBuffer" vkCmdBindIndexBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()
-- | vkCmdBindVertexBuffers - Bind vertex buffers to a command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall "vkCmdBindVertexBuffers" vkCmdBindVertexBuffers :: ("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()
-- | vkCmdDraw - Draw primitives
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
--     <{html_spec_relative}#renderpass-compatibility compatible> with the
--     @renderPass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
--     <{html_spec_relative}#fxvertex-input {html_spec_relative}#fxvertex-input>
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
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdDraw" vkCmdDraw :: ("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()
-- | vkCmdDrawIndexed - Issue an indexed draw into a command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
--     <{html_spec_relative}#renderpass-compatibility compatible> with the
--     @renderPass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
--     <{html_spec_relative}#fxvertex-input {html_spec_relative}#fxvertex-input>
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
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdDrawIndexed" vkCmdDrawIndexed :: ("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()
-- | vkCmdDrawIndirect - Issue an indirect draw into a command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   If the
--     <{html_spec_relative}#features-features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   If the
--     <{html_spec_relative}#features-features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     @VkDrawIndirectCommand@ structures accessed by this command /must/
--     be @0@
--
-- -   The current render pass /must/ be
--     <{html_spec_relative}#renderpass-compatibility compatible> with the
--     @renderPass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall "vkCmdDrawIndirect" vkCmdDrawIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
-- | vkCmdDrawIndexedIndirect - Perform an indexed indirect draw
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   If the
--     <{html_spec_relative}#features-features-multiDrawIndirect multi-draw indirect>
--     feature is not enabled, @drawCount@ /must/ be @0@ or @1@
--
-- -   If the
--     <{html_spec_relative}#features-features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, all the @firstInstance@ members of the
--     @VkDrawIndexedIndirectCommand@ structures accessed by this command
--     /must/ be @0@
--
-- -   The current render pass /must/ be
--     <{html_spec_relative}#renderpass-compatibility compatible> with the
--     @renderPass@ member of the @VkGraphicsPipelineCreateInfo@ structure
--     specified when creating the @VkPipeline@ bound to
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
--
-- -   For each push constant that is statically used by the @VkPipeline@
--     bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@, a push constant value
--     /must/ have been set for @VK_PIPELINE_BIND_POINT_GRAPHICS@, with a
--     @VkPipelineLayout@ that is compatible for push constants, with the
--     @VkPipelineLayout@ used to create the current @VkPipeline@, as
--     described in
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_GRAPHICS@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall "vkCmdDrawIndexedIndirect" vkCmdDrawIndexedIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()
-- | vkCmdDispatch - Dispatch compute work items
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Compute         | Compute         |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdDispatch" vkCmdDispatch :: ("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()
-- | vkCmdDispatchIndirect - Dispatch compute work items using indirect
-- parameters
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command will be
--     recorded.
--
-- -   @buffer@ is the buffer containing dispatch parameters.
--
-- -   @offset@ is the byte offset into @buffer@ where parameters begin.
--
-- = Description
-- #_description#
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
--     <{html_spec_relative}#descriptorsets-compatibility {html_spec_relative}#descriptorsets-compatibility>
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
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
--     feature is not enabled, and any shader stage in the @VkPipeline@
--     object bound to @VK_PIPELINE_BIND_POINT_COMPUTE@ accesses a uniform
--     buffer, it /must/ not access values outside of the range of that
--     buffer specified in the bound descriptor set
--
-- -   If the
--     <{html_spec_relative}#features-features-robustBufferAccess robust buffer access>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @buffer@ /must/ be a valid @VkBuffer@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Compute         | Compute         |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall "vkCmdDispatchIndirect" vkCmdDispatchIndirect :: ("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()
-- | vkCmdCopyBuffer - Copy data between buffer regions
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', 'VkBufferCopy',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdCopyBuffer" vkCmdCopyBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()
-- | vkCmdCopyImage - Copy data between images
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- @vkCmdCopyImage@ /can/ be used to copy image data between multisample
-- images, but both images /must/ have the same number of samples.
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
--     @VK_IMAGE_USAGE_TRANSFER_SRC_BIT@ usage flag
--
-- -   If @srcImage@ is non-sparse then it /must/ be bound completely and
--     contiguously to a single @VkDeviceMemory@ object
--
-- -   @srcImageLayout@ /must/ specify the layout of the image subresources
--     of @srcImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
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
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   The 'Graphics.Vulkan.Core10.Core.VkFormat' of each of @srcImage@ and
--     @dstImage@ /must/ be compatible, as defined
--     <{html_spec_relative}#copies-images-format-compatibility below>
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage', 'VkImageCopy',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall "vkCmdCopyImage" vkCmdCopyImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()
-- | vkCmdBlitImage - Copy regions of an image, potentially performing format
-- conversion,
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
--     inverse of the equations described in
--     <{html_spec_relative}#textures-unnormalized-to-integer unnormalized to integer conversion>:
--
--     []
--         ubase = i + ½
--
--     []
--         vbase = j + ½
--
--     []
--         wbase = k + ½
--
-- -   These base coordinates are then offset by the first destination
--     offset:
--
--     []
--         uoffset = ubase - xdst0
--
--     []
--         voffset = vbase - ydst0
--
--     []
--         woffset = wbase - zdst0
--
--     []
--         aoffset = a - @baseArrayCount@dst
--
-- -   The scale is determined from the source and destination regions, and
--     applied to the offset coordinates:
--
--     []
--         scale_u = (xsrc1 - xsrc0) \/ (xdst1 - xdst0)
--
--     []
--         scale_v = (ysrc1 - ysrc0) \/ (ydst1 - ydst0)
--
--     []
--         scale_w = (zsrc1 - zsrc0) \/ (zdst1 - zdst0)
--
--     []
--         uscaled = uoffset * scaleu
--
--     []
--         vscaled = voffset * scalev
--
--     []
--         wscaled = woffset * scalew
--
-- -   Finally the source offset is added to the scaled coordinates, to
--     determine the final unnormalized coordinates used to sample from
--     @srcImage@:
--
--     []
--         u = uscaled + xsrc0
--
--     []
--         v = vscaled + ysrc0
--
--     []
--         w = wscaled + zsrc0
--
--     []
--         q = @mipLevel@
--
--     []
--         a = aoffset + @baseArrayCount@src
--
-- These coordinates are used to sample from the source image, as described
-- in <{html_spec_relative}#textures Image Operations chapter>, with the
-- filter mode equal to that of @filter@, a mipmap mode of
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
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   @dstImage@ /must/ use a format that supports
--     @VK_FORMAT_FEATURE_BLIT_DST_BIT@, which is indicated by
--     @VkFormatProperties@::@linearTilingFeatures@ (for linearly tiled
--     images) or @VkFormatProperties@::@optimalTilingFeatures@ (for
--     optimally tiled images) - as returned by
--     @vkGetPhysicalDeviceFormatProperties@
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
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Sampler.VkFilter',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage', 'VkImageBlit',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall "vkCmdBlitImage" vkCmdBlitImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()
-- | vkCmdCopyBufferToImage - Copy data from a buffer into an image
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source buffer to the specified region of the destination image.
--
-- == Valid Usage
--
-- -   The buffer region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @srcBuffer@
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @dstImage@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
--
-- -   @srcBuffer@ /must/ have been created with
--     @VK_BUFFER_USAGE_TRANSFER_SRC_BIT@ usage flag
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
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', 'VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall "vkCmdCopyBufferToImage" vkCmdCopyBufferToImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
-- | vkCmdCopyImageToBuffer - Copy image data into a buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- Each region in @pRegions@ is copied from the specified region of the
-- source image to the specified region of the destination buffer.
--
-- == Valid Usage
--
-- -   The image region specified by each element of @pRegions@ /must/ be a
--     region that is contained within @srcImage@
--
-- -   The buffer region specified by each element of @pRegions@ /must/ be
--     a region that is contained within @dstBuffer@
--
-- -   The union of all source regions, and the union of all destination
--     regions, specified by the elements of @pRegions@, /must/ not overlap
--     in memory
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
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', 'VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout'
foreign import ccall "vkCmdCopyImageToBuffer" vkCmdCopyImageToBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()
-- | vkCmdUpdateBuffer - Update a buffer’s contents from host memory
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- @dataSize@ /must/ be less than or equal to 65536 bytes. For larger
-- updates, applications /can/ use buffer to buffer
-- <{html_spec_relative}#copies-buffers copies>.
--
-- __Note__
--
-- Buffer updates performed with @vkCmdUpdateBuffer@ first copy the data
-- into command buffer memory when the command is recorded (which requires
-- additional storage and may incur an additional allocation), and then
-- copy the data from the command buffer into @dstBuffer@ when the command
-- is executed on a device.
--
-- The additional cost of this functionality compared to
-- <{html_spec_relative}#copies-buffers buffer to buffer copies> means it
-- is only recommended for very small amounts of data, and is why it is
-- limited to only 65536 bytes.
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall "vkCmdUpdateBuffer" vkCmdUpdateBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()
-- | vkCmdFillBuffer - Fill a region of a buffer with a fixed value
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   The @VkCommandPool@ that @commandBuffer@ was allocated from /must/
--     support graphics or compute operations
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@
foreign import ccall "vkCmdFillBuffer" vkCmdFillBuffer :: ("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()
-- | vkCmdClearColorImage - Clear regions of a color image
--
-- = Parameters
-- #_parameters#
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
-- -   @pColor@ is a pointer to a 'VkClearColorValue' structure that
--     contains the values the image subresource ranges will be cleared to
--     (see
--     <{html_spec_relative}#clears-values {html_spec_relative}#clears-values>
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ points to an array of
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     structures that describe a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in
--     <{html_spec_relative}#resources-image-views Image Views>. The
--     @aspectMask@ of all image subresource ranges /must/ only include
--     @VK_IMAGE_ASPECT_COLOR_BIT@.
--
-- = Description
-- #_description#
--
-- Each specified range in @pRanges@ is cleared to the value specified by
-- @pColor@.
--
-- == Valid Usage
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
-- -   @imageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'VkClearColorValue', 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
foreign import ccall "vkCmdClearColorImage" vkCmdClearColorImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
-- | vkCmdClearDepthStencilImage - Fill regions of a combined depth\/stencil
-- image
--
-- = Parameters
-- #_parameters#
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
--     <{html_spec_relative}#clears-values {html_spec_relative}#clears-values>
--     below).
--
-- -   @rangeCount@ is the number of image subresource range structures in
--     @pRanges@.
--
-- -   @pRanges@ points to an array of
--     'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
--     structures that describe a range of mipmap levels, array layers, and
--     aspects to be cleared, as described in
--     <{html_spec_relative}#resources-image-views Image Views>. The
--     @aspectMask@ of each image subresource range in @pRanges@ /can/
--     include @VK_IMAGE_ASPECT_DEPTH_BIT@ if the image format has a depth
--     component, and @VK_IMAGE_ASPECT_STENCIL_BIT@ if the image format has
--     a stencil component. @pDepthStencil@ is a pointer to a
--     @VkClearDepthStencilValue@ structure that contains the values the
--     image subresource ranges will be cleared to (see
--     <{html_spec_relative}#clears-values {html_spec_relative}#clears-values>
--     below).
--
-- = Description
-- #_description#
--
-- == Valid Usage
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'VkClearDepthStencilValue',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange'
foreign import ccall "vkCmdClearDepthStencilImage" vkCmdClearDepthStencilImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()
-- | vkCmdClearAttachments - Clear regions within bound framebuffer
-- attachments
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'VkClearAttachment', 'VkClearRect',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdClearAttachments" vkCmdClearAttachments :: ("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()
-- | vkCmdResolveImage - Resolve regions of an image
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   @srcImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
--
-- -   @dstImageLayout@ /must/ specify the layout of the image subresources
--     of @dstImage@ specified in @pRegions@ at the time this command is
--     executed on a @VkDevice@
--
-- -   @dstImageLayout@ /must/ be @VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL@ or
--     @VK_IMAGE_LAYOUT_GENERAL@
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 |                 |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout', 'VkImageResolve'
foreign import ccall "vkCmdResolveImage" vkCmdResolveImage :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()
-- | vkCmdSetEvent - Set an event object to signaled state
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @event@ is the event that will be signaled.
--
-- -   @stageMask@ specifies the
--     <{html_spec_relative}#synchronization-pipeline-stages source stage mask>
--     used to determine when the @event@ is signaled.
--
-- = Description
-- #_description#
--
-- When 'vkCmdSetEvent' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event signal operation which sets the event to the signaled state.
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <{html_spec_relative}#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @stageMask@.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
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
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall "vkCmdSetEvent" vkCmdSetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
-- | vkCmdResetEvent - Reset an event object to non-signaled state
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @event@ is the event that will be unsignaled.
--
-- -   @stageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <{html_spec_relative}#synchronization-pipeline-stages source stage mask>
--     used to determine when the @event@ is unsignaled.
--
-- = Description
-- #_description#
--
-- When 'vkCmdResetEvent' is submitted to a queue, it defines an execution
-- dependency on commands that were submitted before it, and defines an
-- event unsignal operation which resets the event to the unsignaled state.
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <{html_spec_relative}#synchronization-submission-order submission order>.
-- The synchronization scope is limited to operations on the pipeline
-- stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @stageMask@.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
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
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @stageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   When this command executes, @event@ /must/ not be waited on by a
--     @vkCmdWaitEvents@ command that is currently executing
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall "vkCmdResetEvent" vkCmdResetEvent :: ("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()
-- | vkCmdWaitEvents - Wait for one or more events and insert a set of memory
--
-- = Parameters
-- #_parameters#
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
--     the
--     <{html_spec_relative}#synchronization-pipeline-stages source stage mask>.
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <{html_spec_relative}#synchronization-pipeline-stages destination stage mask>.
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
-- #_description#
--
-- When @vkCmdWaitEvents@ is submitted to a queue, it defines a memory
-- dependency between prior event signal operations on the same queue or
-- the host, and subsequent commands. @vkCmdWaitEvents@ /must/ not be used
-- to wait on event signal operations occuring on other queues.
--
-- The first synchronization scope only includes event signal operations
-- that operate on members of @pEvents@, and the operations that
-- happened-before the event signal operations. Event signal operations
-- performed by 'vkCmdSetEvent' that occur earlier in
-- <{html_spec_relative}#synchronization-submission-order submission order>
-- are included in the first synchronization scope, if the
-- <{html_spec_relative}#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in their @stageMask@ parameter is
-- <{html_spec_relative}#synchronization-pipeline-stages-order logically earlier>
-- than or equal to the
-- <{html_spec_relative}#synchronization-pipeline-stages-order logically latest>
-- pipeline stage in @srcStageMask@. Event signal operations performed by
-- 'Graphics.Vulkan.Core10.Event.vkSetEvent' are only included in the first
-- synchronization scope if @VK_PIPELINE_STAGE_HOST_BIT@ is included in
-- @srcStageMask@.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <{html_spec_relative}#synchronization-submission-order submission order>.
-- The second synchronization scope is limited to operations on the
-- pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <{html_spec_relative}#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <{html_spec_relative}#synchronization-memory-barriers memory barriers>.
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
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
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
--     @commandBuffer@ was allocated from, as specified in the
--     <{html_spec_relative}#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the
--     <{html_spec_relative}#synchronization-access-types-supported table of supported access types>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ or
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the
--     <{html_spec_relative}#synchronization-access-types-supported table of supported access types>.
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'VkBufferMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Event.VkEvent', 'VkImageMemoryBarrier',
-- 'VkMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall "vkCmdWaitEvents" vkCmdWaitEvents :: ("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
-- | vkCmdPipelineBarrier - Insert a memory dependency
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer into which the command is
--     recorded.
--
-- -   @srcStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>.
--
-- -   @dstStageMask@ is a bitmask of
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' specifying
--     the
--     <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>.
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
-- #_description#
--
-- When 'vkCmdPipelineBarrier' is submitted to a queue, it defines a memory
-- dependency between commands that were submitted before it, and those
-- submitted after it.
--
-- If 'vkCmdPipelineBarrier' was recorded outside a render pass instance,
-- the first
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur earlier in
-- <{html_spec_relative}#synchronization-submission-order submission order>.
-- If 'vkCmdPipelineBarrier' was recorded inside a render pass instance,
-- the first synchronization scope includes only commands that occur
-- earlier in
-- <{html_spec_relative}#synchronization-submission-order submission order>
-- within the same subpass. In either case, the first synchronization scope
-- is limited to operations on the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@.
--
-- If 'vkCmdPipelineBarrier' was recorded outside a render pass instance,
-- the second
-- <{html_spec_relative}#synchronization-dependencies-scopes synchronization scope>
-- includes all commands that occur later in
-- <{html_spec_relative}#synchronization-submission-order submission order>.
-- If 'vkCmdPipelineBarrier' was recorded inside a render pass instance,
-- the second synchronization scope includes only commands that occur later
-- in
-- <{html_spec_relative}#synchronization-submission-order submission order>
-- within the same subpass. In either case, the second synchronization
-- scope is limited to operations on the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@.
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks source stage mask>
-- specified by @srcStageMask@. Within that, the first access scope only
-- includes the first access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <{html_spec_relative}#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the first access scope
-- includes no accesses.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access in the pipeline stages determined by the
-- <{html_spec_relative}#synchronization-pipeline-stages-masks destination stage mask>
-- specified by @dstStageMask@. Within that, the second access scope only
-- includes the second access scopes defined by elements of the
-- @pMemoryBarriers@, @pBufferMemoryBarriers@ and @pImageMemoryBarriers@
-- arrays, which each define a set of
-- <{html_spec_relative}#synchronization-memory-barriers memory barriers>.
-- If no memory barriers are specified, then the second access scope
-- includes no accesses.
--
-- If @dependencyFlags@ includes @VK_DEPENDENCY_BY_REGION_BIT@, then any
-- dependency between
-- <{html_spec_relative}#synchronization-framebuffer-regions framebuffer-space>
-- pipeline stages is
-- <{html_spec_relative}#synchronization-framebuffer-regions framebuffer-local>
-- - otherwise it is
-- <{html_spec_relative}#synchronization-framebuffer-regions framebuffer-global>.
--
-- == Valid Usage
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-geometryShader geometry shaders>
--     feature is not enabled, @dstStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
--     feature is not enabled, @srcStageMask@ /must/ not contain
--     @VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT@ or
--     @VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT@
--
-- -   If the
--     <{html_spec_relative}#features-features-tessellationShader tessellation shaders>
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
--     @commandBuffer@ was allocated from, as specified in the
--     <{html_spec_relative}#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ and
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @srcAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @srcStageMask@, as specified in the
--     <{html_spec_relative}#synchronization-access-types-supported table of supported access types>.
--
-- -   Each element of @pMemoryBarriers@, @pBufferMemoryBarriers@ and
--     @pImageMemoryBarriers@ /must/ not have any access flag included in
--     its @dstAccessMask@ member if that bit is not supported by any of
--     the pipeline stages in @dstStageMask@, as specified in the
--     <{html_spec_relative}#synchronization-access-types-supported table of supported access types>.
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Transfer        |                 |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'VkBufferMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pass.VkDependencyFlags', 'VkImageMemoryBarrier',
-- 'VkMemoryBarrier', 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlags'
foreign import ccall "vkCmdPipelineBarrier" vkCmdPipelineBarrier :: ("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()
-- | vkCmdBeginQuery - Begin a query
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- If the @queryType@ of the pool is @VK_QUERY_TYPE_OCCLUSION@ and @flags@
-- contains @VK_QUERY_CONTROL_PRECISE_BIT@, an implementation /must/ return
-- a result that matches the actual number of samples passed. This is
-- described in more detail in
-- <{html_spec_relative}#queries-occlusion Occlusion Queries>.
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
--     <{html_spec_relative}#queries-operation-active active> within
--     @commandBuffer@
--
-- -   All queries used by the command /must/ be unavailable
--
-- -   If the
--     <{html_spec_relative}#features-features-occlusionQueryPrecise precise occlusion queries>
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @flags@ /must/ be a valid combination of
--     'Graphics.Vulkan.Core10.CommandBuffer.VkQueryControlFlagBits' values
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkQueryControlFlags',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall "vkCmdBeginQuery" vkCmdBeginQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()
-- | vkCmdEndQuery - Ends a query
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
--     <{html_spec_relative}#queries-operation-active active>
--
-- -   @query@ /must/ be less than the number of queries in @queryPool@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall "vkCmdEndQuery" vkCmdEndQuery :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
-- | vkCmdResetQueryPool - Reset queries in a query pool
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall "vkCmdResetQueryPool" vkCmdResetQueryPool :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()
-- | vkCmdWriteTimestamp - Write a device timestamp into a query object
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pipelineStage@ /must/ be a valid
--     'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits' value
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Transfer        | Transfer        |
-- > | Secondary       |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool'
foreign import ccall "vkCmdWriteTimestamp" vkCmdWriteTimestamp :: ("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()
-- | vkCmdCopyQueryPoolResults - Copy the results of queries in a query pool
-- to a buffer object
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
--     <{html_spec_relative}#queries-operation-memorylayout here>
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Transfer        |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer',
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.Core10.Query.VkQueryResultFlags'
foreign import ccall "vkCmdCopyQueryPoolResults" vkCmdCopyQueryPoolResults :: ("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()
-- | vkCmdPushConstants - Update the values of push constants
--
-- = Parameters
-- #_parameters#
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
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Graphics        |                 |
-- > | Secondary       |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkShaderStageFlags'
foreign import ccall "vkCmdPushConstants" vkCmdPushConstants :: ("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()
-- | vkCmdBeginRenderPass - Begin a new render pass
--
-- = Parameters
-- #_parameters#
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
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Outside         | Graphics        | Graphics        |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkRenderPassBeginInfo',
-- 'VkSubpassContents'
foreign import ccall "vkCmdBeginRenderPass" vkCmdBeginRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()
-- | vkCmdNextSubpass - Transition to the next subpass of a render pass
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer in which to record the
--     command.
--
-- -   @contents@ specifies how the commands in the next subpass will be
--     provided, in the same fashion as the corresponding parameter of
--     'vkCmdBeginRenderPass'.
--
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer', 'VkSubpassContents'
foreign import ccall "vkCmdNextSubpass" vkCmdNextSubpass :: ("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()
-- | vkCmdEndRenderPass - End the current render pass
--
-- = Parameters
-- #_parameters#
--
-- -   @commandBuffer@ is the command buffer in which to end the current
--     render pass instance.
--
-- = Description
-- #_description#
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
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Inside          | Graphics        | Graphics        |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdEndRenderPass" vkCmdEndRenderPass :: ("commandBuffer" ::: VkCommandBuffer) -> IO ()
-- | vkCmdExecuteCommands - Execute a secondary command buffer from a primary
-- command buffer
--
-- = Parameters
-- #_parameters#
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
-- #_description#
--
-- If any element of @pCommandBuffers@ was not recorded with the
-- @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, and it was recorded
-- into any other primary command buffer which is currently in the
-- <{html_spec_relative}#commandbuffers-lifecycle executable or recording state>,
-- that primary command buffer becomes
-- <{html_spec_relative}#commandbuffers-lifecycle invalid>.
--
-- == Valid Usage
--
-- -   @commandBuffer@ /must/ have been allocated with a @level@ of
--     @VK_COMMAND_BUFFER_LEVEL_PRIMARY@
--
-- -   Each element of @pCommandBuffers@ /must/ have been allocated with a
--     @level@ of @VK_COMMAND_BUFFER_LEVEL_SECONDARY@
--
-- -   Each element of @pCommandBuffers@ /must/ be in the
--     <{html_spec_relative}#commandbuffers-lifecycle pending or executable state>.
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, and it was
--     recorded into any other primary command buffer, that primary command
--     buffer /must/ not be in the
--     <{html_spec_relative}#commandbuffers-lifecycle pending state>
--
-- -   If any element of @pCommandBuffers@ was not recorded with the
--     @VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT@ flag, it /must/ not
--     be in the
--     <{html_spec_relative}#commandbuffers-lifecycle pending state>.
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
--     <{html_spec_relative}#renderpass-compatibility compatible> with the
--     current render pass.
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
-- -   If the
--     <{html_spec_relative}#features-features-inheritedQueries inherited queries>
--     feature is not enabled, @commandBuffer@ /must/ not have any queries
--     <{html_spec_relative}#queries-operation-active active>
--
-- -   If @commandBuffer@ has a @VK_QUERY_TYPE_OCCLUSION@ query
--     <{html_spec_relative}#queries-operation-active active>, then each
--     element of @pCommandBuffers@ /must/ have been recorded with
--     @VkCommandBufferInheritanceInfo@::@occlusionQueryEnable@ set to
--     @VK_TRUE@
--
-- -   If @commandBuffer@ has a @VK_QUERY_TYPE_OCCLUSION@ query
--     <{html_spec_relative}#queries-operation-active active>, then each
--     element of @pCommandBuffers@ /must/ have been recorded with
--     @VkCommandBufferInheritanceInfo@::@queryFlags@ having all bits set
--     that are set for the query
--
-- -   If @commandBuffer@ has a @VK_QUERY_TYPE_PIPELINE_STATISTICS@ query
--     <{html_spec_relative}#queries-operation-active active>, then each
--     element of @pCommandBuffers@ /must/ have been recorded with
--     @VkCommandBufferInheritanceInfo@::@pipelineStatistics@ having all
--     bits set that are set in the @VkQueryPool@ the query uses
--
-- -   Each element of @pCommandBuffers@ /must/ not begin any query types
--     that are <{html_spec_relative}#queries-operation-active active> in
--     @commandBuffer@
--
-- == Valid Usage (Implicit)
--
-- -   @commandBuffer@ /must/ be a valid @VkCommandBuffer@ handle
--
-- -   @pCommandBuffers@ /must/ be a valid pointer to an array of
--     @commandBufferCount@ valid @VkCommandBuffer@ handles
--
-- -   @commandBuffer@ /must/ be in the
--     <#commandbuffers-lifecycle recording state>
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
-- > +-----------------+-----------------+-----------------+-----------------+
-- > | <#VkCommandBuff | <#vkCmdBeginRen | <#VkQueueFlagBi | <#synchronizati |
-- > | erLevel Command | derPass Render  | ts Supported Qu | on-pipeline-sta |
-- > |  Buffer Levels> | Pass Scope>     | eue Types>      | ges-types Pipel |
-- > |                 |                 |                 | ine Type>       |
-- > +=================+=================+=================+=================+
-- > | Primary         | Both            | Transfer        |                 |
-- > |                 |                 | Graphics        |                 |
-- > |                 |                 | Compute         |                 |
-- > +-----------------+-----------------+-----------------+-----------------+
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'
foreign import ccall "vkCmdExecuteCommands" vkCmdExecuteCommands :: ("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()
-- | VkClearRect - Structure specifying a clear rectangle
--
-- = Description
-- #_description#
--
-- The layers [@baseArrayLayer@, @baseArrayLayer@ + @layerCount@) counting
-- from the base layer of the attachment image view are cleared.
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D', 'vkCmdClearAttachments'
data VkClearRect = VkClearRect
  { -- No documentation found for Nested "VkClearRect" "vkRect"
  vkRect :: VkRect2D
  , -- No documentation found for Nested "VkClearRect" "vkBaseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "VkClearRect" "vkLayerCount"
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
-- = Description
-- #_description#
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
-- #_see_also#
--
-- 'VkBufferImageCopy',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'VkImageBlit', 'VkImageCopy', 'VkImageResolve'
data VkImageSubresourceLayers = VkImageSubresourceLayers
  { -- No documentation found for Nested "VkImageSubresourceLayers" "vkAspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkImageSubresourceLayers" "vkMipLevel"
  vkMipLevel :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "vkBaseArrayLayer"
  vkBaseArrayLayer :: Word32
  , -- No documentation found for Nested "VkImageSubresourceLayers" "vkLayerCount"
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
-- #_description#
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <{html_spec_relative}#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access types in the
-- <{html_spec_relative}#synchronization-access-masks destination access mask>
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdPipelineBarrier',
-- 'vkCmdWaitEvents'
data VkMemoryBarrier = VkMemoryBarrier
  { -- No documentation found for Nested "VkMemoryBarrier" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryBarrier" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryBarrier" "vkSrcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkMemoryBarrier" "vkDstAccessMask"
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
-- #_description#
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <{html_spec_relative}#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@, memory writes performed by that access type
-- are also made visible, as that access type is not performed through a
-- resource.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified buffer range, via
-- access types in the
-- <{html_spec_relative}#synchronization-access-masks destination access mask>.
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@ or @VK_ACCESS_HOST_READ_BIT@, available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <{html_spec_relative}#synchronization-queue-transfers-release queue family release operation>
-- for the specified buffer range, and the second access scope includes no
-- access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <{html_spec_relative}#synchronization-queue-transfers-acquire queue family acquire operation>
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
--     @VK_SHARING_MODE_CONCURRENT@, @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ /must/ both be @VK_QUEUE_FAMILY_IGNORED@
--
-- -   If @buffer@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@, @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ /must/ either both be
--     @VK_QUEUE_FAMILY_IGNORED@, or both be a valid queue family (see
--     <{html_spec_relative}#devsandqueues-queueprops {html_spec_relative}#devsandqueues-queueprops>)
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer', @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdPipelineBarrier',
-- 'vkCmdWaitEvents'
data VkBufferMemoryBarrier = VkBufferMemoryBarrier
  { -- No documentation found for Nested "VkBufferMemoryBarrier" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkSrcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkDstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkSrcQueueFamilyIndex"
  vkSrcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkDstQueueFamilyIndex"
  vkDstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkBuffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkOffset"
  vkOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferMemoryBarrier" "vkSize"
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
-- #_description#
--
-- The first
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <{html_spec_relative}#synchronization-access-masks source access mask>
-- specified by @srcAccessMask@. If @srcAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@, memory writes performed by that access type
-- are also made visible, as that access type is not performed through a
-- resource.
--
-- The second
-- <{html_spec_relative}#synchronization-dependencies-access-scopes access scope>
-- is limited to access to memory through the specified image subresource
-- range, via access types in the
-- <{html_spec_relative}#synchronization-access-masks destination access mask>
-- specified by @dstAccessMask@. If @dstAccessMask@ includes
-- @VK_ACCESS_HOST_WRITE_BIT@ or @VK_ACCESS_HOST_READ_BIT@, available
-- memory writes are also made visible to accesses of those types, as those
-- access types are not performed through a resource.
--
-- If @srcQueueFamilyIndex@ is not equal to @dstQueueFamilyIndex@, and
-- @srcQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <{html_spec_relative}#synchronization-queue-transfers-release queue family release operation>
-- for the specified image subresource range, and the second access scope
-- includes no access, as if @dstAccessMask@ was @0@.
--
-- If @dstQueueFamilyIndex@ is not equal to @srcQueueFamilyIndex@, and
-- @dstQueueFamilyIndex@ is equal to the current queue family, then the
-- memory barrier defines a
-- <{html_spec_relative}#synchronization-queue-transfers-acquire queue family acquire operation>
-- for the specified image subresource range, and the first access scope
-- includes no access, as if @srcAccessMask@ was @0@.
--
-- If @oldLayout@ is not equal to @newLayout@, then the memory barrier
-- defines an
-- <{html_spec_relative}#synchronization-image-layout-transitions image layout transition>
-- for the specified image subresource range.
--
-- Layout transitions that are performed via image memory barriers execute
-- in their entirety in
-- <{html_spec_relative}#synchronization-submission-order submission order>,
-- relative to other image layout transitions submitted to the same queue,
-- including those performed by
-- <{html_spec_relative}#renderpass render passes>. In effect there is an
-- implicit execution dependency from each such layout transition to all
-- layout transitions previously submitted to the same queue.
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
--     @VK_SHARING_MODE_CONCURRENT@, @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ /must/ both be @VK_QUEUE_FAMILY_IGNORED@
--
-- -   If @image@ was created with a sharing mode of
--     @VK_SHARING_MODE_EXCLUSIVE@, @srcQueueFamilyIndex@ and
--     @dstQueueFamilyIndex@ /must/ either both be
--     @VK_QUEUE_FAMILY_IGNORED@, or both be a valid queue family (see
--     <{html_spec_relative}#devsandqueues-queueprops {html_spec_relative}#devsandqueues-queueprops>).
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.Pass.VkAccessFlags',
-- 'Graphics.Vulkan.Core10.MemoryManagement.VkImage',
-- 'Graphics.Vulkan.Core10.Image.VkImageLayout',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageSubresourceRange',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdPipelineBarrier',
-- 'vkCmdWaitEvents'
data VkImageMemoryBarrier = VkImageMemoryBarrier
  { -- No documentation found for Nested "VkImageMemoryBarrier" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkSrcAccessMask"
  vkSrcAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkDstAccessMask"
  vkDstAccessMask :: VkAccessFlags
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkOldLayout"
  vkOldLayout :: VkImageLayout
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkNewLayout"
  vkNewLayout :: VkImageLayout
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkSrcQueueFamilyIndex"
  vkSrcQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkDstQueueFamilyIndex"
  vkDstQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkImage"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkImageMemoryBarrier" "vkSubresourceRange"
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
-- = Description
-- #_description#
--
-- = See Also
-- #_see_also#
--
-- @VkDeviceSize@, 'vkCmdCopyBuffer'
data VkBufferCopy = VkBufferCopy
  { -- No documentation found for Nested "VkBufferCopy" "vkSrcOffset"
  vkSrcOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferCopy" "vkDstOffset"
  vkDstOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferCopy" "vkSize"
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
-- #_description#
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
-- -   If the calling command’s @srcImage@ or @dstImage@ is of type
--     @VK_IMAGE_TYPE_2D@, then @extent.depth@ /must/ be @1@.
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
-- -   @srcSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- -   @dstSubresource@ /must/ be a valid @VkImageSubresourceLayers@
--     structure
--
-- = See Also
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdCopyImage'
data VkImageCopy = VkImageCopy
  { -- No documentation found for Nested "VkImageCopy" "vkSrcSubresource"
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "vkSrcOffset"
  vkSrcOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageCopy" "vkDstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageCopy" "vkDstOffset"
  vkDstOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageCopy" "vkExtent"
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
-- #_description#
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
-- #_see_also#
--
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdBlitImage'
data VkImageBlit = VkImageBlit
  { -- No documentation found for Nested "VkImageBlit" "vkSrcSubresource"
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit" "vkSrcOffsets"
  vkSrcOffsets :: Vector 2 VkOffset3D
  , -- No documentation found for Nested "VkImageBlit" "vkDstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageBlit" "vkDstOffsets"
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
-- #_description#
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
--     depth\/stencil format, then @bufferOffset@ /must/ be a multiple of
--     the format’s element size
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
--     @bufferRowLength@ /must/ be a multiple of the compressed texel block
--     width
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     @bufferImageHeight@ /must/ be a multiple of the compressed texel
--     block height
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     all members of @imageOffset@ /must/ be a multiple of the
--     corresponding dimensions of the compressed texel block
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     @bufferOffset@ /must/ be a multiple of the compressed texel block
--     size in bytes
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     @imageExtent.width@ /must/ be a multiple of the compressed texel
--     block width or (@imageExtent.width@ + @imageOffset.x@) /must/ equal
--     the image subresource width
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     @imageExtent.height@ /must/ be a multiple of the compressed texel
--     block height or (@imageExtent.height@ + @imageOffset.y@) /must/
--     equal the image subresource height
--
-- -   If the calling command’s @VkImage@ parameter is a compressed image,
--     @imageExtent.depth@ /must/ be a multiple of the compressed texel
--     block depth or (@imageExtent.depth@ + @imageOffset.z@) /must/ equal
--     the image subresource depth
--
-- -   The @aspectMask@ member of @imageSubresource@ /must/ specify aspects
--     present in the calling command’s @VkImage@ parameter
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
-- #_see_also#
--
-- @VkDeviceSize@,
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdCopyBufferToImage', 'vkCmdCopyImageToBuffer'
data VkBufferImageCopy = VkBufferImageCopy
  { -- No documentation found for Nested "VkBufferImageCopy" "vkBufferOffset"
  vkBufferOffset :: VkDeviceSize
  , -- No documentation found for Nested "VkBufferImageCopy" "vkBufferRowLength"
  vkBufferRowLength :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "vkBufferImageHeight"
  vkBufferImageHeight :: Word32
  , -- No documentation found for Nested "VkBufferImageCopy" "vkImageSubresource"
  vkImageSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkBufferImageCopy" "vkImageOffset"
  vkImageOffset :: VkOffset3D
  , -- No documentation found for Nested "VkBufferImageCopy" "vkImageExtent"
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
-- = Description
-- #_description#
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
-- #_see_also#
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkExtent3D',
-- 'VkImageSubresourceLayers',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkOffset3D',
-- 'vkCmdResolveImage'
data VkImageResolve = VkImageResolve
  { -- No documentation found for Nested "VkImageResolve" "vkSrcSubresource"
  vkSrcSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "vkSrcOffset"
  vkSrcOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageResolve" "vkDstSubresource"
  vkDstSubresource :: VkImageSubresourceLayers
  , -- No documentation found for Nested "VkImageResolve" "vkDstOffset"
  vkDstOffset :: VkOffset3D
  , -- No documentation found for Nested "VkImageResolve" "vkExtent"
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
-- #_description#
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
--     <{html_spec_relative}#renderpass-compatibility compatible> with the
--     @renderPass@ member of the @VkFramebufferCreateInfo@ structure
--     specified when creating @framebuffer@.
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
-- #_see_also#
--
-- 'VkClearValue', 'Graphics.Vulkan.Core10.Pass.VkFramebuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRect2D',
-- 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass',
-- 'Graphics.Vulkan.Core10.Core.VkStructureType', 'vkCmdBeginRenderPass'
data VkRenderPassBeginInfo = VkRenderPassBeginInfo
  { -- No documentation found for Nested "VkRenderPassBeginInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "vkRenderPass"
  vkRenderPass :: VkRenderPass
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "vkFramebuffer"
  vkFramebuffer :: VkFramebuffer
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "vkRenderArea"
  vkRenderArea :: VkRect2D
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "vkClearValueCount"
  vkClearValueCount :: Word32
  , -- No documentation found for Nested "VkRenderPassBeginInfo" "vkPClearValues"
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
-- = Description
-- #_description#
--
-- == Valid Usage
--
-- -   @depth@ /must/ be between @0.0@ and @1.0@, inclusive
--
-- = See Also
-- #_see_also#
--
-- 'VkClearValue', 'vkCmdClearDepthStencilImage'
data VkClearDepthStencilValue = VkClearDepthStencilValue
  { -- No documentation found for Nested "VkClearDepthStencilValue" "vkDepth"
  vkDepth :: CFloat
  , -- No documentation found for Nested "VkClearDepthStencilValue" "vkStencil"
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
-- #_description#
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
-- #_see_also#
--
-- 'VkClearValue',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkImageAspectFlags',
-- 'vkCmdClearAttachments'
data VkClearAttachment = VkClearAttachment
  { -- No documentation found for Nested "VkClearAttachment" "vkAspectMask"
  vkAspectMask :: VkImageAspectFlags
  , -- No documentation found for Nested "VkClearAttachment" "vkColorAttachment"
  vkColorAttachment :: Word32
  , -- No documentation found for Nested "VkClearAttachment" "vkClearValue"
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
-- #_description#
--
-- The members of @VkDrawIndirectCommand@ have the same meaning as the
-- similarly named parameters of 'vkCmdDraw'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <{html_spec_relative}#fxvertex-input {html_spec_relative}#fxvertex-input>
--
-- -   If the
--     <{html_spec_relative}#features-features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'vkCmdDrawIndirect'
data VkDrawIndirectCommand = VkDrawIndirectCommand
  { -- No documentation found for Nested "VkDrawIndirectCommand" "vkVertexCount"
  vkVertexCount :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "vkInstanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "vkFirstVertex"
  vkFirstVertex :: Word32
  , -- No documentation found for Nested "VkDrawIndirectCommand" "vkFirstInstance"
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
-- #_description#
--
-- The members of @VkDrawIndexedIndirectCommand@ have the same meaning as
-- the similarly named parameters of 'vkCmdDrawIndexed'.
--
-- == Valid Usage
--
-- -   For a given vertex buffer binding, any attribute data fetched /must/
--     be entirely contained within the corresponding vertex buffer
--     binding, as described in
--     <{html_spec_relative}#fxvertex-input {html_spec_relative}#fxvertex-input>
--
-- -   (@indexSize@ * (@firstIndex@ + @indexCount@) + @offset@) /must/ be
--     less than or equal to the size of the bound index buffer, with
--     @indexSize@ being based on the type specified by @indexType@, where
--     the index buffer, @indexType@, and @offset@ are specified via
--     @vkCmdBindIndexBuffer@
--
-- -   If the
--     <{html_spec_relative}#features-features-drawIndirectFirstInstance drawIndirectFirstInstance>
--     feature is not enabled, @firstInstance@ /must/ be @0@
--
-- = See Also
-- #_see_also#
--
-- 'vkCmdDrawIndexedIndirect'
data VkDrawIndexedIndirectCommand = VkDrawIndexedIndirectCommand
  { -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vkIndexCount"
  vkIndexCount :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vkInstanceCount"
  vkInstanceCount :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vkFirstIndex"
  vkFirstIndex :: Word32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vkVertexOffset"
  vkVertexOffset :: Int32
  , -- No documentation found for Nested "VkDrawIndexedIndirectCommand" "vkFirstInstance"
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
-- #_description#
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
-- #_see_also#
--
-- 'vkCmdDispatchIndirect'
data VkDispatchIndirectCommand = VkDispatchIndirectCommand
  { -- No documentation found for Nested "VkDispatchIndirectCommand" "vkX"
  vkX :: Word32
  , -- No documentation found for Nested "VkDispatchIndirectCommand" "vkY"
  vkY :: Word32
  , -- No documentation found for Nested "VkDispatchIndirectCommand" "vkZ"
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
-- #_description#
--
-- The four array elements of the clear color map to R, G, B, and A
-- components of image formats, in order.
--
-- If the image has more than one sample, the same value is written to all
-- samples for any pixels being cleared.
--
-- = See Also
-- #_see_also#
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
-- #_description#
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
-- #_see_also#
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
-- #_description#
--
-- @VkStencilFaceFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkStencilFaceFlagBits'.
--
-- = See Also
-- #_see_also#
--
-- 'VkStencilFaceFlagBits', 'vkCmdSetStencilCompareMask',
-- 'vkCmdSetStencilReference', 'vkCmdSetStencilWriteMask'
type VkStencilFaceFlags = VkStencilFaceFlagBits
