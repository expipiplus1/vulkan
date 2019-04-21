{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Queue
  ( CommandBuffer(..)
  , Fence
  , PipelineStageFlagBits
  , pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT
  , pattern PIPELINE_STAGE_VERTEX_INPUT_BIT
  , pattern PIPELINE_STAGE_VERTEX_SHADER_BIT
  , pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
  , pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
  , pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT
  , pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT
  , pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
  , pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
  , pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT
  , pattern PIPELINE_STAGE_TRANSFER_BIT
  , pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
  , pattern PIPELINE_STAGE_HOST_BIT
  , pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT
  , pattern PIPELINE_STAGE_ALL_COMMANDS_BIT
  , PipelineStageFlags
  , Queue(..)
  , Semaphore
  , withCStructSubmitInfo
  , fromCStructSubmitInfo
  , SubmitInfo(..)
  , deviceWaitIdle
  , getDeviceQueue
  , queueSubmit
  , queueWaitIdle
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Function
  ( (&)
  , on
  )
import Data.List
  ( minimum
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_STRUCTURE_TYPE_SUBMIT_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkSubmitInfo(..)
  , VkCommandBuffer
  , VkFence
  , VkQueue
  , VkSemaphore
  , vkDeviceWaitIdle
  , vkGetDeviceQueue
  , vkQueueSubmit
  , vkQueueWaitIdle
  , pattern VK_PIPELINE_STAGE_ALL_COMMANDS_BIT
  , pattern VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT
  , pattern VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
  , pattern VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT
  , pattern VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_HOST_BIT
  , pattern VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
  , pattern VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT
  , pattern VK_PIPELINE_STAGE_TRANSFER_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_INPUT_BIT
  , pattern VK_PIPELINE_STAGE_VERTEX_SHADER_BIT
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


data CommandBuffer = CommandBuffer
  { commandBufferHandle :: VkCommandBuffer
  , commandBufferCmds    :: DeviceCmds
  }
  deriving Show

instance Eq CommandBuffer where
  (==) = (==) `on` commandBufferHandle

instance Ord CommandBuffer where
  compare = compare `on` commandBufferHandle


-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkQueueBindSparse',
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit',
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type Fence = VkFence

-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = Description
--
-- __Note__
--
-- An execution dependency with only
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' in
-- the destination stage mask will only prevent that stage from executing
-- in subsequently submitted commands. As this stage does not perform any
-- actual execution, this is not observable - in effect, it does not delay
-- processing of subsequent commands. Similarly an execution dependency
-- with only
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT' in
-- the source stage mask will effectively not wait for any prior commands
-- to complete.
--
-- When defining a memory dependency, using only
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' or
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT' would
-- never make any accesses available and\/or visible because these stages
-- do not access memory.
--
-- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'
-- and 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT'
-- are useful for accomplishing layout transitions and queue ownership
-- operations when the required execution dependency is satisfied by other
-- means - for example, semaphore operations between queues.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'
type PipelineStageFlagBits = VkPipelineStageFlagBits


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT'
-- specifies the stage of the pipeline where any commands are initially
-- received by the queue.
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT'
-- specifies the stage of the pipeline where Draw\/DispatchIndirect data
-- structures are consumed. This stage also includes reading commands
-- written by
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCmdProcessCommandsNVX'.
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT = VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_VERTEX_INPUT_BIT'
-- specifies the stage of the pipeline where vertex and index buffers are
-- consumed.
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT = VK_PIPELINE_STAGE_VERTEX_INPUT_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_VERTEX_SHADER_BIT'
-- specifies the vertex shader stage.
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT = VK_PIPELINE_STAGE_VERTEX_SHADER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
-- specifies the tessellation control shader stage.
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
-- specifies the tessellation evaluation shader stage.
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
-- specifies the geometry shader stage.
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT'
-- specifies the fragment shader stage.
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
-- specifies the stage of the pipeline where early fragment tests (depth
-- and stencil tests before fragment shading) are performed. This stage
-- also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
-- specifies the stage of the pipeline where late fragment tests (depth and
-- stencil tests after fragment shading) are performed. This stage also
-- includes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass store operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
-- specifies the stage of the pipeline after blending where the final color
-- values are output from the pipeline. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>
-- and multisample resolve operations for framebuffer attachments with a
-- color or depth\/stencil format.
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT'
-- specifies the execution of a compute shader.
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT = VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TRANSFER_BIT'
-- specifies the execution of copy commands. This includes the operations
-- resulting from all
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#copies copy commands>,
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#clears clear commands>
-- (with the exception of
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdClearAttachments'),
-- and
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'.
pattern PIPELINE_STAGE_TRANSFER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TRANSFER_BIT = VK_PIPELINE_STAGE_TRANSFER_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'
-- specifies the final stage in the pipeline where operations generated by
-- all commands complete execution.
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT' specifies a
-- pseudo-stage indicating execution on the host of reads\/writes of device
-- memory. This stage is not invoked by any commands recorded in a command
-- buffer.
pattern PIPELINE_STAGE_HOST_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_HOST_BIT = VK_PIPELINE_STAGE_HOST_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT'
-- specifies the execution of all graphics pipeline stages, and is
-- equivalent to the logical OR of:
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT'
--
-- -   'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_VERTEX_INPUT_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_VERTEX_SHADER_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   'Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image.VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   'Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map.VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT = VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT


-- | 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_ALL_COMMANDS_BIT' is
-- equivalent to the logical OR of every other pipeline stage flag that is
-- supported on the queue it is used with.
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT

-- | VkPipelineStageFlags - Bitmask of VkPipelineStageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlagBits',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDependency',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdPipelineBarrier',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
type PipelineStageFlags = PipelineStageFlagBits

data Queue = Queue
  { queueHandle :: VkQueue
  , queueCmds    :: DeviceCmds
  }
  deriving Show

instance Eq Queue where
  (==) = (==) `on` queueHandle

instance Ord Queue where
  compare = compare `on` queueHandle


-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore'
type Semaphore = VkSemaphore


-- | VkSubmitInfo - Structure specifying a queue submit operation
--
-- = Description
--
-- The order that command buffers appear in @pCommandBuffers@ is used to
-- determine
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- and thus all the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-implicit implicit ordering guarantees>
-- that respect it. Other than these implicit ordering guarantees and any
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization explicit synchronization primitives>,
-- these command buffers /may/ overlap or otherwise execute out of order.
--
-- == Valid Usage
--
-- -   Each element of @pCommandBuffers@ /must/ not have been allocated
--     with
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_LEVEL_SECONDARY'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometry shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellation shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   Each element of @pWaitDstStageMask@ /must/ not include
--     'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_HOST_BIT'.
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-meshShader mesh shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-taskShader task shaders>
--     feature is not enabled, each element of @pWaitDstStageMask@ /must/
--     not contain
--     'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- Unresolved directive in VkSubmitInfo.txt -
-- include::{generated}\/validity\/structs\/VkSubmitInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer',
-- 'Graphics.Vulkan.C.Core10.Queue.VkPipelineStageFlags',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSemaphore',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit'
data SubmitInfo = SubmitInfo
  { -- Univalued member elided
  -- No documentation found for Nested "SubmitInfo" "pNext"
  next :: Maybe SomeVkStruct
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pWaitSemaphores"
  waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "SubmitInfo" "pWaitDstStageMask"
  waitDstStageMask :: Vector PipelineStageFlags
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pCommandBuffers"
  commandBuffers :: Vector CommandBuffer
  -- Length valued member elided
  , -- No documentation found for Nested "SubmitInfo" "pSignalSemaphores"
  signalSemaphores :: Vector Semaphore
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkSubmitInfo' and
-- marshal a 'SubmitInfo' into it. The 'VkSubmitInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructSubmitInfo :: SubmitInfo -> (VkSubmitInfo -> IO a) -> IO a
withCStructSubmitInfo marshalled cont = withVec (&) (signalSemaphores (marshalled :: SubmitInfo)) (\pPSignalSemaphores -> withVec ((&) . commandBufferHandle) (commandBuffers (marshalled :: SubmitInfo)) (\pPCommandBuffers -> withVec (&) (waitDstStageMask (marshalled :: SubmitInfo)) (\pPWaitDstStageMask -> withVec (&) (waitSemaphores (marshalled :: SubmitInfo)) (\pPWaitSemaphores -> maybeWith withSomeVkStruct (next (marshalled :: SubmitInfo)) (\pPNext -> cont (VkSubmitInfo VK_STRUCTURE_TYPE_SUBMIT_INFO pPNext (fromIntegral (minimum ([Data.Vector.length (waitSemaphores (marshalled :: SubmitInfo)), Data.Vector.length (waitDstStageMask (marshalled :: SubmitInfo))]))) pPWaitSemaphores pPWaitDstStageMask (fromIntegral (Data.Vector.length (commandBuffers (marshalled :: SubmitInfo)))) pPCommandBuffers (fromIntegral (Data.Vector.length (signalSemaphores (marshalled :: SubmitInfo)))) pPSignalSemaphores))))))

-- | A function to read a 'VkSubmitInfo' and all additional
-- structures in the pointer chain into a 'SubmitInfo'.
fromCStructSubmitInfo :: DeviceCmds -> VkSubmitInfo -> IO SubmitInfo
fromCStructSubmitInfo commandTable c = SubmitInfo <$> -- Univalued Member elided
                                                  maybePeek peekVkStruct (castPtr (vkPNext (c :: VkSubmitInfo)))
                                                  -- Length valued member elided
                                                  <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkSubmitInfo))) (peekElemOff (vkPWaitSemaphores (c :: VkSubmitInfo))))
                                                  <*> (Data.Vector.generateM (fromIntegral (vkWaitSemaphoreCount (c :: VkSubmitInfo))) (peekElemOff (vkPWaitDstStageMask (c :: VkSubmitInfo))))
                                                  -- Length valued member elided
                                                  <*> (Data.Vector.generateM (fromIntegral (vkCommandBufferCount (c :: VkSubmitInfo))) ((\p i -> flip CommandBuffer commandTable <$> peekElemOff p i) (vkPCommandBuffers (c :: VkSubmitInfo))))
                                                  -- Length valued member elided
                                                  <*> (Data.Vector.generateM (fromIntegral (vkSignalSemaphoreCount (c :: VkSubmitInfo))) (peekElemOff (vkPSignalSemaphores (c :: VkSubmitInfo))))

instance Zero SubmitInfo where
  zero = SubmitInfo Nothing
                    Data.Vector.empty
                    Data.Vector.empty
                    Data.Vector.empty
                    Data.Vector.empty



-- | vkDeviceWaitIdle - Wait for a device to become idle
--
-- = Parameters
--
-- -   @device@ is the logical device to idle.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Queue.vkDeviceWaitIdle' is equivalent to
-- calling 'Graphics.Vulkan.C.Core10.Queue.vkQueueWaitIdle' for all queues
-- owned by @device@.
--
-- Unresolved directive in vkDeviceWaitIdle.txt -
-- include::{generated}\/validity\/protos\/vkDeviceWaitIdle.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
deviceWaitIdle :: Device ->  IO ()
deviceWaitIdle = \(Device device' commandTable) -> vkDeviceWaitIdle commandTable device' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


-- | vkGetDeviceQueue - Get a queue handle from a device
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the queue.
--
-- -   @queueFamilyIndex@ is the index of the queue family to which the
--     queue belongs.
--
-- -   @queueIndex@ is the index within this queue family of the queue to
--     retrieve.
--
-- -   @pQueue@ is a pointer to a 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
--     object that will be filled with the handle for the requested queue.
--
-- == Valid Usage
--
-- -   @queueFamilyIndex@ /must/ be one of the queue family indices
--     specified when @device@ was created, via the
--     'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo' structure
--
-- -   @queueIndex@ /must/ be less than the number of queues created for
--     the specified queue family index when @device@ was created, via the
--     @queueCount@ member of the
--     'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo' structure
--
-- -   'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo'::@flags@
--     /must/ have been set to zero when @device@ was created
--
-- Unresolved directive in vkGetDeviceQueue.txt -
-- include::{generated}\/validity\/protos\/vkGetDeviceQueue.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
getDeviceQueue :: Device ->  Word32 ->  Word32 ->  IO (Queue)
getDeviceQueue = \(Device device' commandTable) -> \queueFamilyIndex' -> \queueIndex' -> alloca (\pQueue' -> vkGetDeviceQueue commandTable device' queueFamilyIndex' queueIndex' pQueue' *> (flip Queue commandTable <$> peek pQueue'))


-- | vkQueueSubmit - Submits a sequence of semaphores or command buffers to a
-- queue
--
-- = Parameters
--
-- -   @queue@ is the queue that the command buffers will be submitted to.
--
-- -   @submitCount@ is the number of elements in the @pSubmits@ array.
--
-- -   @pSubmits@ is a pointer to an array of
--     'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo' structures, each
--     specifying a command buffer submission batch.
--
-- -   @fence@ is an /optional/ handle to a fence to be signaled once all
--     submitted command buffers have completed execution. If @fence@ is
--     not 'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', it defines
--     a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences-signaling fence signal operation>.
--
-- = Description
--
-- __Note__
--
-- Submission can be a high overhead operation, and applications /should/
-- attempt to batch work together into as few calls to
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit' as possible.
--
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit' is a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-submission queue submission command>,
-- with each batch defined by an element of @pSubmits@ as an instance of
-- the 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo' structure. Batches
-- begin execution in the order they appear in @pSubmits@, but /may/
-- complete out of order.
--
-- Fence and semaphore operations submitted with
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit' have additional ordering
-- constraints compared to other submission commands, with dependencies
-- involving previous and subsequent queue operations. Information about
-- these additional constraints can be found in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores semaphore>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-fences fence>
-- sections of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- Details on the interaction of @pWaitDstStageMask@ with synchronization
-- are described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-waiting semaphore wait operation>
-- section of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization the synchronization chapter>.
--
-- The order that batches appear in @pSubmits@ is used to determine
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-submission-order submission order>,
-- and thus all the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-implicit implicit ordering guarantees>
-- that respect it. Other than these implicit ordering guarantees and any
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization explicit synchronization primitives>,
-- these batches /may/ overlap or otherwise execute out of order.
--
-- If any command buffer submitted to this queue is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>,
-- it is moved to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
-- Once execution of all submissions of a command buffer complete, it moves
-- from the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle executable state>.
-- If a command buffer was recorded with the
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT'
-- flag, it instead moves back to the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle invalid state>.
--
-- If 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit' fails, it /may/ return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY' or
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'. If it
-- does, the implementation /must/ ensure that the state and contents of
-- any resources or synchronization primitives referenced by the submitted
-- command buffers and any semaphores referenced by @pSubmits@ is
-- unaffected by the call or its failure. If
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueSubmit' fails in such a way that
-- the implementation is unable to make that guarantee, the implementation
-- /must/ return 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'. See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>.
--
-- == Valid Usage
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     be unsignaled
--
-- -   If @fence@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @fence@ /must/
--     not be associated with any other queue command that has not yet
--     completed execution on that queue
--
-- -   Any calls to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' or
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
--     that have been recorded into any of the command buffer elements of
--     the @pCommandBuffers@ member of any element of @pSubmits@, /must/
--     not reference any 'Graphics.Vulkan.C.Core10.Event.VkEvent' that is
--     referenced by any of those commands in a command buffer that has
--     been submitted to another queue and is still in the /pending state/.
--
-- -   Any stage flag included in any element of the @pWaitDstStageMask@
--     member of any element of @pSubmits@ /must/ be a pipeline stage
--     supported by one of the capabilities of @queue@, as specified in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-pipeline-stages-supported table of supported pipeline stages>.
--
-- -   Each element of the @pSignalSemaphores@ member of any element of
--     @pSubmits@ /must/ be unsignaled when the semaphore signal operation
--     it defines is executed on the device
--
-- -   When a semaphore unsignal operation defined by any element of the
--     @pWaitSemaphores@ member of any element of @pSubmits@ executes on
--     @queue@, no other queue /must/ be waiting on the same semaphore.
--
-- -   All elements of the @pWaitSemaphores@ member of all elements of
--     @pSubmits@ /must/ be semaphores that are signaled, or have
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-semaphores-signaling semaphore signal operations>
--     previously submitted for execution.
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>.
--
-- -   If any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   Any
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending or executable state>.
--
-- -   If any
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-secondary secondary command buffers recorded>
--     into any element of the @pCommandBuffers@ member of any element of
--     @pSubmits@ was not recorded with the
--     'Graphics.Vulkan.C.Core10.CommandBuffer.VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT',
--     it /must/ not be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>.
--
-- -   Each element of the @pCommandBuffers@ member of each element of
--     @pSubmits@ /must/ have been allocated from a
--     'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPool' that was
--     created for the same queue family @queue@ belongs to.
--
-- -   If any element of @pSubmits@->@pCommandBuffers@ includes a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-acquire Queue Family Transfer Acquire Operation>,
--     there /must/ exist a previously submitted
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-release Queue Family Transfer Release Operation>
--     on a queue in the queue family identified by the acquire operation,
--     with parameters matching the acquire operation as defined in the
--     definition of such
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#synchronization-queue-transfers-acquire acquire operations>,
--     and which happens before the acquire operation.
--
-- Unresolved directive in vkQueueSubmit.txt -
-- include::{generated}\/validity\/protos\/vkQueueSubmit.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkFence',
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo'
queueSubmit :: Queue ->  Vector SubmitInfo ->  Fence ->  IO ()
queueSubmit = \(Queue queue' commandTable) -> \submits' -> \fence' -> withVec withCStructSubmitInfo submits' (\pSubmits' -> vkQueueSubmit commandTable queue' (fromIntegral $ Data.Vector.length submits') pSubmits' fence' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ())))


-- | vkQueueWaitIdle - Wait for a queue to become idle
--
-- = Parameters
--
-- -   @queue@ is the queue on which to wait.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Queue.vkQueueWaitIdle' is equivalent to
-- submitting a fence to a queue and waiting with an infinite timeout for
-- that fence to signal.
--
-- Unresolved directive in vkQueueWaitIdle.txt -
-- include::{generated}\/validity\/protos\/vkQueueWaitIdle.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Queue.VkQueue'
queueWaitIdle :: Queue ->  IO ()
queueWaitIdle = \(Queue queue' commandTable) -> vkQueueWaitIdle commandTable queue' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))
