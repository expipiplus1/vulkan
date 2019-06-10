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
  , pattern PIPELINE_STAGE_RESERVED_27_BIT_KHR
  , pattern PIPELINE_STAGE_RESERVED_26_BIT_KHR
  , pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
  , pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  , pattern PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  , pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
  , pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
  , pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV
  , pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV
  , pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  , PipelineStageFlags
  , Queue(..)
  , Semaphore
#if defined(VK_USE_PLATFORM_GGP)
  , SubmitInfo(..)
#endif
  , deviceWaitIdle
  , getDeviceQueue
  , queueSubmit
  , queueWaitIdle
  , pattern VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR
  , pattern VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR
  ) where

import Data.Function
  ( on
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( length
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



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( pattern VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( pattern VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( pattern VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( pattern VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV
  , pattern VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
  , pattern VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Device(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( withVec
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


data CommandBuffer = CommandBuffer
  { commandBufferHandle :: VkCommandBuffer
  , commandBufferCmds    :: DeviceCmds
  }
  deriving Show

instance Eq CommandBuffer where
  (==) = (==) `on` commandBufferHandle

instance Ord CommandBuffer where
  compare = compare `on` commandBufferHandle


-- No documentation found for TopLevel "Fence"
type Fence = VkFence

-- No documentation found for TopLevel "PipelineStageFlagBits"
type PipelineStageFlagBits = VkPipelineStageFlagBits


{-# complete PIPELINE_STAGE_TOP_OF_PIPE_BIT, PIPELINE_STAGE_DRAW_INDIRECT_BIT, PIPELINE_STAGE_VERTEX_INPUT_BIT, PIPELINE_STAGE_VERTEX_SHADER_BIT, PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT, PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT, PIPELINE_STAGE_GEOMETRY_SHADER_BIT, PIPELINE_STAGE_FRAGMENT_SHADER_BIT, PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT, PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT, PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, PIPELINE_STAGE_COMPUTE_SHADER_BIT, PIPELINE_STAGE_TRANSFER_BIT, PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT, PIPELINE_STAGE_HOST_BIT, PIPELINE_STAGE_ALL_GRAPHICS_BIT, PIPELINE_STAGE_ALL_COMMANDS_BIT, PIPELINE_STAGE_RESERVED_27_BIT_KHR, PIPELINE_STAGE_RESERVED_26_BIT_KHR, PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT, PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT, PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX, PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV, PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV, PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV, PIPELINE_STAGE_TASK_SHADER_BIT_NV, PIPELINE_STAGE_MESH_SHADER_BIT_NV, PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT :: PipelineStageFlagBits #-}


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_TOP_OF_PIPE_BIT"
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_DRAW_INDIRECT_BIT"
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT = VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_VERTEX_INPUT_BIT"
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT = VK_PIPELINE_STAGE_VERTEX_INPUT_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_VERTEX_SHADER_BIT"
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT = VK_PIPELINE_STAGE_VERTEX_SHADER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT = VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_COMPUTE_SHADER_BIT"
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT = VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_TRANSFER_BIT"
pattern PIPELINE_STAGE_TRANSFER_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TRANSFER_BIT = VK_PIPELINE_STAGE_TRANSFER_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_HOST_BIT"
pattern PIPELINE_STAGE_HOST_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_HOST_BIT = VK_PIPELINE_STAGE_HOST_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_ALL_GRAPHICS_BIT"
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT = VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_ALL_COMMANDS_BIT"
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_RESERVED_27_BIT_KHR"
pattern PIPELINE_STAGE_RESERVED_27_BIT_KHR :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_RESERVED_27_BIT_KHR = VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_RESERVED_26_BIT_KHR"
pattern PIPELINE_STAGE_RESERVED_26_BIT_KHR :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_RESERVED_26_BIT_KHR = VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT = VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT = VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
pattern PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX = VK_PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV = VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV = VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV = VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_TASK_SHADER_BIT_NV"
pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV = VK_PIPELINE_STAGE_TASK_SHADER_BIT_NV


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_MESH_SHADER_BIT_NV"
pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV = VK_PIPELINE_STAGE_MESH_SHADER_BIT_NV


-- No documentation found for Nested "PipelineStageFlagBits" "PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT :: (a ~ PipelineStageFlagBits) => a
pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT = VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT

-- No documentation found for TopLevel "PipelineStageFlags"
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


-- No documentation found for TopLevel "Semaphore"
type Semaphore = VkSemaphore


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkSubmitInfo"
data SubmitInfo = SubmitInfo
  { -- No documentation found for Nested "SubmitInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "SubmitInfo" "pWaitSemaphores"
  waitSemaphores :: Vector Semaphore
  , -- No documentation found for Nested "SubmitInfo" "pWaitDstStageMask"
  waitDstStageMask :: Vector PipelineStageFlags
  , -- No documentation found for Nested "SubmitInfo" "pCommandBuffers"
  commandBuffers :: Vector CommandBuffer
  , -- No documentation found for Nested "SubmitInfo" "pSignalSemaphores"
  signalSemaphores :: Vector Semaphore
  }
  deriving (Show, Eq)

instance Zero SubmitInfo where
  zero = SubmitInfo Nothing
                    mempty
                    mempty
                    mempty
                    mempty

#endif


-- No documentation found for TopLevel "vkDeviceWaitIdle"
deviceWaitIdle :: Device ->  IO ()
deviceWaitIdle = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetDeviceQueue"
getDeviceQueue :: Device ->  Word32 ->  Word32 ->  IO (Queue)
getDeviceQueue = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkQueueSubmit"
queueSubmit :: Queue ->  Vector SubmitInfo ->  Fence ->  IO ()
queueSubmit = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkQueueWaitIdle"
queueWaitIdle :: Queue ->  IO ()
queueWaitIdle = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR"
pattern VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_RESERVED_26_BIT_KHR = VkPipelineStageFlagBits 0x04000000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR"
pattern VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR :: VkPipelineStageFlagBits
pattern VK_PIPELINE_STAGE_RESERVED_27_BIT_KHR = VkPipelineStageFlagBits 0x08000000
