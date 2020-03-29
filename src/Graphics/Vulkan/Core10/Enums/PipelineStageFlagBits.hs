{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.PipelineStageFlagBits  ( PipelineStageFlagBits( PIPELINE_STAGE_TOP_OF_PIPE_BIT
                                                                                  , PIPELINE_STAGE_DRAW_INDIRECT_BIT
                                                                                  , PIPELINE_STAGE_VERTEX_INPUT_BIT
                                                                                  , PIPELINE_STAGE_VERTEX_SHADER_BIT
                                                                                  , PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
                                                                                  , PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
                                                                                  , PIPELINE_STAGE_GEOMETRY_SHADER_BIT
                                                                                  , PIPELINE_STAGE_FRAGMENT_SHADER_BIT
                                                                                  , PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
                                                                                  , PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
                                                                                  , PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                                  , PIPELINE_STAGE_COMPUTE_SHADER_BIT
                                                                                  , PIPELINE_STAGE_TRANSFER_BIT
                                                                                  , PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
                                                                                  , PIPELINE_STAGE_HOST_BIT
                                                                                  , PIPELINE_STAGE_ALL_GRAPHICS_BIT
                                                                                  , PIPELINE_STAGE_ALL_COMMANDS_BIT
                                                                                  , PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                                  , PIPELINE_STAGE_MESH_SHADER_BIT_NV
                                                                                  , PIPELINE_STAGE_TASK_SHADER_BIT_NV
                                                                                  , PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
                                                                                  , PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
                                                                                  , PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
                                                                                  , PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX
                                                                                  , PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                                  , PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
                                                                                  , ..
                                                                                  )
                                                           , PipelineStageFlags
                                                           ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = Description
--
-- Note
--
-- An execution dependency with only 'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' in
-- the destination stage mask will only prevent that stage from executing
-- in subsequently submitted commands. As this stage does not perform any
-- actual execution, this is not observable - in effect, it does not delay
-- processing of subsequent commands. Similarly an execution dependency
-- with only 'PIPELINE_STAGE_TOP_OF_PIPE_BIT' in the source stage mask will
-- effectively not wait for any prior commands to complete.
--
-- When defining a memory dependency, using only
-- 'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' or 'PIPELINE_STAGE_TOP_OF_PIPE_BIT'
-- would never make any accesses available and\/or visible because these
-- stages do not access memory.
--
-- 'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' and 'PIPELINE_STAGE_TOP_OF_PIPE_BIT'
-- are useful for accomplishing layout transitions and queue ownership
-- operations when the required execution dependency is satisfied by other
-- means - for example, semaphore operations between queues.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.CheckpointDataNV',
-- 'PipelineStageFlags',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp'
newtype PipelineStageFlagBits = PipelineStageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'PIPELINE_STAGE_TOP_OF_PIPE_BIT' specifies the stage of the pipeline
-- where any commands are initially received by the queue.
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT = PipelineStageFlagBits 0x00000001
-- | 'PIPELINE_STAGE_DRAW_INDIRECT_BIT' specifies the stage of the pipeline
-- where Draw\/DispatchIndirect data structures are consumed. This stage
-- also includes reading commands written by
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.cmdProcessCommandsNVX'.
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT = PipelineStageFlagBits 0x00000002
-- | 'PIPELINE_STAGE_VERTEX_INPUT_BIT' specifies the stage of the pipeline
-- where vertex and index buffers are consumed.
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT = PipelineStageFlagBits 0x00000004
-- | 'PIPELINE_STAGE_VERTEX_SHADER_BIT' specifies the vertex shader stage.
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT = PipelineStageFlagBits 0x00000008
-- | 'PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT' specifies the
-- tessellation control shader stage.
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = PipelineStageFlagBits 0x00000010
-- | 'PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT' specifies the
-- tessellation evaluation shader stage.
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = PipelineStageFlagBits 0x00000020
-- | 'PIPELINE_STAGE_GEOMETRY_SHADER_BIT' specifies the geometry shader
-- stage.
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT = PipelineStageFlagBits 0x00000040
-- | 'PIPELINE_STAGE_FRAGMENT_SHADER_BIT' specifies the fragment shader
-- stage.
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT = PipelineStageFlagBits 0x00000080
-- | 'PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT' specifies the stage of the
-- pipeline where early fragment tests (depth and stencil tests before
-- fragment shading) are performed. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = PipelineStageFlagBits 0x00000100
-- | 'PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT' specifies the stage of the
-- pipeline where late fragment tests (depth and stencil tests after
-- fragment shading) are performed. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass store operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = PipelineStageFlagBits 0x00000200
-- | 'PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT' specifies the stage of the
-- pipeline after blending where the final color values are output from the
-- pipeline. This stage also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>
-- and multisample resolve operations for framebuffer attachments with a
-- color or depth\/stencil format.
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = PipelineStageFlagBits 0x00000400
-- | 'PIPELINE_STAGE_COMPUTE_SHADER_BIT' specifies the execution of a compute
-- shader.
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT = PipelineStageFlagBits 0x00000800
-- | 'PIPELINE_STAGE_TRANSFER_BIT' specifies the following commands:
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy commands>,
--     including
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'
--
-- -   'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'
--
-- -   'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears clear commands>,
--     with the exception of
--     'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'
pattern PIPELINE_STAGE_TRANSFER_BIT = PipelineStageFlagBits 0x00001000
-- | 'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' specifies the final stage in the
-- pipeline where operations generated by all commands complete execution.
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = PipelineStageFlagBits 0x00002000
-- | 'PIPELINE_STAGE_HOST_BIT' specifies a pseudo-stage indicating execution
-- on the host of reads\/writes of device memory. This stage is not invoked
-- by any commands recorded in a command buffer.
pattern PIPELINE_STAGE_HOST_BIT = PipelineStageFlagBits 0x00004000
-- | 'PIPELINE_STAGE_ALL_GRAPHICS_BIT' specifies the execution of all
-- graphics pipeline stages, and is equivalent to the logical OR of:
--
-- -   'PIPELINE_STAGE_TOP_OF_PIPE_BIT'
--
-- -   'PIPELINE_STAGE_DRAW_INDIRECT_BIT'
--
-- -   'PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   'PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
-- -   'PIPELINE_STAGE_VERTEX_INPUT_BIT'
--
-- -   'PIPELINE_STAGE_VERTEX_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_FRAGMENT_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
--
-- -   'PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
--
-- -   'PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
--
-- -   'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT'
--
-- -   'PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   'PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   'PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   'PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT = PipelineStageFlagBits 0x00008000
-- | 'PIPELINE_STAGE_ALL_COMMANDS_BIT' is equivalent to the logical OR of
-- every other pipeline stage flag that is supported on the queue it is
-- used with.
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT = PipelineStageFlagBits 0x00010000
-- | 'PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT' specifies the stage of
-- the pipeline where the fragment density map is read to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT = PipelineStageFlagBits 0x00800000
-- | 'PIPELINE_STAGE_MESH_SHADER_BIT_NV' specifies the mesh shader stage.
pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV = PipelineStageFlagBits 0x00100000
-- | 'PIPELINE_STAGE_TASK_SHADER_BIT_NV' specifies the task shader stage.
pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV = PipelineStageFlagBits 0x00080000
-- | 'PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV' specifies the
-- execution of
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
-- and
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV'.
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV = PipelineStageFlagBits 0x02000000
-- | 'PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV' specifies the execution of
-- the ray tracing shader stages.
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV = PipelineStageFlagBits 0x00200000
-- | 'PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV' specifies the stage of the
-- pipeline where the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
-- is read to determine the shading rate for portions of a rasterized
-- primitive.
pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV = PipelineStageFlagBits 0x00400000
-- | 'PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX' specifies the stage of the
-- pipeline where device-side generation of commands via
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.cmdProcessCommandsNVX'
-- is handled.
pattern PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX = PipelineStageFlagBits 0x00020000
-- | 'PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies the stage of
-- the pipeline where the predicate of conditional rendering is consumed.
pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT = PipelineStageFlagBits 0x00040000
-- | 'PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT' specifies the stage of the
-- pipeline where vertex attribute output values are written to the
-- transform feedback buffers.
pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT = PipelineStageFlagBits 0x01000000

type PipelineStageFlags = PipelineStageFlagBits

instance Show PipelineStageFlagBits where
  showsPrec p = \case
    PIPELINE_STAGE_TOP_OF_PIPE_BIT -> showString "PIPELINE_STAGE_TOP_OF_PIPE_BIT"
    PIPELINE_STAGE_DRAW_INDIRECT_BIT -> showString "PIPELINE_STAGE_DRAW_INDIRECT_BIT"
    PIPELINE_STAGE_VERTEX_INPUT_BIT -> showString "PIPELINE_STAGE_VERTEX_INPUT_BIT"
    PIPELINE_STAGE_VERTEX_SHADER_BIT -> showString "PIPELINE_STAGE_VERTEX_SHADER_BIT"
    PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT -> showString "PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
    PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT -> showString "PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
    PIPELINE_STAGE_GEOMETRY_SHADER_BIT -> showString "PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
    PIPELINE_STAGE_FRAGMENT_SHADER_BIT -> showString "PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
    PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT -> showString "PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
    PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT -> showString "PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
    PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT -> showString "PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
    PIPELINE_STAGE_COMPUTE_SHADER_BIT -> showString "PIPELINE_STAGE_COMPUTE_SHADER_BIT"
    PIPELINE_STAGE_TRANSFER_BIT -> showString "PIPELINE_STAGE_TRANSFER_BIT"
    PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT -> showString "PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
    PIPELINE_STAGE_HOST_BIT -> showString "PIPELINE_STAGE_HOST_BIT"
    PIPELINE_STAGE_ALL_GRAPHICS_BIT -> showString "PIPELINE_STAGE_ALL_GRAPHICS_BIT"
    PIPELINE_STAGE_ALL_COMMANDS_BIT -> showString "PIPELINE_STAGE_ALL_COMMANDS_BIT"
    PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT -> showString "PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
    PIPELINE_STAGE_MESH_SHADER_BIT_NV -> showString "PIPELINE_STAGE_MESH_SHADER_BIT_NV"
    PIPELINE_STAGE_TASK_SHADER_BIT_NV -> showString "PIPELINE_STAGE_TASK_SHADER_BIT_NV"
    PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV -> showString "PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
    PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV -> showString "PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
    PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV -> showString "PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
    PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX -> showString "PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX"
    PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT -> showString "PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
    PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT -> showString "PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
    PipelineStageFlagBits x -> showParen (p >= 11) (showString "PipelineStageFlagBits 0x" . showHex x)

instance Read PipelineStageFlagBits where
  readPrec = parens (choose [("PIPELINE_STAGE_TOP_OF_PIPE_BIT", pure PIPELINE_STAGE_TOP_OF_PIPE_BIT)
                            , ("PIPELINE_STAGE_DRAW_INDIRECT_BIT", pure PIPELINE_STAGE_DRAW_INDIRECT_BIT)
                            , ("PIPELINE_STAGE_VERTEX_INPUT_BIT", pure PIPELINE_STAGE_VERTEX_INPUT_BIT)
                            , ("PIPELINE_STAGE_VERTEX_SHADER_BIT", pure PIPELINE_STAGE_VERTEX_SHADER_BIT)
                            , ("PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT", pure PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT)
                            , ("PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT", pure PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT)
                            , ("PIPELINE_STAGE_GEOMETRY_SHADER_BIT", pure PIPELINE_STAGE_GEOMETRY_SHADER_BIT)
                            , ("PIPELINE_STAGE_FRAGMENT_SHADER_BIT", pure PIPELINE_STAGE_FRAGMENT_SHADER_BIT)
                            , ("PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT", pure PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT)
                            , ("PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT", pure PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT)
                            , ("PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT", pure PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT)
                            , ("PIPELINE_STAGE_COMPUTE_SHADER_BIT", pure PIPELINE_STAGE_COMPUTE_SHADER_BIT)
                            , ("PIPELINE_STAGE_TRANSFER_BIT", pure PIPELINE_STAGE_TRANSFER_BIT)
                            , ("PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT", pure PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT)
                            , ("PIPELINE_STAGE_HOST_BIT", pure PIPELINE_STAGE_HOST_BIT)
                            , ("PIPELINE_STAGE_ALL_GRAPHICS_BIT", pure PIPELINE_STAGE_ALL_GRAPHICS_BIT)
                            , ("PIPELINE_STAGE_ALL_COMMANDS_BIT", pure PIPELINE_STAGE_ALL_COMMANDS_BIT)
                            , ("PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT", pure PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT)
                            , ("PIPELINE_STAGE_MESH_SHADER_BIT_NV", pure PIPELINE_STAGE_MESH_SHADER_BIT_NV)
                            , ("PIPELINE_STAGE_TASK_SHADER_BIT_NV", pure PIPELINE_STAGE_TASK_SHADER_BIT_NV)
                            , ("PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV", pure PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV)
                            , ("PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV", pure PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV)
                            , ("PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV", pure PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV)
                            , ("PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX", pure PIPELINE_STAGE_COMMAND_PROCESS_BIT_NVX)
                            , ("PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT", pure PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT)
                            , ("PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT", pure PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineStageFlagBits")
                       v <- step readPrec
                       pure (PipelineStageFlagBits v)))

