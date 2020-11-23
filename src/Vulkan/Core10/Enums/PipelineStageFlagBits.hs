{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineStageFlagBits  ( PipelineStageFlagBits( PIPELINE_STAGE_TOP_OF_PIPE_BIT
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
                                                                         , PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV
                                                                         , PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                         , PIPELINE_STAGE_MESH_SHADER_BIT_NV
                                                                         , PIPELINE_STAGE_TASK_SHADER_BIT_NV
                                                                         , PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV
                                                                         , PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
                                                                         , PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.CheckpointDataNV',
-- 'PipelineStageFlags',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp'
newtype PipelineStageFlagBits = PipelineStageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_STAGE_TOP_OF_PIPE_BIT' is equivalent to
-- 'PIPELINE_STAGE_ALL_COMMANDS_BIT' with
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags' set to @0@ when
-- specified in the second synchronization scope, but specifies no stages
-- in the first scope.
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT = PipelineStageFlagBits 0x00000001
-- | 'PIPELINE_STAGE_DRAW_INDIRECT_BIT' specifies the stage of the pipeline
-- where Draw\/DispatchIndirect\/TraceRaysIndirect data structures are
-- consumed. This stage also includes reading commands written by
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdExecuteGeneratedCommandsNV'.
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
-- | #synchronization-pipeline-stages-transfer# 'PIPELINE_STAGE_TRANSFER_BIT'
-- specifies the following commands:
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy commands>,
--     including
--     'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'
--
-- -   'Vulkan.Extensions.VK_KHR_copy_commands2.cmdBlitImage2KHR' and
--     'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'
--
-- -   'Vulkan.Extensions.VK_KHR_copy_commands2.cmdResolveImage2KHR' and
--     'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears clear commands>,
--     with the exception of
--     'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'
pattern PIPELINE_STAGE_TRANSFER_BIT = PipelineStageFlagBits 0x00001000
-- | 'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' is equivalent to
-- 'PIPELINE_STAGE_ALL_COMMANDS_BIT' with
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags' set to @0@ when
-- specified in the first synchronization scope, but specifies no stages in
-- the second scope.
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = PipelineStageFlagBits 0x00002000
-- | 'PIPELINE_STAGE_HOST_BIT' specifies a pseudo-stage indicating execution
-- on the host of reads\/writes of device memory. This stage is not invoked
-- by any commands recorded in a command buffer.
pattern PIPELINE_STAGE_HOST_BIT = PipelineStageFlagBits 0x00004000
-- | 'PIPELINE_STAGE_ALL_GRAPHICS_BIT' specifies the execution of all
-- graphics pipeline stages, and is equivalent to the logical OR of:
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
-- -   'PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   'PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   'Vulkan.Extensions.VK_KHR_fragment_shading_rate.PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- -   'PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT = PipelineStageFlagBits 0x00008000
-- | 'PIPELINE_STAGE_ALL_COMMANDS_BIT' specifies all commands supported on
-- the queue it is used with.
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT = PipelineStageFlagBits 0x00010000
-- | 'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV' specifies the stage of the
-- pipeline where device-side preprocessing for generated commands via
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'
-- is handled.
pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV = PipelineStageFlagBits 0x00020000
-- | 'PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT' specifies the stage of
-- the pipeline where the fragment density map is read to
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT = PipelineStageFlagBits 0x00800000
-- | 'PIPELINE_STAGE_MESH_SHADER_BIT_NV' specifies the mesh shader stage.
pattern PIPELINE_STAGE_MESH_SHADER_BIT_NV = PipelineStageFlagBits 0x00100000
-- | 'PIPELINE_STAGE_TASK_SHADER_BIT_NV' specifies the task shader stage.
pattern PIPELINE_STAGE_TASK_SHADER_BIT_NV = PipelineStageFlagBits 0x00080000
-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV = PipelineStageFlagBits 0x00400000
-- | 'PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR' specifies the execution of
-- the ray tracing shader stages, via
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV' ,
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR', or
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR = PipelineStageFlagBits 0x00200000
-- | 'PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' specifies the
-- execution of
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV'
-- ,
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresIndirectKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyAccelerationStructureToMemoryKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyMemoryToAccelerationStructureKHR',
-- and
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR'.
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR = PipelineStageFlagBits 0x02000000
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
    PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV -> showString "PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV"
    PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT -> showString "PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
    PIPELINE_STAGE_MESH_SHADER_BIT_NV -> showString "PIPELINE_STAGE_MESH_SHADER_BIT_NV"
    PIPELINE_STAGE_TASK_SHADER_BIT_NV -> showString "PIPELINE_STAGE_TASK_SHADER_BIT_NV"
    PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV -> showString "PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV"
    PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR -> showString "PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR"
    PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR -> showString "PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR"
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
                            , ("PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV", pure PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV)
                            , ("PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT", pure PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT)
                            , ("PIPELINE_STAGE_MESH_SHADER_BIT_NV", pure PIPELINE_STAGE_MESH_SHADER_BIT_NV)
                            , ("PIPELINE_STAGE_TASK_SHADER_BIT_NV", pure PIPELINE_STAGE_TASK_SHADER_BIT_NV)
                            , ("PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV", pure PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV)
                            , ("PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR", pure PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR)
                            , ("PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR", pure PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR)
                            , ("PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT", pure PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT)
                            , ("PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT", pure PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineStageFlagBits")
                       v <- step readPrec
                       pure (PipelineStageFlagBits v)))

