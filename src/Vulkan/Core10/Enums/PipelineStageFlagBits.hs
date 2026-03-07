{-# language CPP #-}
-- No documentation found for Chapter "PipelineStageFlagBits"
module Vulkan.Core10.Enums.PipelineStageFlagBits  ( PipelineStageFlags
                                                  , PipelineStageFlagBits( PIPELINE_STAGE_TOP_OF_PIPE_BIT
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
                                                                         , PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT
                                                                         , PIPELINE_STAGE_MESH_SHADER_BIT_EXT
                                                                         , PIPELINE_STAGE_TASK_SHADER_BIT_EXT
                                                                         , PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                         , PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                         , PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
                                                                         , PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
                                                                         , PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
                                                                         , PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
                                                                         , PIPELINE_STAGE_NONE
                                                                         , ..
                                                                         )
                                                  ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type PipelineStageFlags = PipelineStageFlagBits

-- | VkPipelineStageFlagBits - Bitmask specifying pipeline stages
--
-- = Description
--
-- These values all have the same meaning as the equivalently named values
-- for 'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlags2'.
--
-- -   'PIPELINE_STAGE_NONE' specifies no stages of execution.
--
-- -   'PIPELINE_STAGE_DRAW_INDIRECT_BIT' specifies the stage of the
--     pipeline where @VkDrawIndirect*@ \/ @VkDispatchIndirect*@ \/
--     @VkTraceRaysIndirect*@ data structures are consumed. This stage also
--     includes reading commands written by
--     'Vulkan.Extensions.VK_NV_device_generated_commands.cmdExecuteGeneratedCommandsNV'.
--     This stage also includes reading commands written by
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.cmdExecuteGeneratedCommandsEXT'.
--
-- -   'PIPELINE_STAGE_TASK_SHADER_BIT_EXT' specifies the task shader
--     stage.
--
-- -   'PIPELINE_STAGE_MESH_SHADER_BIT_EXT' specifies the mesh shader
--     stage.
--
-- -   'PIPELINE_STAGE_VERTEX_INPUT_BIT' specifies the stage of the
--     pipeline where vertex and index buffers are consumed.
--
-- -   'PIPELINE_STAGE_VERTEX_SHADER_BIT' specifies the vertex shader
--     stage.
--
-- -   'PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT' specifies the
--     tessellation control shader stage.
--
-- -   'PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT' specifies the
--     tessellation evaluation shader stage.
--
-- -   'PIPELINE_STAGE_GEOMETRY_SHADER_BIT' specifies the geometry shader
--     stage.
--
-- -   'PIPELINE_STAGE_FRAGMENT_SHADER_BIT' specifies the fragment shader
--     stage.
--
-- -   'PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT' specifies the stage of the
--     pipeline where early fragment tests (depth and stencil tests before
--     fragment shading) are performed. This stage also includes
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-load-operations render pass load operations>
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   'PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT' specifies the stage of the
--     pipeline where late fragment tests (depth and stencil tests after
--     fragment shading) are performed. This stage also includes
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-store-operations render pass store operations>
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   'PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT' specifies the stage of
--     the pipeline after blending where the final color values are output
--     from the pipeline. This stage includes
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-blending blending>,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#framebuffer-logicop logic operations>,
--     render pass
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-load-operations load>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-store-operations store>
--     operations for color attachments,
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-resolve-operations render pass multisample resolve operations>,
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'.
--
-- -   'PIPELINE_STAGE_COMPUTE_SHADER_BIT' specifies the execution of a
--     compute shader.
--
-- -   #synchronization-pipeline-stages-transfer#
--     'PIPELINE_STAGE_TRANSFER_BIT' specifies the following commands:
--
--     -   All
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies copy commands>,
--         including
--         'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'
--
--     -   'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2'
--         and 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'
--
--     -   'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdResolveImage2'
--         and 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'
--
--     -   All
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#clears clear commands>,
--         with the exception of
--         'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'
--
-- -   'PIPELINE_STAGE_HOST_BIT' specifies a pseudo-stage indicating
--     execution on the host of reads\/writes of device memory. This stage
--     is not invoked by any commands recorded in a command buffer.
--
-- -   'PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' specifies the
--     execution of
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV'
--     ,
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresKHR',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresIndirectKHR',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyAccelerationStructureKHR',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyAccelerationStructureToMemoryKHR',
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyMemoryToAccelerationStructureKHR',
--     and
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR'.
--
-- -   'PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR' specifies the execution
--     of the ray tracing shader stages, via
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV' ,
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR', or
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR'
--
-- -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT'
--     specifies the execution of decompression commands with
--     'Vulkan.Extensions.VK_EXT_memory_decompression.cmdDecompressMemoryEXT'
--     and
--     'Vulkan.Extensions.VK_EXT_memory_decompression.cmdDecompressMemoryIndirectCountEXT'.
--
-- -   'PIPELINE_STAGE_ALL_GRAPHICS_BIT' specifies the execution of all
--     graphics pipeline stages, and is equivalent to the logical OR of:
--
--     -   'PIPELINE_STAGE_DRAW_INDIRECT_BIT'
--
--     -   'PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
--     -   'PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
--     -   'PIPELINE_STAGE_VERTEX_INPUT_BIT'
--
--     -   'PIPELINE_STAGE_VERTEX_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_FRAGMENT_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT'
--
--     -   'PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'
--
--     -   'PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'
--
--     -   'PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'
--
--     -   'PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'
--
--     -   'PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
--     -   'PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   'PIPELINE_STAGE_ALL_COMMANDS_BIT' specifies all operations performed
--     by all commands supported on the queue it is used with.
--
-- -   'PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT' specifies the stage
--     of the pipeline where the predicate of conditional rendering is
--     consumed.
--
-- -   'PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT' specifies the stage of
--     the pipeline where vertex attribute output values are written to the
--     transform feedback buffers.
--
-- -   'Vulkan.Extensions.VK_NV_device_generated_commands.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'
--     specifies the stage of the pipeline where device-side preprocessing
--     for generated commands via
--     'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'
--     is handled.
--
-- -   'PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT' specifies the stage of
--     the pipeline where device-side preprocessing for generated commands
--     via
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.cmdPreprocessGeneratedCommandsEXT'
--     is handled.
--
-- -   'PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' specifies
--     the stage of the pipeline where the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-shading-rate-image shading rate image>
--     is read to determine the fragment shading rate for portions of a
--     rasterized primitive.
--
-- -   'PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT' specifies the
--     stage of the pipeline where the fragment density map is read to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
--
-- -   'PIPELINE_STAGE_TOP_OF_PIPE_BIT' is equivalent to
--     'PIPELINE_STAGE_ALL_COMMANDS_BIT' with
--     'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags' set to @0@ when
--     specified in the second synchronization scope, but specifies no
--     stage of execution when specified in the first scope.
--
-- -   'PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT' is equivalent to
--     'PIPELINE_STAGE_ALL_COMMANDS_BIT' with
--     'Vulkan.Core10.Enums.AccessFlagBits.AccessFlags' set to @0@ when
--     specified in the first synchronization scope, but specifies no stage
--     of execution when specified in the second scope.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.CheckpointDataNV',
-- 'PipelineStageFlags',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp'
newtype PipelineStageFlagBits = PipelineStageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT"
pattern PIPELINE_STAGE_TOP_OF_PIPE_BIT = PipelineStageFlagBits 0x00000001

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_DRAW_INDIRECT_BIT"
pattern PIPELINE_STAGE_DRAW_INDIRECT_BIT = PipelineStageFlagBits 0x00000002

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_INPUT_BIT"
pattern PIPELINE_STAGE_VERTEX_INPUT_BIT = PipelineStageFlagBits 0x00000004

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_VERTEX_SHADER_BIT"
pattern PIPELINE_STAGE_VERTEX_SHADER_BIT = PipelineStageFlagBits 0x00000008

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT"
pattern PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT = PipelineStageFlagBits 0x00000010

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT"
pattern PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT = PipelineStageFlagBits 0x00000020

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT"
pattern PIPELINE_STAGE_GEOMETRY_SHADER_BIT = PipelineStageFlagBits 0x00000040

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT"
pattern PIPELINE_STAGE_FRAGMENT_SHADER_BIT = PipelineStageFlagBits 0x00000080

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT = PipelineStageFlagBits 0x00000100

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT = PipelineStageFlagBits 0x00000200

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT"
pattern PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT = PipelineStageFlagBits 0x00000400

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMPUTE_SHADER_BIT"
pattern PIPELINE_STAGE_COMPUTE_SHADER_BIT = PipelineStageFlagBits 0x00000800

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFER_BIT"
pattern PIPELINE_STAGE_TRANSFER_BIT = PipelineStageFlagBits 0x00001000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT"
pattern PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT = PipelineStageFlagBits 0x00002000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_HOST_BIT"
pattern PIPELINE_STAGE_HOST_BIT = PipelineStageFlagBits 0x00004000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT"
pattern PIPELINE_STAGE_ALL_GRAPHICS_BIT = PipelineStageFlagBits 0x00008000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ALL_COMMANDS_BIT"
pattern PIPELINE_STAGE_ALL_COMMANDS_BIT = PipelineStageFlagBits 0x00010000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT"
pattern PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT = PipelineStageFlagBits 0x00020000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_MESH_SHADER_BIT_EXT"
pattern PIPELINE_STAGE_MESH_SHADER_BIT_EXT = PipelineStageFlagBits 0x00100000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TASK_SHADER_BIT_EXT"
pattern PIPELINE_STAGE_TASK_SHADER_BIT_EXT = PipelineStageFlagBits 0x00080000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineStageFlagBits 0x00400000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
pattern PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT = PipelineStageFlagBits 0x00800000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR = PipelineStageFlagBits 0x00200000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR"
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR = PipelineStageFlagBits 0x02000000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT"
pattern PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT = PipelineStageFlagBits 0x00040000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT"
pattern PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT = PipelineStageFlagBits 0x01000000

-- No documentation found for Nested "VkPipelineStageFlagBits" "VK_PIPELINE_STAGE_NONE"
pattern PIPELINE_STAGE_NONE = PipelineStageFlagBits 0x00000000

conNamePipelineStageFlagBits :: String
conNamePipelineStageFlagBits = "PipelineStageFlagBits"

enumPrefixPipelineStageFlagBits :: String
enumPrefixPipelineStageFlagBits = "PIPELINE_STAGE_"

showTablePipelineStageFlagBits :: [(PipelineStageFlagBits, String)]
showTablePipelineStageFlagBits =
  [
    ( PIPELINE_STAGE_TOP_OF_PIPE_BIT
    , "TOP_OF_PIPE_BIT"
    )
  ,
    ( PIPELINE_STAGE_DRAW_INDIRECT_BIT
    , "DRAW_INDIRECT_BIT"
    )
  ,
    ( PIPELINE_STAGE_VERTEX_INPUT_BIT
    , "VERTEX_INPUT_BIT"
    )
  ,
    ( PIPELINE_STAGE_VERTEX_SHADER_BIT
    , "VERTEX_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT
    , "TESSELLATION_CONTROL_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT
    , "TESSELLATION_EVALUATION_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_GEOMETRY_SHADER_BIT
    , "GEOMETRY_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_FRAGMENT_SHADER_BIT
    , "FRAGMENT_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT
    , "EARLY_FRAGMENT_TESTS_BIT"
    )
  ,
    ( PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT
    , "LATE_FRAGMENT_TESTS_BIT"
    )
  ,
    ( PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
    , "COLOR_ATTACHMENT_OUTPUT_BIT"
    )
  ,
    ( PIPELINE_STAGE_COMPUTE_SHADER_BIT
    , "COMPUTE_SHADER_BIT"
    )
  , (PIPELINE_STAGE_TRANSFER_BIT, "TRANSFER_BIT")
  ,
    ( PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT
    , "BOTTOM_OF_PIPE_BIT"
    )
  , (PIPELINE_STAGE_HOST_BIT, "HOST_BIT")
  ,
    ( PIPELINE_STAGE_ALL_GRAPHICS_BIT
    , "ALL_GRAPHICS_BIT"
    )
  ,
    ( PIPELINE_STAGE_ALL_COMMANDS_BIT
    , "ALL_COMMANDS_BIT"
    )
  ,
    ( PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_EXT
    , "COMMAND_PREPROCESS_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_MESH_SHADER_BIT_EXT
    , "MESH_SHADER_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_TASK_SHADER_BIT_EXT
    , "TASK_SHADER_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT
    , "FRAGMENT_DENSITY_PROCESS_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR
    , "RAY_TRACING_SHADER_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT
    , "CONDITIONAL_RENDERING_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT
    , "TRANSFORM_FEEDBACK_BIT_EXT"
    )
  , (PIPELINE_STAGE_NONE, "NONE")
  ]

instance Show PipelineStageFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineStageFlagBits
      showTablePipelineStageFlagBits
      conNamePipelineStageFlagBits
      (\(PipelineStageFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineStageFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixPipelineStageFlagBits
      showTablePipelineStageFlagBits
      conNamePipelineStageFlagBits
      PipelineStageFlagBits
