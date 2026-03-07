{-# language CPP #-}
-- No documentation found for Chapter "PipelineStageFlags2"
module Vulkan.Core13.Enums.PipelineStageFlags2  ( pattern PIPELINE_STAGE_2_TRANSFER_BIT
                                                , PipelineStageFlags2
                                                , PipelineStageFlagBits2( PIPELINE_STAGE_2_NONE
                                                                        , PIPELINE_STAGE_2_TOP_OF_PIPE_BIT
                                                                        , PIPELINE_STAGE_2_DRAW_INDIRECT_BIT
                                                                        , PIPELINE_STAGE_2_VERTEX_INPUT_BIT
                                                                        , PIPELINE_STAGE_2_VERTEX_SHADER_BIT
                                                                        , PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT
                                                                        , PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT
                                                                        , PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT
                                                                        , PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT
                                                                        , PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT
                                                                        , PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT
                                                                        , PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
                                                                        , PIPELINE_STAGE_2_COMPUTE_SHADER_BIT
                                                                        , PIPELINE_STAGE_2_ALL_TRANSFER_BIT
                                                                        , PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT
                                                                        , PIPELINE_STAGE_2_HOST_BIT
                                                                        , PIPELINE_STAGE_2_ALL_GRAPHICS_BIT
                                                                        , PIPELINE_STAGE_2_ALL_COMMANDS_BIT
                                                                        , PIPELINE_STAGE_2_COPY_BIT
                                                                        , PIPELINE_STAGE_2_RESOLVE_BIT
                                                                        , PIPELINE_STAGE_2_BLIT_BIT
                                                                        , PIPELINE_STAGE_2_CLEAR_BIT
                                                                        , PIPELINE_STAGE_2_INDEX_INPUT_BIT
                                                                        , PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT
                                                                        , PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT
                                                                        , PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT
                                                                        , PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR
                                                                        , PIPELINE_STAGE_2_DATA_GRAPH_BIT_ARM
                                                                        , PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV
                                                                        , PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV
                                                                        , PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI
                                                                        , PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT
                                                                        , PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR
                                                                        , PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI
                                                                        , PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI
                                                                        , PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT
                                                                        , PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT
                                                                        , PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                        , PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR
                                                                        , PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
                                                                        , PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                        , PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT
                                                                        , PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT
                                                                        , PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT
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
import Vulkan.Core10.FundamentalTypes (Flags64)
-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TRANSFER_BIT"
pattern PIPELINE_STAGE_2_TRANSFER_BIT = PIPELINE_STAGE_2_ALL_TRANSFER_BIT


type PipelineStageFlags2 = PipelineStageFlagBits2

-- | VkPipelineStageFlagBits2 - Pipeline stage flags for
-- VkPipelineStageFlags2
--
-- = Description
--
-- -   'PIPELINE_STAGE_2_NONE' specifies no stages of execution.
--
-- -   'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT' specifies the stage of the
--     pipeline where indirect command parameters are consumed. This stage
--     also includes reading commands written by
--     'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
--     This stage also includes reading commands written by
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.cmdPreprocessGeneratedCommandsEXT'.
--
-- -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT' specifies the task shader
--     stage.
--
-- -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT' specifies the mesh shader
--     stage.
--
-- -   'PIPELINE_STAGE_2_INDEX_INPUT_BIT' specifies the stage of the
--     pipeline where index buffers are consumed.
--
-- -   'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT' specifies the stage of
--     the pipeline where vertex buffers are consumed.
--
-- -   'PIPELINE_STAGE_2_VERTEX_INPUT_BIT' is equivalent to the logical OR
--     of:
--
--     -   'PIPELINE_STAGE_2_INDEX_INPUT_BIT'
--
--     -   'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT'
--
-- -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT' specifies the vertex shader
--     stage.
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT' specifies the
--     tessellation control shader stage.
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT' specifies the
--     tessellation evaluation shader stage.
--
-- -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT' specifies the geometry shader
--     stage.
--
-- -   'PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT' is equivalent to
--     specifying all supported
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stages>:
--
--     -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI'
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT' specifies the fragment shader
--     stage.
--
-- -   'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT' specifies the stage of
--     the pipeline where early fragment tests (depth and stencil tests
--     before fragment shading) are performed. This stage also includes
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-load-operations render pass load operations>
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT' specifies the stage of
--     the pipeline where late fragment tests (depth and stencil tests
--     after fragment shading) are performed. This stage also includes
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-store-operations render pass store operations>
--     for framebuffer attachments with a depth\/stencil format.
--
-- -   'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT' specifies the stage
--     of the pipeline where final color values are output from the
--     pipeline. This stage includes
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
-- -   'PIPELINE_STAGE_2_COMPUTE_SHADER_BIT' specifies the compute shader
--     stage.
--
-- -   'PIPELINE_STAGE_2_HOST_BIT' specifies a pseudo-stage indicating
--     execution on the host of reads\/writes of device memory. This stage
--     is not invoked by any commands recorded in a command buffer.
--
-- -   'PIPELINE_STAGE_2_COPY_BIT' specifies the execution of all
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#copies copy commands>,
--     including
--     'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'.
--
-- -   'PIPELINE_STAGE_2_BLIT_BIT' specifies the execution of
--     'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
--
-- -   'PIPELINE_STAGE_2_RESOLVE_BIT' specifies the execution of
--     'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'.
--
-- -   'PIPELINE_STAGE_2_CLEAR_BIT' specifies the execution of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#clears clear commands>,
--     with the exception of
--     'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'.
--
-- -   'PIPELINE_STAGE_2_ALL_TRANSFER_BIT' is equivalent to specifying all
--     of:
--
--     -   'PIPELINE_STAGE_2_COPY_BIT'
--
--     -   'PIPELINE_STAGE_2_BLIT_BIT'
--
--     -   'PIPELINE_STAGE_2_RESOLVE_BIT'
--
--     -   'PIPELINE_STAGE_2_CLEAR_BIT'
--
--     -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
--
-- -   'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR' specifies the
--     execution of the ray tracing shader stages.
--
-- -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' specifies
--     the execution of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure acceleration structure commands>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-copying acceleration structure copy commands>.
--
-- -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR' specifies the
--     execution of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-copying acceleration structure copy commands>.
--
-- -   'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT' specifies the execution of all
--     graphics pipeline stages, and is equivalent to the logical OR of:
--
--     -   'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT'
--
--     -   'PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR'
--
--     -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_VERTEX_INPUT_BIT'
--
--     -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT'
--
--     -   'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT'
--
--     -   'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT'
--
--     -   'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--
--     -   'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
--     -   'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
--     -   'PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI'
--
--     -   'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
--
--     -   'PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI'
--
-- -   'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' specifies all operations
--     performed by all commands supported on the queue it is used with.
--
-- -   'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT' specifies the stage
--     of the pipeline where the predicate of conditional rendering is
--     consumed.
--
-- -   'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT' specifies the stage of
--     the pipeline where vertex attribute output values are written to the
--     transform feedback buffers.
--
-- -   'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
--     specifies the stage of the pipeline where device-side generation of
--     commands via
--     'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'
--     is handled.
--
-- -   'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT' specifies the stage of
--     the pipeline where device-side generation of commands via
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.cmdPreprocessGeneratedCommandsEXT'
--     is handled.
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     specifies the stage of the pipeline where the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-shading-rate-image shading rate image>
--     is read to determine the fragment shading rate for portions of a
--     rasterized primitive.
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT' specifies the
--     stage of the pipeline where the fragment density map is read to
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
--
-- -   'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI' specifies the stage of
--     the pipeline where the invocation mask image is read by the
--     implementation to optimize the ray dispatch.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineStageFlagBits2KHR VK_PIPELINE_STAGE_2_VIDEO_DECODE_BIT_KHR>
--     specifies the execution of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-decode-operations video decode operations>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkPipelineStageFlagBits2KHR VK_PIPELINE_STAGE_2_VIDEO_ENCODE_BIT_KHR>
--     specifies the execution of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operations>.
--
-- -   'PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV' specifies the stage of the
--     pipeline where
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#opticalflow-operations optical flow operation>
--     are performed.
--
-- -   'PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI' specifies the subpass
--     shading shader stage.
--
-- -   'PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT' specifies the execution of
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#micromap micromap commands>.
--
-- -   'PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI' specifies the
--     cluster culling shader stage.
--
-- -   'PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV'
--     specifies the execution of
--     'Vulkan.Extensions.VK_NV_cooperative_vector.cmdConvertCooperativeVectorMatrixNV'.
--
-- -   'PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR' specifies the stage of the
--     pipeline where indirect copy commands (vkCmdCopyMemoryIndirect* and
--     vkCmdCopyMemoryToImageIndirect*) parameters are consumed.
--
-- -   'PIPELINE_STAGE_2_TOP_OF_PIPE_BIT' is equivalent to
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' with
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2' set to @0@ when
--     specified in the second synchronization scope, but equivalent to
--     'PIPELINE_STAGE_2_NONE' in the first scope.
--
-- -   'PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT' is equivalent to
--     'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' with
--     'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2' set to @0@ when
--     specified in the first synchronization scope, but equivalent to
--     'PIPELINE_STAGE_2_NONE' in the second scope.
--
-- The @TOP@ and @BOTTOM@ pipeline stages are legacy, and applications
-- should prefer 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' and
-- 'PIPELINE_STAGE_2_NONE'.
--
-- The 'PipelineStageFlags2' bitmask goes beyond the 31 individual bit
-- flags allowable within a C99 enum, which is how
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits' is
-- defined. The first 31 values are common to both, and are
-- interchangeable.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'PipelineStageFlags2'
newtype PipelineStageFlagBits2 = PipelineStageFlagBits2 Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_NONE"
pattern PIPELINE_STAGE_2_NONE = PipelineStageFlagBits2 0x0000000000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT"
pattern PIPELINE_STAGE_2_TOP_OF_PIPE_BIT = PipelineStageFlagBits2 0x0000000000000001

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT"
pattern PIPELINE_STAGE_2_DRAW_INDIRECT_BIT = PipelineStageFlagBits2 0x0000000000000002

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT"
pattern PIPELINE_STAGE_2_VERTEX_INPUT_BIT = PipelineStageFlagBits2 0x0000000000000004

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT"
pattern PIPELINE_STAGE_2_VERTEX_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000008

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT"
pattern PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000010

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT"
pattern PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000020

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT"
pattern PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000040

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT"
pattern PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000080

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT = PipelineStageFlagBits2 0x0000000000000100

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT"
pattern PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT = PipelineStageFlagBits2 0x0000000000000200

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT"
pattern PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT = PipelineStageFlagBits2 0x0000000000000400

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT"
pattern PIPELINE_STAGE_2_COMPUTE_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000800

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT"
pattern PIPELINE_STAGE_2_ALL_TRANSFER_BIT = PipelineStageFlagBits2 0x0000000000001000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT"
pattern PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT = PipelineStageFlagBits2 0x0000000000002000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_HOST_BIT"
pattern PIPELINE_STAGE_2_HOST_BIT = PipelineStageFlagBits2 0x0000000000004000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT"
pattern PIPELINE_STAGE_2_ALL_GRAPHICS_BIT = PipelineStageFlagBits2 0x0000000000008000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT"
pattern PIPELINE_STAGE_2_ALL_COMMANDS_BIT = PipelineStageFlagBits2 0x0000000000010000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_COPY_BIT"
pattern PIPELINE_STAGE_2_COPY_BIT = PipelineStageFlagBits2 0x0000000100000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_RESOLVE_BIT"
pattern PIPELINE_STAGE_2_RESOLVE_BIT = PipelineStageFlagBits2 0x0000000200000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_BLIT_BIT"
pattern PIPELINE_STAGE_2_BLIT_BIT = PipelineStageFlagBits2 0x0000000400000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_CLEAR_BIT"
pattern PIPELINE_STAGE_2_CLEAR_BIT = PipelineStageFlagBits2 0x0000000800000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT"
pattern PIPELINE_STAGE_2_INDEX_INPUT_BIT = PipelineStageFlagBits2 0x0000001000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT"
pattern PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT = PipelineStageFlagBits2 0x0000002000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT"
pattern PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT = PipelineStageFlagBits2 0x0000004000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT"
pattern PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT = PipelineStageFlagBits2 0x0000200000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR"
pattern PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR = PipelineStageFlagBits2 0x0000400000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_DATA_GRAPH_BIT_ARM"
pattern PIPELINE_STAGE_2_DATA_GRAPH_BIT_ARM = PipelineStageFlagBits2 0x0000040000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV"
pattern PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV = PipelineStageFlagBits2 0x0000100000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV"
pattern PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV = PipelineStageFlagBits2 0x0000000020000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI"
pattern PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI = PipelineStageFlagBits2 0x0000020000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT"
pattern PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT = PipelineStageFlagBits2 0x0000000040000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR"
pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR = PipelineStageFlagBits2 0x0000000010000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI"
pattern PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI = PipelineStageFlagBits2 0x0000010000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI"
pattern PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI = PipelineStageFlagBits2 0x0000008000000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT"
pattern PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT = PipelineStageFlagBits2 0x0000000000100000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT"
pattern PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT = PipelineStageFlagBits2 0x0000000000080000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT"
pattern PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT = PipelineStageFlagBits2 0x0000000000800000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR = PipelineStageFlagBits2 0x0000000000200000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR"
pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR = PipelineStageFlagBits2 0x0000000002000000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineStageFlagBits2 0x0000000000400000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT"
pattern PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT = PipelineStageFlagBits2 0x0000000000020000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT"
pattern PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT = PipelineStageFlagBits2 0x0000000000040000

-- No documentation found for Nested "VkPipelineStageFlagBits2" "VK_PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT"
pattern PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT = PipelineStageFlagBits2 0x0000000001000000

conNamePipelineStageFlagBits2 :: String
conNamePipelineStageFlagBits2 = "PipelineStageFlagBits2"

enumPrefixPipelineStageFlagBits2 :: String
enumPrefixPipelineStageFlagBits2 = "PIPELINE_STAGE_2_"

showTablePipelineStageFlagBits2 :: [(PipelineStageFlagBits2, String)]
showTablePipelineStageFlagBits2 =
  [ (PIPELINE_STAGE_2_NONE, "NONE")
  ,
    ( PIPELINE_STAGE_2_TOP_OF_PIPE_BIT
    , "TOP_OF_PIPE_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_DRAW_INDIRECT_BIT
    , "DRAW_INDIRECT_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_VERTEX_INPUT_BIT
    , "VERTEX_INPUT_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_VERTEX_SHADER_BIT
    , "VERTEX_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT
    , "TESSELLATION_CONTROL_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT
    , "TESSELLATION_EVALUATION_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT
    , "GEOMETRY_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT
    , "FRAGMENT_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT
    , "EARLY_FRAGMENT_TESTS_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT
    , "LATE_FRAGMENT_TESTS_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT
    , "COLOR_ATTACHMENT_OUTPUT_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_COMPUTE_SHADER_BIT
    , "COMPUTE_SHADER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_ALL_TRANSFER_BIT
    , "ALL_TRANSFER_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT
    , "BOTTOM_OF_PIPE_BIT"
    )
  , (PIPELINE_STAGE_2_HOST_BIT, "HOST_BIT")
  ,
    ( PIPELINE_STAGE_2_ALL_GRAPHICS_BIT
    , "ALL_GRAPHICS_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_ALL_COMMANDS_BIT
    , "ALL_COMMANDS_BIT"
    )
  , (PIPELINE_STAGE_2_COPY_BIT, "COPY_BIT")
  ,
    ( PIPELINE_STAGE_2_RESOLVE_BIT
    , "RESOLVE_BIT"
    )
  , (PIPELINE_STAGE_2_BLIT_BIT, "BLIT_BIT")
  , (PIPELINE_STAGE_2_CLEAR_BIT, "CLEAR_BIT")
  ,
    ( PIPELINE_STAGE_2_INDEX_INPUT_BIT
    , "INDEX_INPUT_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT
    , "VERTEX_ATTRIBUTE_INPUT_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT
    , "PRE_RASTERIZATION_SHADERS_BIT"
    )
  ,
    ( PIPELINE_STAGE_2_MEMORY_DECOMPRESSION_BIT_EXT
    , "MEMORY_DECOMPRESSION_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_COPY_INDIRECT_BIT_KHR
    , "COPY_INDIRECT_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_2_DATA_GRAPH_BIT_ARM
    , "DATA_GRAPH_BIT_ARM"
    )
  ,
    ( PIPELINE_STAGE_2_CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV
    , "CONVERT_COOPERATIVE_VECTOR_MATRIX_BIT_NV"
    )
  ,
    ( PIPELINE_STAGE_2_OPTICAL_FLOW_BIT_NV
    , "OPTICAL_FLOW_BIT_NV"
    )
  ,
    ( PIPELINE_STAGE_2_CLUSTER_CULLING_SHADER_BIT_HUAWEI
    , "CLUSTER_CULLING_SHADER_BIT_HUAWEI"
    )
  ,
    ( PIPELINE_STAGE_2_MICROMAP_BUILD_BIT_EXT
    , "MICROMAP_BUILD_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR
    , "ACCELERATION_STRUCTURE_COPY_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI
    , "INVOCATION_MASK_BIT_HUAWEI"
    )
  ,
    ( PIPELINE_STAGE_2_SUBPASS_SHADER_BIT_HUAWEI
    , "SUBPASS_SHADER_BIT_HUAWEI"
    )
  ,
    ( PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT
    , "MESH_SHADER_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT
    , "TASK_SHADER_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT
    , "FRAGMENT_DENSITY_PROCESS_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR
    , "RAY_TRACING_SHADER_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
    , "ACCELERATION_STRUCTURE_BUILD_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_EXT
    , "COMMAND_PREPROCESS_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT
    , "CONDITIONAL_RENDERING_BIT_EXT"
    )
  ,
    ( PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT
    , "TRANSFORM_FEEDBACK_BIT_EXT"
    )
  ]

instance Show PipelineStageFlagBits2 where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineStageFlagBits2
      showTablePipelineStageFlagBits2
      conNamePipelineStageFlagBits2
      (\(PipelineStageFlagBits2 x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineStageFlagBits2 where
  readPrec =
    enumReadPrec
      enumPrefixPipelineStageFlagBits2
      showTablePipelineStageFlagBits2
      conNamePipelineStageFlagBits2
      PipelineStageFlagBits2
