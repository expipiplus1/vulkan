{-# language CPP #-}
-- No documentation found for Chapter "PipelineStageFlags2"
module Vulkan.Core13.Enums.PipelineStageFlags2  ( pattern PIPELINE_STAGE_2_NONE_KHR
                                                , pattern PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_TRANSFER_BIT
                                                , pattern PIPELINE_STAGE_2_TRANSFER_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_HOST_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_COPY_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_RESOLVE_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_BLIT_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_CLEAR_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR
                                                , pattern PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR
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
                                                                        , PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR
                                                                        , PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI
                                                                        , PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI
                                                                        , PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT
                                                                        , PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT
                                                                        , PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT
                                                                        , PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR
                                                                        , PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR
                                                                        , PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                        , PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV
                                                                        , PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT
                                                                        , PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT
                                                                        , ..
                                                                        )
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags64)
-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_NONE_KHR"
pattern PIPELINE_STAGE_2_NONE_KHR = PIPELINE_STAGE_2_NONE


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR"
pattern PIPELINE_STAGE_2_TOP_OF_PIPE_BIT_KHR = PIPELINE_STAGE_2_TOP_OF_PIPE_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR"
pattern PIPELINE_STAGE_2_DRAW_INDIRECT_BIT_KHR = PIPELINE_STAGE_2_DRAW_INDIRECT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR"
pattern PIPELINE_STAGE_2_VERTEX_INPUT_BIT_KHR = PIPELINE_STAGE_2_VERTEX_INPUT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_VERTEX_SHADER_BIT_KHR = PIPELINE_STAGE_2_VERTEX_SHADER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT_KHR = PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT_KHR = PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT_KHR = PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT_KHR = PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR"
pattern PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT_KHR = PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR"
pattern PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT_KHR = PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR"
pattern PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT_KHR = PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR"
pattern PIPELINE_STAGE_2_COMPUTE_SHADER_BIT_KHR = PIPELINE_STAGE_2_COMPUTE_SHADER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR"
pattern PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR = PIPELINE_STAGE_2_ALL_TRANSFER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TRANSFER_BIT"
pattern PIPELINE_STAGE_2_TRANSFER_BIT = PIPELINE_STAGE_2_ALL_TRANSFER_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_TRANSFER_BIT_KHR"
pattern PIPELINE_STAGE_2_TRANSFER_BIT_KHR = PIPELINE_STAGE_2_ALL_TRANSFER_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR"
pattern PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT_KHR = PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_HOST_BIT_KHR"
pattern PIPELINE_STAGE_2_HOST_BIT_KHR = PIPELINE_STAGE_2_HOST_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR"
pattern PIPELINE_STAGE_2_ALL_GRAPHICS_BIT_KHR = PIPELINE_STAGE_2_ALL_GRAPHICS_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR"
pattern PIPELINE_STAGE_2_ALL_COMMANDS_BIT_KHR = PIPELINE_STAGE_2_ALL_COMMANDS_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_COPY_BIT_KHR"
pattern PIPELINE_STAGE_2_COPY_BIT_KHR = PIPELINE_STAGE_2_COPY_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_RESOLVE_BIT_KHR"
pattern PIPELINE_STAGE_2_RESOLVE_BIT_KHR = PIPELINE_STAGE_2_RESOLVE_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_BLIT_BIT_KHR"
pattern PIPELINE_STAGE_2_BLIT_BIT_KHR = PIPELINE_STAGE_2_BLIT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_CLEAR_BIT_KHR"
pattern PIPELINE_STAGE_2_CLEAR_BIT_KHR = PIPELINE_STAGE_2_CLEAR_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR"
pattern PIPELINE_STAGE_2_INDEX_INPUT_BIT_KHR = PIPELINE_STAGE_2_INDEX_INPUT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR"
pattern PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT_KHR = PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR"
pattern PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT_KHR = PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT


type PipelineStageFlags2 = PipelineStageFlagBits2

-- | VkPipelineStageFlagBits2 - Pipeline stage flags for
-- VkPipelineStageFlags2
--
-- = Description
--
-- Note
--
-- The @TOP@ and @BOTTOM@ pipeline stages are deprecated, and applications
-- should prefer 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' and
-- 'PIPELINE_STAGE_2_NONE'.
--
-- Note
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>
newtype PipelineStageFlagBits2 = PipelineStageFlagBits2 Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'PIPELINE_STAGE_2_NONE' specifies no stages of execution.
pattern PIPELINE_STAGE_2_NONE                               = PipelineStageFlagBits2 0x0000000000000000
-- | 'PIPELINE_STAGE_2_TOP_OF_PIPE_BIT' is equivalent to
-- 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' with
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2' set to @0@ when
-- specified in the second synchronization scope, but equivalent to
-- 'PIPELINE_STAGE_2_NONE' in the first scope.
pattern PIPELINE_STAGE_2_TOP_OF_PIPE_BIT                    = PipelineStageFlagBits2 0x0000000000000001
-- | 'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT' specifies the stage of the pipeline
-- where indirect command parameters are consumed. This stage also includes
-- reading commands written by
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
pattern PIPELINE_STAGE_2_DRAW_INDIRECT_BIT                  = PipelineStageFlagBits2 0x0000000000000002
-- | 'PIPELINE_STAGE_2_VERTEX_INPUT_BIT' is equivalent to the logical OR of:
--
-- -   'PIPELINE_STAGE_2_INDEX_INPUT_BIT'
--
-- -   'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT'
pattern PIPELINE_STAGE_2_VERTEX_INPUT_BIT                   = PipelineStageFlagBits2 0x0000000000000004
-- | 'PIPELINE_STAGE_2_VERTEX_SHADER_BIT' specifies the vertex shader stage.
pattern PIPELINE_STAGE_2_VERTEX_SHADER_BIT                  = PipelineStageFlagBits2 0x0000000000000008
-- | 'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT' specifies the
-- tessellation control shader stage.
pattern PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT    = PipelineStageFlagBits2 0x0000000000000010
-- | 'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT' specifies the
-- tessellation evaluation shader stage.
pattern PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT = PipelineStageFlagBits2 0x0000000000000020
-- | 'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT' specifies the geometry shader
-- stage.
pattern PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT                = PipelineStageFlagBits2 0x0000000000000040
-- | 'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT' specifies the fragment shader
-- stage.
pattern PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT                = PipelineStageFlagBits2 0x0000000000000080
-- | 'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT' specifies the stage of the
-- pipeline where early fragment tests (depth and stencil tests before
-- fragment shading) are performed. This stage also includes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT           = PipelineStageFlagBits2 0x0000000000000100
-- | 'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT' specifies the stage of the
-- pipeline where late fragment tests (depth and stencil tests after
-- fragment shading) are performed. This stage also includes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass store operations>
-- for framebuffer attachments with a depth\/stencil format.
pattern PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT            = PipelineStageFlagBits2 0x0000000000000200
-- | 'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT' specifies the stage of
-- the pipeline after blending where the final color values are output from
-- the pipeline. This stage also includes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>,
-- multisample resolve operations for framebuffer attachments with a color
-- or depth\/stencil format, and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'.
pattern PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT        = PipelineStageFlagBits2 0x0000000000000400
-- | 'PIPELINE_STAGE_2_COMPUTE_SHADER_BIT' specifies the compute shader
-- stage.
pattern PIPELINE_STAGE_2_COMPUTE_SHADER_BIT                 = PipelineStageFlagBits2 0x0000000000000800
-- | 'PIPELINE_STAGE_2_ALL_TRANSFER_BIT' is equivalent to specifying all of:
--
-- -   'PIPELINE_STAGE_2_COPY_BIT'
--
-- -   'PIPELINE_STAGE_2_BLIT_BIT'
--
-- -   'PIPELINE_STAGE_2_RESOLVE_BIT'
--
-- -   'PIPELINE_STAGE_2_CLEAR_BIT'
--
-- -   'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
pattern PIPELINE_STAGE_2_ALL_TRANSFER_BIT                   = PipelineStageFlagBits2 0x0000000000001000
-- | 'PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT' is equivalent to
-- 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' with
-- 'Vulkan.Core13.Enums.AccessFlags2.AccessFlags2' set to @0@ when
-- specified in the first synchronization scope, but equivalent to
-- 'PIPELINE_STAGE_2_NONE' in the second scope.
pattern PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT                 = PipelineStageFlagBits2 0x0000000000002000
-- | 'PIPELINE_STAGE_2_HOST_BIT' specifies a pseudo-stage indicating
-- execution on the host of reads\/writes of device memory. This stage is
-- not invoked by any commands recorded in a command buffer.
pattern PIPELINE_STAGE_2_HOST_BIT                           = PipelineStageFlagBits2 0x0000000000004000
-- | 'PIPELINE_STAGE_2_ALL_GRAPHICS_BIT' specifies the execution of all
-- graphics pipeline stages, and is equivalent to the logical OR of:
--
-- -   'PIPELINE_STAGE_2_DRAW_INDIRECT_BIT'
--
-- -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_VERTEX_INPUT_BIT'
--
-- -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT'
--
-- -   'PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT'
--
-- -   'PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
--
-- -   'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
--
-- -   'Vulkan.Extensions.VK_KHR_synchronization2.PIPELINE_STAGE_2_SHADING_RATE_IMAGE_BIT_NV'
--
-- -   'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
pattern PIPELINE_STAGE_2_ALL_GRAPHICS_BIT                   = PipelineStageFlagBits2 0x0000000000008000
-- | 'PIPELINE_STAGE_2_ALL_COMMANDS_BIT' specifies all operations performed
-- by all commands supported on the queue it is used with.
pattern PIPELINE_STAGE_2_ALL_COMMANDS_BIT                   = PipelineStageFlagBits2 0x0000000000010000
-- | 'PIPELINE_STAGE_2_COPY_BIT' specifies the execution of all
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy commands>,
-- including 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'.
pattern PIPELINE_STAGE_2_COPY_BIT                           = PipelineStageFlagBits2 0x0000000100000000
-- | 'PIPELINE_STAGE_2_RESOLVE_BIT' specifies the execution of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage'.
pattern PIPELINE_STAGE_2_RESOLVE_BIT                        = PipelineStageFlagBits2 0x0000000200000000
-- | 'PIPELINE_STAGE_2_BLIT_BIT' specifies the execution of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
pattern PIPELINE_STAGE_2_BLIT_BIT                           = PipelineStageFlagBits2 0x0000000400000000
-- | 'PIPELINE_STAGE_2_CLEAR_BIT' specifies the execution of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears clear commands>,
-- with the exception of
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments'.
pattern PIPELINE_STAGE_2_CLEAR_BIT                          = PipelineStageFlagBits2 0x0000000800000000
-- | 'PIPELINE_STAGE_2_INDEX_INPUT_BIT' specifies the stage of the pipeline
-- where index buffers are consumed.
pattern PIPELINE_STAGE_2_INDEX_INPUT_BIT                    = PipelineStageFlagBits2 0x0000001000000000
-- | 'PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT' specifies the stage of the
-- pipeline where vertex buffers are consumed.
pattern PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT         = PipelineStageFlagBits2 0x0000002000000000
-- | 'PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT' is equivalent to
-- specifying all supported
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-graphics-subsets-pre-rasterization pre-rasterization shader stages>:
--
-- -   'PIPELINE_STAGE_2_VERTEX_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT'
--
-- -   'PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT'
--
-- -   'PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT'
pattern PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT      = PipelineStageFlagBits2 0x0000004000000000
-- | 'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR' specifies the
-- execution of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure-copying acceleration structure copy commands>.
pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR = PipelineStageFlagBits2 0x0000000010000000
-- | 'PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI' specifies the stage of the
-- pipeline where the invocation mask image is read by the implementation
-- to optimize the ray dispatch.
pattern PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI         = PipelineStageFlagBits2 0x0000010000000000
-- | 'PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI' specifies the subpass
-- shading shader stage.
pattern PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI         = PipelineStageFlagBits2 0x0000008000000000
-- | 'PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT' specifies the mesh shader stage.
pattern PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT                = PipelineStageFlagBits2 0x0000000000100000
-- | 'PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT' specifies the task shader stage.
pattern PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT                = PipelineStageFlagBits2 0x0000000000080000
-- | 'PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT' specifies the stage
-- of the pipeline where the fragment density map is read to
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragmentdensitymapops generate the fragment areas>.
pattern PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT   = PipelineStageFlagBits2 0x0000000000800000
-- | 'PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR' specifies the execution of
-- the ray tracing shader stages.
pattern PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR         = PipelineStageFlagBits2 0x0000000000200000
-- | 'PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' specifies the
-- execution of
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure acceleration structure commands>.
pattern PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR = PipelineStageFlagBits2 0x0000000002000000
-- | 'PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' specifies
-- the stage of the pipeline where the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
-- or
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
-- is read to determine the fragment shading rate for portions of a
-- rasterized primitive.
pattern PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineStageFlagBits2 0x0000000000400000
-- | 'PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV' specifies the stage of the
-- pipeline where device-side generation of commands via
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'
-- is handled.
pattern PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV          = PipelineStageFlagBits2 0x0000000000020000
-- | 'PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT' specifies the stage of
-- the pipeline where the predicate of conditional rendering is consumed.
pattern PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT      = PipelineStageFlagBits2 0x0000000000040000
-- | 'PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT' specifies the stage of the
-- pipeline where vertex attribute output values are written to the
-- transform feedback buffers.
pattern PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT         = PipelineStageFlagBits2 0x0000000001000000

conNamePipelineStageFlagBits2 :: String
conNamePipelineStageFlagBits2 = "PipelineStageFlagBits2"

enumPrefixPipelineStageFlagBits2 :: String
enumPrefixPipelineStageFlagBits2 = "PIPELINE_STAGE_2_"

showTablePipelineStageFlagBits2 :: [(PipelineStageFlagBits2, String)]
showTablePipelineStageFlagBits2 =
  [ (PIPELINE_STAGE_2_NONE                              , "NONE")
  , (PIPELINE_STAGE_2_TOP_OF_PIPE_BIT                   , "TOP_OF_PIPE_BIT")
  , (PIPELINE_STAGE_2_DRAW_INDIRECT_BIT                 , "DRAW_INDIRECT_BIT")
  , (PIPELINE_STAGE_2_VERTEX_INPUT_BIT                  , "VERTEX_INPUT_BIT")
  , (PIPELINE_STAGE_2_VERTEX_SHADER_BIT                 , "VERTEX_SHADER_BIT")
  , (PIPELINE_STAGE_2_TESSELLATION_CONTROL_SHADER_BIT   , "TESSELLATION_CONTROL_SHADER_BIT")
  , (PIPELINE_STAGE_2_TESSELLATION_EVALUATION_SHADER_BIT, "TESSELLATION_EVALUATION_SHADER_BIT")
  , (PIPELINE_STAGE_2_GEOMETRY_SHADER_BIT               , "GEOMETRY_SHADER_BIT")
  , (PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT               , "FRAGMENT_SHADER_BIT")
  , (PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT          , "EARLY_FRAGMENT_TESTS_BIT")
  , (PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT           , "LATE_FRAGMENT_TESTS_BIT")
  , (PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT       , "COLOR_ATTACHMENT_OUTPUT_BIT")
  , (PIPELINE_STAGE_2_COMPUTE_SHADER_BIT                , "COMPUTE_SHADER_BIT")
  , (PIPELINE_STAGE_2_ALL_TRANSFER_BIT                  , "ALL_TRANSFER_BIT")
  , (PIPELINE_STAGE_2_BOTTOM_OF_PIPE_BIT                , "BOTTOM_OF_PIPE_BIT")
  , (PIPELINE_STAGE_2_HOST_BIT                          , "HOST_BIT")
  , (PIPELINE_STAGE_2_ALL_GRAPHICS_BIT                  , "ALL_GRAPHICS_BIT")
  , (PIPELINE_STAGE_2_ALL_COMMANDS_BIT                  , "ALL_COMMANDS_BIT")
  , (PIPELINE_STAGE_2_COPY_BIT                          , "COPY_BIT")
  , (PIPELINE_STAGE_2_RESOLVE_BIT                       , "RESOLVE_BIT")
  , (PIPELINE_STAGE_2_BLIT_BIT                          , "BLIT_BIT")
  , (PIPELINE_STAGE_2_CLEAR_BIT                         , "CLEAR_BIT")
  , (PIPELINE_STAGE_2_INDEX_INPUT_BIT                   , "INDEX_INPUT_BIT")
  , (PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT        , "VERTEX_ATTRIBUTE_INPUT_BIT")
  , (PIPELINE_STAGE_2_PRE_RASTERIZATION_SHADERS_BIT     , "PRE_RASTERIZATION_SHADERS_BIT")
  , (PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR, "ACCELERATION_STRUCTURE_COPY_BIT_KHR")
  , (PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI        , "INVOCATION_MASK_BIT_HUAWEI")
  , (PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI        , "SUBPASS_SHADING_BIT_HUAWEI")
  , (PIPELINE_STAGE_2_MESH_SHADER_BIT_EXT               , "MESH_SHADER_BIT_EXT")
  , (PIPELINE_STAGE_2_TASK_SHADER_BIT_EXT               , "TASK_SHADER_BIT_EXT")
  , (PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT  , "FRAGMENT_DENSITY_PROCESS_BIT_EXT")
  , (PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR        , "RAY_TRACING_SHADER_BIT_KHR")
  , (PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR, "ACCELERATION_STRUCTURE_BUILD_BIT_KHR")
  , (PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR, "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR")
  , (PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV         , "COMMAND_PREPROCESS_BIT_NV")
  , (PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT     , "CONDITIONAL_RENDERING_BIT_EXT")
  , (PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT        , "TRANSFORM_FEEDBACK_BIT_EXT")
  ]

instance Show PipelineStageFlagBits2 where
  showsPrec = enumShowsPrec enumPrefixPipelineStageFlagBits2
                            showTablePipelineStageFlagBits2
                            conNamePipelineStageFlagBits2
                            (\(PipelineStageFlagBits2 x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read PipelineStageFlagBits2 where
  readPrec = enumReadPrec enumPrefixPipelineStageFlagBits2
                          showTablePipelineStageFlagBits2
                          conNamePipelineStageFlagBits2
                          PipelineStageFlagBits2

