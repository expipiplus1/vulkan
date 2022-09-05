{-# language CPP #-}
-- No documentation found for Chapter "AccessFlags2"
module Vulkan.Core13.Enums.AccessFlags2  ( pattern ACCESS_2_NONE_KHR
                                         , pattern ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR
                                         , pattern ACCESS_2_INDEX_READ_BIT_KHR
                                         , pattern ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR
                                         , pattern ACCESS_2_UNIFORM_READ_BIT_KHR
                                         , pattern ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR
                                         , pattern ACCESS_2_SHADER_READ_BIT_KHR
                                         , pattern ACCESS_2_SHADER_WRITE_BIT_KHR
                                         , pattern ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR
                                         , pattern ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR
                                         , pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR
                                         , pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR
                                         , pattern ACCESS_2_TRANSFER_READ_BIT_KHR
                                         , pattern ACCESS_2_TRANSFER_WRITE_BIT_KHR
                                         , pattern ACCESS_2_HOST_READ_BIT_KHR
                                         , pattern ACCESS_2_HOST_WRITE_BIT_KHR
                                         , pattern ACCESS_2_MEMORY_READ_BIT_KHR
                                         , pattern ACCESS_2_MEMORY_WRITE_BIT_KHR
                                         , pattern ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR
                                         , pattern ACCESS_2_SHADER_STORAGE_READ_BIT_KHR
                                         , pattern ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR
                                         , AccessFlags2
                                         , AccessFlagBits2( ACCESS_2_NONE
                                                          , ACCESS_2_INDIRECT_COMMAND_READ_BIT
                                                          , ACCESS_2_INDEX_READ_BIT
                                                          , ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT
                                                          , ACCESS_2_UNIFORM_READ_BIT
                                                          , ACCESS_2_INPUT_ATTACHMENT_READ_BIT
                                                          , ACCESS_2_SHADER_READ_BIT
                                                          , ACCESS_2_SHADER_WRITE_BIT
                                                          , ACCESS_2_COLOR_ATTACHMENT_READ_BIT
                                                          , ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT
                                                          , ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT
                                                          , ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
                                                          , ACCESS_2_TRANSFER_READ_BIT
                                                          , ACCESS_2_TRANSFER_WRITE_BIT
                                                          , ACCESS_2_HOST_READ_BIT
                                                          , ACCESS_2_HOST_WRITE_BIT
                                                          , ACCESS_2_MEMORY_READ_BIT
                                                          , ACCESS_2_MEMORY_WRITE_BIT
                                                          , ACCESS_2_SHADER_SAMPLED_READ_BIT
                                                          , ACCESS_2_SHADER_STORAGE_READ_BIT
                                                          , ACCESS_2_SHADER_STORAGE_WRITE_BIT
                                                          , ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR
                                                          , ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI
                                                          , ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
                                                          , ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
                                                          , ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR
                                                          , ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR
                                                          , ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR
                                                          , ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV
                                                          , ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV
                                                          , ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT
                                                          , ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
                                                          , ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
                                                          , ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
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
-- No documentation found for TopLevel "VK_ACCESS_2_NONE_KHR"
pattern ACCESS_2_NONE_KHR = ACCESS_2_NONE


-- No documentation found for TopLevel "VK_ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR"
pattern ACCESS_2_INDIRECT_COMMAND_READ_BIT_KHR = ACCESS_2_INDIRECT_COMMAND_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_INDEX_READ_BIT_KHR"
pattern ACCESS_2_INDEX_READ_BIT_KHR = ACCESS_2_INDEX_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR"
pattern ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT_KHR = ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_UNIFORM_READ_BIT_KHR"
pattern ACCESS_2_UNIFORM_READ_BIT_KHR = ACCESS_2_UNIFORM_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR"
pattern ACCESS_2_INPUT_ATTACHMENT_READ_BIT_KHR = ACCESS_2_INPUT_ATTACHMENT_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_SHADER_READ_BIT_KHR"
pattern ACCESS_2_SHADER_READ_BIT_KHR = ACCESS_2_SHADER_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_SHADER_WRITE_BIT_KHR"
pattern ACCESS_2_SHADER_WRITE_BIT_KHR = ACCESS_2_SHADER_WRITE_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR"
pattern ACCESS_2_COLOR_ATTACHMENT_READ_BIT_KHR = ACCESS_2_COLOR_ATTACHMENT_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR"
pattern ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT_KHR = ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR"
pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT_KHR = ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR"
pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT_KHR = ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_TRANSFER_READ_BIT_KHR"
pattern ACCESS_2_TRANSFER_READ_BIT_KHR = ACCESS_2_TRANSFER_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_TRANSFER_WRITE_BIT_KHR"
pattern ACCESS_2_TRANSFER_WRITE_BIT_KHR = ACCESS_2_TRANSFER_WRITE_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_HOST_READ_BIT_KHR"
pattern ACCESS_2_HOST_READ_BIT_KHR = ACCESS_2_HOST_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_HOST_WRITE_BIT_KHR"
pattern ACCESS_2_HOST_WRITE_BIT_KHR = ACCESS_2_HOST_WRITE_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_MEMORY_READ_BIT_KHR"
pattern ACCESS_2_MEMORY_READ_BIT_KHR = ACCESS_2_MEMORY_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_MEMORY_WRITE_BIT_KHR"
pattern ACCESS_2_MEMORY_WRITE_BIT_KHR = ACCESS_2_MEMORY_WRITE_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR"
pattern ACCESS_2_SHADER_SAMPLED_READ_BIT_KHR = ACCESS_2_SHADER_SAMPLED_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_SHADER_STORAGE_READ_BIT_KHR"
pattern ACCESS_2_SHADER_STORAGE_READ_BIT_KHR = ACCESS_2_SHADER_STORAGE_READ_BIT


-- No documentation found for TopLevel "VK_ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR"
pattern ACCESS_2_SHADER_STORAGE_WRITE_BIT_KHR = ACCESS_2_SHADER_STORAGE_WRITE_BIT


type AccessFlags2 = AccessFlagBits2

-- | VkAccessFlagBits2 - Access flags for VkAccessFlags2
--
-- = Description
--
-- Note
--
-- In situations where an application wishes to select all access types for
-- a given set of pipeline stages, 'ACCESS_2_MEMORY_READ_BIT' or
-- 'ACCESS_2_MEMORY_WRITE_BIT' can be used. This is particularly useful
-- when specifying stages that only have a single access type.
--
-- Note
--
-- The 'AccessFlags2' bitmask goes beyond the 31 individual bit flags
-- allowable within a C99 enum, which is how
-- 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits' is defined. The
-- first 31 values are common to both, and are interchangeable.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>
newtype AccessFlagBits2 = AccessFlagBits2 Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'ACCESS_2_NONE' specifies no accesses.
pattern ACCESS_2_NONE                               = AccessFlagBits2 0x0000000000000000
-- | 'ACCESS_2_INDIRECT_COMMAND_READ_BIT' specifies read access to command
-- data read from indirect buffers as part of an indirect build, trace,
-- drawing or dispatch command. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DRAW_INDIRECT_BIT'
-- pipeline stage.
pattern ACCESS_2_INDIRECT_COMMAND_READ_BIT          = AccessFlagBits2 0x0000000000000001
-- | 'ACCESS_2_INDEX_READ_BIT' specifies read access to an index buffer as
-- part of an indexed drawing command, bound by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'. Such access
-- occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INDEX_INPUT_BIT'
-- pipeline stage.
pattern ACCESS_2_INDEX_READ_BIT                     = AccessFlagBits2 0x0000000000000002
-- | 'ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT' specifies read access to a vertex
-- buffer as part of a drawing command, bound by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'. Such access
-- occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_VERTEX_ATTRIBUTE_INPUT_BIT'
-- pipeline stage.
pattern ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT          = AccessFlagBits2 0x0000000000000004
-- | 'ACCESS_2_UNIFORM_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer>
-- in any shader pipeline stage.
pattern ACCESS_2_UNIFORM_READ_BIT                   = AccessFlagBits2 0x0000000000000008
-- | 'ACCESS_2_INPUT_ATTACHMENT_READ_BIT' specifies read access to an
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass input attachment>
-- within a render pass during subpass shading or fragment shading. Such
-- access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_SUBPASS_SHADING_BIT_HUAWEI'
-- or
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADER_BIT'
-- pipeline stage.
pattern ACCESS_2_INPUT_ATTACHMENT_READ_BIT          = AccessFlagBits2 0x0000000000000010
-- | 'ACCESS_2_SHADER_READ_BIT' is equivalent to the logical OR of:
--
-- -   'ACCESS_2_UNIFORM_READ_BIT'
--
-- -   'ACCESS_2_SHADER_SAMPLED_READ_BIT'
--
-- -   'ACCESS_2_SHADER_STORAGE_READ_BIT'
--
-- -   'ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR'
pattern ACCESS_2_SHADER_READ_BIT                    = AccessFlagBits2 0x0000000000000020
-- | 'ACCESS_2_SHADER_WRITE_BIT' is equivalent to
-- 'ACCESS_2_SHADER_STORAGE_WRITE_BIT'.
pattern ACCESS_2_SHADER_WRITE_BIT                   = AccessFlagBits2 0x0000000000000040
-- | 'ACCESS_2_COLOR_ATTACHMENT_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass color attachment>,
-- such as via
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-blending blending>,
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-logicop logic operations>,
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- It does not include
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
pattern ACCESS_2_COLOR_ATTACHMENT_READ_BIT          = AccessFlagBits2 0x0000000000000080
-- | 'ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass color, resolve, or depth\/stencil resolve attachment>
-- during a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass render pass>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
pattern ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT         = AccessFlagBits2 0x0000000000000100
-- | 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT'
-- or
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT'
-- pipeline stages.
pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT  = AccessFlagBits2 0x0000000000000200
-- | 'ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT' specifies write access to
-- a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_EARLY_FRAGMENT_TESTS_BIT'
-- or
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_LATE_FRAGMENT_TESTS_BIT'
-- pipeline stages.
pattern ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = AccessFlagBits2 0x0000000000000400
-- | 'ACCESS_2_TRANSFER_READ_BIT' specifies read access to an image or buffer
-- in a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy>
-- operation. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT', or
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT'
-- pipeline stages.
pattern ACCESS_2_TRANSFER_READ_BIT                  = AccessFlagBits2 0x0000000000000800
-- | 'ACCESS_2_TRANSFER_WRITE_BIT' specifies write access to an image or
-- buffer in a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#clears clear>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy>
-- operation. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COPY_BIT',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_BLIT_BIT',
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CLEAR_BIT', or
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RESOLVE_BIT'
-- pipeline stages.
pattern ACCESS_2_TRANSFER_WRITE_BIT                 = AccessFlagBits2 0x0000000000001000
-- | 'ACCESS_2_HOST_READ_BIT' specifies read access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
-- pipeline stage.
pattern ACCESS_2_HOST_READ_BIT                      = AccessFlagBits2 0x0000000000002000
-- | 'ACCESS_2_HOST_WRITE_BIT' specifies write access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_HOST_BIT'
-- pipeline stage.
pattern ACCESS_2_HOST_WRITE_BIT                     = AccessFlagBits2 0x0000000000004000
-- | 'ACCESS_2_MEMORY_READ_BIT' specifies all read accesses. It is always
-- valid in any access mask, and is treated as equivalent to setting all
-- @READ@ access flags that are valid where it is used.
pattern ACCESS_2_MEMORY_READ_BIT                    = AccessFlagBits2 0x0000000000008000
-- | 'ACCESS_2_MEMORY_WRITE_BIT' specifies all write accesses. It is always
-- valid in any access mask, and is treated as equivalent to setting all
-- @WRITE@ access flags that are valid where it is used.
pattern ACCESS_2_MEMORY_WRITE_BIT                   = AccessFlagBits2 0x0000000000010000
-- | 'ACCESS_2_SHADER_SAMPLED_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_SAMPLED_READ_BIT            = AccessFlagBits2 0x0000000100000000
-- | 'ACCESS_2_SHADER_STORAGE_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage image>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_STORAGE_READ_BIT            = AccessFlagBits2 0x0000000200000000
-- | 'ACCESS_2_SHADER_STORAGE_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage image>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_STORAGE_WRITE_BIT           = AccessFlagBits2 0x0000000400000000
-- | 'ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shader-binding-table shader binding table>
-- in any shader pipeline stage.
pattern ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR  = AccessFlagBits2 0x0000010000000000
-- | 'ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI' specifies read access to a
-- invocation mask image in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_INVOCATION_MASK_BIT_HUAWEI'
-- pipeline stage.
pattern ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI    = AccessFlagBits2 0x0000008000000000
-- | 'ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT' specifies read
-- access to
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass color attachments>,
-- including
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COLOR_ATTACHMENT_OUTPUT_BIT'
-- pipeline stage.
pattern ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = AccessFlagBits2 0x0000000000080000
-- | 'ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>
-- during dynamic
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#fragmentdensitymapops fragment density map operations>.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_DENSITY_PROCESS_BIT_EXT'
-- pipeline stage.
pattern ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT  = AccessFlagBits2 0x0000000001000000
-- | 'ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR' specifies write access
-- to an acceleration structure or
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure-scratch acceleration structure scratch buffer>
-- as part of a build or copy command. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- pipeline stage.
pattern ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR = AccessFlagBits2 0x0000000000400000
-- | 'ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR' specifies read access to
-- an acceleration structure as part of a trace, build, or copy command, or
-- to an
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure-scratch acceleration structure scratch buffer>
-- as part of a build command. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_RAY_TRACING_SHADER_BIT_KHR'
-- pipeline stage or
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- pipeline stage.
pattern ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR = AccessFlagBits2 0x0000000000200000
-- | 'ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR' specifies read
-- access to a fragment shading rate attachment during rasterization. Such
-- access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
-- pipeline stage.
pattern ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR = AccessFlagBits2 0x0000000000800000
-- | 'ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV' specifies writes to the
-- target command buffer preprocess outputs. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
-- pipeline stage.
pattern ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV    = AccessFlagBits2 0x0000000000040000
-- | 'ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV' specifies reads from buffer
-- inputs to
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
-- Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_COMMAND_PREPROCESS_BIT_NV'
-- pipeline stage.
pattern ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV     = AccessFlagBits2 0x0000000000020000
-- | 'ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT' specifies read access to a
-- predicate as part of conditional rendering. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
-- pipeline stage.
pattern ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT = AccessFlagBits2 0x0000000000100000
-- | 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT' specifies write
-- access to a transform feedback counter buffer which is written when
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'
-- executes. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
-- pipeline stage.
pattern ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT = AccessFlagBits2 0x0000000008000000
-- | 'ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT' specifies read access
-- to a transform feedback counter buffer which is read when
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
-- executes. Such access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
-- pipeline stage.
pattern ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT = AccessFlagBits2 0x0000000004000000
-- | 'ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT' specifies write access to a
-- transform feedback buffer made when transform feedback is active. Such
-- access occurs in the
-- 'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_TRANSFORM_FEEDBACK_BIT_EXT'
-- pipeline stage.
pattern ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT   = AccessFlagBits2 0x0000000002000000

conNameAccessFlagBits2 :: String
conNameAccessFlagBits2 = "AccessFlagBits2"

enumPrefixAccessFlagBits2 :: String
enumPrefixAccessFlagBits2 = "ACCESS_2_"

showTableAccessFlagBits2 :: [(AccessFlagBits2, String)]
showTableAccessFlagBits2 =
  [ (ACCESS_2_NONE                              , "NONE")
  , (ACCESS_2_INDIRECT_COMMAND_READ_BIT         , "INDIRECT_COMMAND_READ_BIT")
  , (ACCESS_2_INDEX_READ_BIT                    , "INDEX_READ_BIT")
  , (ACCESS_2_VERTEX_ATTRIBUTE_READ_BIT         , "VERTEX_ATTRIBUTE_READ_BIT")
  , (ACCESS_2_UNIFORM_READ_BIT                  , "UNIFORM_READ_BIT")
  , (ACCESS_2_INPUT_ATTACHMENT_READ_BIT         , "INPUT_ATTACHMENT_READ_BIT")
  , (ACCESS_2_SHADER_READ_BIT                   , "SHADER_READ_BIT")
  , (ACCESS_2_SHADER_WRITE_BIT                  , "SHADER_WRITE_BIT")
  , (ACCESS_2_COLOR_ATTACHMENT_READ_BIT         , "COLOR_ATTACHMENT_READ_BIT")
  , (ACCESS_2_COLOR_ATTACHMENT_WRITE_BIT        , "COLOR_ATTACHMENT_WRITE_BIT")
  , (ACCESS_2_DEPTH_STENCIL_ATTACHMENT_READ_BIT , "DEPTH_STENCIL_ATTACHMENT_READ_BIT")
  , (ACCESS_2_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT, "DEPTH_STENCIL_ATTACHMENT_WRITE_BIT")
  , (ACCESS_2_TRANSFER_READ_BIT                 , "TRANSFER_READ_BIT")
  , (ACCESS_2_TRANSFER_WRITE_BIT                , "TRANSFER_WRITE_BIT")
  , (ACCESS_2_HOST_READ_BIT                     , "HOST_READ_BIT")
  , (ACCESS_2_HOST_WRITE_BIT                    , "HOST_WRITE_BIT")
  , (ACCESS_2_MEMORY_READ_BIT                   , "MEMORY_READ_BIT")
  , (ACCESS_2_MEMORY_WRITE_BIT                  , "MEMORY_WRITE_BIT")
  , (ACCESS_2_SHADER_SAMPLED_READ_BIT           , "SHADER_SAMPLED_READ_BIT")
  , (ACCESS_2_SHADER_STORAGE_READ_BIT           , "SHADER_STORAGE_READ_BIT")
  , (ACCESS_2_SHADER_STORAGE_WRITE_BIT          , "SHADER_STORAGE_WRITE_BIT")
  , (ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR , "SHADER_BINDING_TABLE_READ_BIT_KHR")
  , (ACCESS_2_INVOCATION_MASK_READ_BIT_HUAWEI   , "INVOCATION_MASK_READ_BIT_HUAWEI")
  , (ACCESS_2_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT, "COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT")
  , (ACCESS_2_FRAGMENT_DENSITY_MAP_READ_BIT_EXT , "FRAGMENT_DENSITY_MAP_READ_BIT_EXT")
  , (ACCESS_2_ACCELERATION_STRUCTURE_WRITE_BIT_KHR, "ACCELERATION_STRUCTURE_WRITE_BIT_KHR")
  , (ACCESS_2_ACCELERATION_STRUCTURE_READ_BIT_KHR, "ACCELERATION_STRUCTURE_READ_BIT_KHR")
  , (ACCESS_2_FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR, "FRAGMENT_SHADING_RATE_ATTACHMENT_READ_BIT_KHR")
  , (ACCESS_2_COMMAND_PREPROCESS_WRITE_BIT_NV   , "COMMAND_PREPROCESS_WRITE_BIT_NV")
  , (ACCESS_2_COMMAND_PREPROCESS_READ_BIT_NV    , "COMMAND_PREPROCESS_READ_BIT_NV")
  , (ACCESS_2_CONDITIONAL_RENDERING_READ_BIT_EXT, "CONDITIONAL_RENDERING_READ_BIT_EXT")
  , (ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT, "TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT")
  , (ACCESS_2_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT, "TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT")
  , (ACCESS_2_TRANSFORM_FEEDBACK_WRITE_BIT_EXT  , "TRANSFORM_FEEDBACK_WRITE_BIT_EXT")
  ]

instance Show AccessFlagBits2 where
  showsPrec = enumShowsPrec enumPrefixAccessFlagBits2
                            showTableAccessFlagBits2
                            conNameAccessFlagBits2
                            (\(AccessFlagBits2 x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read AccessFlagBits2 where
  readPrec = enumReadPrec enumPrefixAccessFlagBits2 showTableAccessFlagBits2 conNameAccessFlagBits2 AccessFlagBits2

