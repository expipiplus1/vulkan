{-# language CPP #-}
module Vulkan.Core10.Enums.AccessFlagBits  ( AccessFlagBits( ACCESS_INDIRECT_COMMAND_READ_BIT
                                                           , ACCESS_INDEX_READ_BIT
                                                           , ACCESS_VERTEX_ATTRIBUTE_READ_BIT
                                                           , ACCESS_UNIFORM_READ_BIT
                                                           , ACCESS_INPUT_ATTACHMENT_READ_BIT
                                                           , ACCESS_SHADER_READ_BIT
                                                           , ACCESS_SHADER_WRITE_BIT
                                                           , ACCESS_COLOR_ATTACHMENT_READ_BIT
                                                           , ACCESS_COLOR_ATTACHMENT_WRITE_BIT
                                                           , ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT
                                                           , ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT
                                                           , ACCESS_TRANSFER_READ_BIT
                                                           , ACCESS_TRANSFER_WRITE_BIT
                                                           , ACCESS_HOST_READ_BIT
                                                           , ACCESS_HOST_WRITE_BIT
                                                           , ACCESS_MEMORY_READ_BIT
                                                           , ACCESS_MEMORY_WRITE_BIT
                                                           , ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV
                                                           , ACCESS_COMMAND_PREPROCESS_READ_BIT_NV
                                                           , ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT
                                                           , ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV
                                                           , ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR
                                                           , ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR
                                                           , ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT
                                                           , ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT
                                                           , ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT
                                                           , ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT
                                                           , ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT
                                                           , ..
                                                           )
                                           , AccessFlags
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
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkAccessFlagBits - Bitmask specifying memory access types that will
-- participate in a memory dependency
--
-- = Description
--
-- Certain access types are only performed by a subset of pipeline stages.
-- Any synchronization command that takes both stage masks and access masks
-- uses both to define the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies-access-scopes access scopes>
-- - only the specified access types performed by the specified stages are
-- included in the access scope. An application /must/ not specify an
-- access flag in a synchronization command if it does not include a
-- pipeline stage in the corresponding stage mask that is able to perform
-- accesses of that type. The following table lists, for each access flag,
-- which pipeline stages /can/ perform that type of access.
--
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | Access flag                                        | Supported pipeline stages                                                                       |
-- +====================================================+=================================================================================================+
-- | 'ACCESS_INDIRECT_COMMAND_READ_BIT'                 | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_DRAW_INDIRECT_BIT'                    |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_INDEX_READ_BIT'                            | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_INPUT_BIT'                     |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_VERTEX_ATTRIBUTE_READ_BIT'                 | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_INPUT_BIT'                     |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_UNIFORM_READ_BIT'                          | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV',                  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV',                  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR',          |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_SHADER_BIT',                   |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT',     |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT',  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT',                 |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADER_BIT', or              |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMPUTE_SHADER_BIT'                   |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_SHADER_READ_BIT'                           | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV',                  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV',                  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR',          |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_SHADER_BIT',                   |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT',     |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT',  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT',                 |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADER_BIT', or              |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMPUTE_SHADER_BIT'                   |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_SHADER_WRITE_BIT'                          | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV',                  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV',                  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR',          |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_SHADER_BIT',                   |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT',     |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT',  |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_GEOMETRY_SHADER_BIT',                 |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADER_BIT', or              |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMPUTE_SHADER_BIT'                   |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_INPUT_ATTACHMENT_READ_BIT'                 | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_SHADER_BIT'                  |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_COLOR_ATTACHMENT_READ_BIT'                 | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'          |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_COLOR_ATTACHMENT_WRITE_BIT'                | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'          |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT'         | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT', or         |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'              |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT'        | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT', or         |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_LATE_FRAGMENT_TESTS_BIT'              |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_TRANSFER_READ_BIT'                         | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'                         |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_TRANSFER_WRITE_BIT'                        | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFER_BIT'                         |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_HOST_READ_BIT'                             | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'                             |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_HOST_WRITE_BIT'                            | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_HOST_BIT'                             |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_MEMORY_READ_BIT'                           | Any                                                                                             |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_MEMORY_WRITE_BIT'                          | Any                                                                                             |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT' | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT'          |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_COMMAND_PREPROCESS_READ_BIT_NV'            | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'            |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV'           | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_COMMAND_PREPROCESS_BIT_NV'            |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT'        | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_CONDITIONAL_RENDERING_BIT_EXT'        |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV'            | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_SHADING_RATE_IMAGE_BIT_NV'            |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT'          | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'           |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT'  | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TRANSFORM_FEEDBACK_BIT_EXT'           |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT'   | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_DRAW_INDIRECT_BIT'                    |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'       | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR', or       |
-- |                                                    | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'      | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR' |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
-- | 'ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT'         | 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_FRAGMENT_DENSITY_PROCESS_BIT_EXT'     |
-- +----------------------------------------------------+-------------------------------------------------------------------------------------------------+
--
-- Supported access types
--
-- = See Also
--
-- 'AccessFlags'
newtype AccessFlagBits = AccessFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'ACCESS_INDIRECT_COMMAND_READ_BIT' specifies read access to indirect
-- command data read as part of an indirect drawing or dispatch command.
pattern ACCESS_INDIRECT_COMMAND_READ_BIT = AccessFlagBits 0x00000001
-- | 'ACCESS_INDEX_READ_BIT' specifies read access to an index buffer as part
-- of an indexed drawing command, bound by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer'.
pattern ACCESS_INDEX_READ_BIT = AccessFlagBits 0x00000002
-- | 'ACCESS_VERTEX_ATTRIBUTE_READ_BIT' specifies read access to a vertex
-- buffer as part of a drawing command, bound by
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers'.
pattern ACCESS_VERTEX_ATTRIBUTE_READ_BIT = AccessFlagBits 0x00000004
-- | 'ACCESS_UNIFORM_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer>.
pattern ACCESS_UNIFORM_READ_BIT = AccessFlagBits 0x00000008
-- | 'ACCESS_INPUT_ATTACHMENT_READ_BIT' specifies read access to an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass input attachment>
-- within a render pass during fragment shading.
pattern ACCESS_INPUT_ATTACHMENT_READ_BIT = AccessFlagBits 0x00000010
-- | 'ACCESS_SHADER_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-binding-table shader binding table>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern ACCESS_SHADER_READ_BIT = AccessFlagBits 0x00000020
-- | 'ACCESS_SHADER_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-physical-storage-buffer physical storage buffer>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer>,
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern ACCESS_SHADER_WRITE_BIT = AccessFlagBits 0x00000040
-- | 'ACCESS_COLOR_ATTACHMENT_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass color attachment>,
-- such as via
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blending blending>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-logicop logic operations>,
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
-- It does not include
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
pattern ACCESS_COLOR_ATTACHMENT_READ_BIT = AccessFlagBits 0x00000080
-- | 'ACCESS_COLOR_ATTACHMENT_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass color, resolve, or depth\/stencil resolve attachment>
-- during a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass render pass>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
pattern ACCESS_COLOR_ATTACHMENT_WRITE_BIT = AccessFlagBits 0x00000100
-- | 'ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load operations>.
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT = AccessFlagBits 0x00000200
-- | 'ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT' specifies write access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass depth\/stencil attachment>,
-- via
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-ds-state depth or stencil operations>
-- or via certain
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-load-store-ops subpass load and store operations>.
pattern ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT = AccessFlagBits 0x00000400
-- | 'ACCESS_TRANSFER_READ_BIT' specifies read access to an image or buffer
-- in a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy>
-- operation.
pattern ACCESS_TRANSFER_READ_BIT = AccessFlagBits 0x00000800
-- | 'ACCESS_TRANSFER_WRITE_BIT' specifies write access to an image or buffer
-- in a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#clears clear>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#copies copy>
-- operation.
pattern ACCESS_TRANSFER_WRITE_BIT = AccessFlagBits 0x00001000
-- | 'ACCESS_HOST_READ_BIT' specifies read access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory.
pattern ACCESS_HOST_READ_BIT = AccessFlagBits 0x00002000
-- | 'ACCESS_HOST_WRITE_BIT' specifies write access by a host operation.
-- Accesses of this type are not performed through a resource, but directly
-- on memory.
pattern ACCESS_HOST_WRITE_BIT = AccessFlagBits 0x00004000
-- | 'ACCESS_MEMORY_READ_BIT' specifies all read accesses. It is always valid
-- in any access mask, and is treated as equivalent to setting all @READ@
-- access flags that are valid where it is used.
pattern ACCESS_MEMORY_READ_BIT = AccessFlagBits 0x00008000
-- | 'ACCESS_MEMORY_WRITE_BIT' specifies all write accesses. It is always
-- valid in any access mask, and is treated as equivalent to setting all
-- @WRITE@ access flags that are valid where it is used.
pattern ACCESS_MEMORY_WRITE_BIT = AccessFlagBits 0x00010000
-- | 'ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV' specifies writes to the
-- 'Vulkan.Core10.Handles.Buffer' preprocess outputs in
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
pattern ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV = AccessFlagBits 0x00040000
-- | 'ACCESS_COMMAND_PREPROCESS_READ_BIT_NV' specifies reads from
-- 'Vulkan.Core10.Handles.Buffer' inputs to
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV'.
pattern ACCESS_COMMAND_PREPROCESS_READ_BIT_NV = AccessFlagBits 0x00020000
-- | 'ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT' specifies read access to a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>
-- during dynamic
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops fragment density map operations>
pattern ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT = AccessFlagBits 0x01000000
-- | 'ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV' specifies read access to a
-- shading rate image as part of a drawing command, as bound by
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV'.
pattern ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV = AccessFlagBits 0x00800000
-- | 'ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR' specifies write access to
-- an acceleration structure or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-scratch acceleration structure scratch buffer>
-- as part of a build command.
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR = AccessFlagBits 0x00400000
-- | 'ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR' specifies read access to an
-- acceleration structure as part of a trace or build command, or to an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-scratch acceleration structure scratch buffer>
-- as part of a build command.
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR = AccessFlagBits 0x00200000
-- | 'ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT' is similar to
-- 'ACCESS_COLOR_ATTACHMENT_READ_BIT', but also includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#framebuffer-blend-advanced advanced blend operations>.
pattern ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT = AccessFlagBits 0x00080000
-- | 'ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT' specifies read access to a
-- predicate as part of conditional rendering.
pattern ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT = AccessFlagBits 0x00100000
-- | 'ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT' specifies write access
-- to a transform feedback counter buffer which is written when
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT'
-- executes.
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT = AccessFlagBits 0x08000000
-- | 'ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT' specifies read access
-- to a transform feedback counter buffer which is read when
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT'
-- executes.
pattern ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT = AccessFlagBits 0x04000000
-- | 'ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT' specifies write access to a
-- transform feedback buffer made when transform feedback is active.
pattern ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT = AccessFlagBits 0x02000000

type AccessFlags = AccessFlagBits

instance Show AccessFlagBits where
  showsPrec p = \case
    ACCESS_INDIRECT_COMMAND_READ_BIT -> showString "ACCESS_INDIRECT_COMMAND_READ_BIT"
    ACCESS_INDEX_READ_BIT -> showString "ACCESS_INDEX_READ_BIT"
    ACCESS_VERTEX_ATTRIBUTE_READ_BIT -> showString "ACCESS_VERTEX_ATTRIBUTE_READ_BIT"
    ACCESS_UNIFORM_READ_BIT -> showString "ACCESS_UNIFORM_READ_BIT"
    ACCESS_INPUT_ATTACHMENT_READ_BIT -> showString "ACCESS_INPUT_ATTACHMENT_READ_BIT"
    ACCESS_SHADER_READ_BIT -> showString "ACCESS_SHADER_READ_BIT"
    ACCESS_SHADER_WRITE_BIT -> showString "ACCESS_SHADER_WRITE_BIT"
    ACCESS_COLOR_ATTACHMENT_READ_BIT -> showString "ACCESS_COLOR_ATTACHMENT_READ_BIT"
    ACCESS_COLOR_ATTACHMENT_WRITE_BIT -> showString "ACCESS_COLOR_ATTACHMENT_WRITE_BIT"
    ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT -> showString "ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT"
    ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT -> showString "ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT"
    ACCESS_TRANSFER_READ_BIT -> showString "ACCESS_TRANSFER_READ_BIT"
    ACCESS_TRANSFER_WRITE_BIT -> showString "ACCESS_TRANSFER_WRITE_BIT"
    ACCESS_HOST_READ_BIT -> showString "ACCESS_HOST_READ_BIT"
    ACCESS_HOST_WRITE_BIT -> showString "ACCESS_HOST_WRITE_BIT"
    ACCESS_MEMORY_READ_BIT -> showString "ACCESS_MEMORY_READ_BIT"
    ACCESS_MEMORY_WRITE_BIT -> showString "ACCESS_MEMORY_WRITE_BIT"
    ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV -> showString "ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV"
    ACCESS_COMMAND_PREPROCESS_READ_BIT_NV -> showString "ACCESS_COMMAND_PREPROCESS_READ_BIT_NV"
    ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT -> showString "ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT"
    ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV -> showString "ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV"
    ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR -> showString "ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR"
    ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR -> showString "ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR"
    ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT -> showString "ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT"
    ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT -> showString "ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT"
    ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT -> showString "ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT"
    ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT -> showString "ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT"
    ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT -> showString "ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT"
    AccessFlagBits x -> showParen (p >= 11) (showString "AccessFlagBits 0x" . showHex x)

instance Read AccessFlagBits where
  readPrec = parens (choose [("ACCESS_INDIRECT_COMMAND_READ_BIT", pure ACCESS_INDIRECT_COMMAND_READ_BIT)
                            , ("ACCESS_INDEX_READ_BIT", pure ACCESS_INDEX_READ_BIT)
                            , ("ACCESS_VERTEX_ATTRIBUTE_READ_BIT", pure ACCESS_VERTEX_ATTRIBUTE_READ_BIT)
                            , ("ACCESS_UNIFORM_READ_BIT", pure ACCESS_UNIFORM_READ_BIT)
                            , ("ACCESS_INPUT_ATTACHMENT_READ_BIT", pure ACCESS_INPUT_ATTACHMENT_READ_BIT)
                            , ("ACCESS_SHADER_READ_BIT", pure ACCESS_SHADER_READ_BIT)
                            , ("ACCESS_SHADER_WRITE_BIT", pure ACCESS_SHADER_WRITE_BIT)
                            , ("ACCESS_COLOR_ATTACHMENT_READ_BIT", pure ACCESS_COLOR_ATTACHMENT_READ_BIT)
                            , ("ACCESS_COLOR_ATTACHMENT_WRITE_BIT", pure ACCESS_COLOR_ATTACHMENT_WRITE_BIT)
                            , ("ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT", pure ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT)
                            , ("ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT", pure ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT)
                            , ("ACCESS_TRANSFER_READ_BIT", pure ACCESS_TRANSFER_READ_BIT)
                            , ("ACCESS_TRANSFER_WRITE_BIT", pure ACCESS_TRANSFER_WRITE_BIT)
                            , ("ACCESS_HOST_READ_BIT", pure ACCESS_HOST_READ_BIT)
                            , ("ACCESS_HOST_WRITE_BIT", pure ACCESS_HOST_WRITE_BIT)
                            , ("ACCESS_MEMORY_READ_BIT", pure ACCESS_MEMORY_READ_BIT)
                            , ("ACCESS_MEMORY_WRITE_BIT", pure ACCESS_MEMORY_WRITE_BIT)
                            , ("ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV", pure ACCESS_COMMAND_PREPROCESS_WRITE_BIT_NV)
                            , ("ACCESS_COMMAND_PREPROCESS_READ_BIT_NV", pure ACCESS_COMMAND_PREPROCESS_READ_BIT_NV)
                            , ("ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT", pure ACCESS_FRAGMENT_DENSITY_MAP_READ_BIT_EXT)
                            , ("ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV", pure ACCESS_SHADING_RATE_IMAGE_READ_BIT_NV)
                            , ("ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR", pure ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR)
                            , ("ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR", pure ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR)
                            , ("ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT", pure ACCESS_COLOR_ATTACHMENT_READ_NONCOHERENT_BIT_EXT)
                            , ("ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT", pure ACCESS_CONDITIONAL_RENDERING_READ_BIT_EXT)
                            , ("ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT", pure ACCESS_TRANSFORM_FEEDBACK_COUNTER_WRITE_BIT_EXT)
                            , ("ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT", pure ACCESS_TRANSFORM_FEEDBACK_COUNTER_READ_BIT_EXT)
                            , ("ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT", pure ACCESS_TRANSFORM_FEEDBACK_WRITE_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "AccessFlagBits")
                       v <- step readPrec
                       pure (AccessFlagBits v)))

